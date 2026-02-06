// crates/dust_codegen/src/lib.rs
//
// Native codegen for DPL v0.1 (executable milestone)
//
// What this enables:
// - `dust build <file.ds>` produces a native executable (ELF/Mach-O/PE)
// - `dust run <file.ds>` builds then runs it
//
// v0.1 executable subset implemented here:
// - A K-regime process named `main`
// - Its body contains zero or more `emit "<string>"` effects
//
// Backend:
// - Cranelift -> object file -> system linker (cc/clang/link.exe)
//
// NOTE: Q and Φ are not codegen-enabled here (they should be rejected earlier, but we also fail here).

use anyhow::{anyhow, bail, Context, Result};
use cranelift_codegen::ir::{AbiParam, FuncRef, Signature, UserFuncName};
use cranelift_codegen::isa;
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use dust_dir::{DirProgram, DirProc, DirStmt};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use target_lexicon::Triple;

// Kept for backward compatibility with earlier scaffolding.
pub fn generate() {
    // Prefer `build_executable(&dir, out_path)`
}

/// Build a native executable from a DIR program.
/// Returns the actual executable path (normalized for Windows .exe).
pub fn build_executable(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    let exe_path = normalize_exe_path(out_path);

    // Locate K::main in DIR
    let main = find_k_main(dir).context("DIR does not contain a codegen-capable K::main")?;

    // Validate supported subset and extract emit payloads
    let emit_strings = extract_emit_strings(&main.body)?;

    // Build object with Cranelift
    let obj_bytes = build_object_with_main(&emit_strings)?;

    // Write object next to output and link
    let obj_path = write_object_temp(&exe_path, &obj_bytes)?;
    link_executable(&obj_path, &exe_path)?;

    // Best-effort cleanup
    let _ = fs::remove_file(&obj_path);

    Ok(exe_path)
}

fn normalize_exe_path(out_path: &Path) -> PathBuf {
    if cfg!(windows) {
        if out_path.extension().is_none() {
            let mut p = out_path.to_path_buf();
            p.set_extension("exe");
            return p;
        }
    }
    out_path.to_path_buf()
}

fn find_k_main(dir: &DirProgram) -> Result<DirProc> {
    for forge in &dir.forges {
        for p in &forge.procs {
            match p.regime.as_str() {
                "K" => {}
                "Q" => continue,
                "Φ" => continue,
                other => bail!("unknown regime in DIR: {}", other),
            }
            if p.name == "main" {
                return Ok(p.clone());
            }
        }
    }
    bail!("no K::main found")
}

fn extract_emit_strings(stmts: &[DirStmt]) -> Result<Vec<String>> {
    let mut out = Vec::new();

    for s in stmts {
        match s {
            DirStmt::Effect { kind, payload } => {
                if kind != "emit" {
                    bail!("unsupported effect kind in codegen v0.1: {}", kind);
                }
                let decoded = decode_string_literal(payload).with_context(|| {
                    format!("emit payload must be a string literal, got: {}", payload)
                })?;
                out.push(decoded);
            }
            // For the executable milestone, we support only emit statements in main.
            other => bail!("unsupported statement in v0.1 codegen main: {:?}", other),
        }
    }

    Ok(out)
}

/// `dust_semantics::lower_to_dir` serializes string literals as `format!("{:?}", s)`,
/// e.g. "\"Hello\\nWorld\"" (quoted + escaped).
fn decode_string_literal(payload: &str) -> Result<String> {
    let p = payload.trim();
    if !(p.starts_with('"') && p.ends_with('"') && p.len() >= 2) {
        bail!("not a string literal");
    }

    let inner = &p[1..p.len() - 1];
    let mut out = String::new();
    let mut chars = inner.chars();

    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        let esc = chars.next().ok_or_else(|| anyhow!("dangling escape"))?;
        match esc {
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            '\\' => out.push('\\'),
            '"' => out.push('"'),
            _ => bail!("unsupported escape sequence: \\{}", esc),
        }
    }

    Ok(out)
}

fn build_object_with_main(emit_strings: &[String]) -> Result<Vec<u8>> {
    // Host target
    let triple = Triple::host();

    // Cranelift 0.110+: finish() returns Result, so we must `?`.
    let flags = settings::Flags::new(settings::builder());
    let isa = isa::lookup(triple.clone())
        .map_err(|e| anyhow!("failed to lookup ISA for {}: {}", triple, e))?
        .finish(flags)
        .map_err(|e| anyhow!("failed to finish ISA for {}: {}", triple, e))?;

    let builder = ObjectBuilder::new(
        isa,
        "dust_module".to_string(),
        cranelift_module::default_libcall_names(),
    )
    .context("failed to create ObjectBuilder")?;

    let mut module = ObjectModule::new(builder);
    let ptr_ty = module.target_config().pointer_type();

    // Declare external C `puts`: int puts(const char*);
    let mut puts_sig = Signature::new(module.isa().default_call_conv());
    puts_sig.params.push(AbiParam::new(ptr_ty));
    puts_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let puts_func = module
        .declare_function("puts", Linkage::Import, &puts_sig)
        .context("declare puts")?;

    // Declare `main`: int main(void)
    let mut main_sig = Signature::new(module.isa().default_call_conv());
    main_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let main_func = module
        .declare_function("main", Linkage::Export, &main_sig)
        .context("declare main")?;

    // Define string data objects
    let mut string_data_ids = Vec::new();
    for (i, s) in emit_strings.iter().enumerate() {
        let name = format!("__dust_str_{}", i);
        let data_id = module
            .declare_data(&name, Linkage::Local, true, false)
            .with_context(|| format!("declare data {}", name))?;

        let mut data_desc = DataDescription::new();
        let mut bytes = s.as_bytes().to_vec();
        bytes.push(0); // null terminator
        data_desc.define(bytes.into_boxed_slice());

        module
            .define_data(data_id, &data_desc)
            .with_context(|| format!("define data {}", name))?;

        string_data_ids.push(data_id);
    }

    // Build main function body
    let mut ctx = module.make_context();
    ctx.func.signature = main_sig;
    ctx.func.name = UserFuncName::user(0, main_func.as_u32());

    let mut fb_ctx = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
        let entry = b.create_block();
        b.switch_to_block(entry);
        b.seal_block(entry);

        let puts_ref: FuncRef = module.declare_func_in_func(puts_func, b.func);

        for data_id in string_data_ids {
            let gv = module.declare_data_in_func(data_id, b.func);
            let addr = b.ins().global_value(ptr_ty, gv);
            b.ins().call(puts_ref, &[addr]);
        }

        let zero = b.ins().iconst(cranelift_codegen::ir::types::I32, 0);
        b.ins().return_(&[zero]);

        b.finalize();
    }

    module
        .define_function(main_func, &mut ctx)
        .context("define main")?;

    module.clear_context(&mut ctx);
    module.finalize_definitions();

    let product = module.finish();
    let obj = product.object;
    Ok(obj.write().context("emit object")?)
}

fn write_object_temp(exe_path: &Path, obj_bytes: &[u8]) -> Result<PathBuf> {
    let mut obj_path = exe_path.to_path_buf();
    obj_path.set_extension("o");
    fs::write(&obj_path, obj_bytes).with_context(|| format!("write object {:?}", obj_path))?;
    Ok(obj_path)
}

fn link_executable(obj_path: &Path, exe_path: &Path) -> Result<()> {
    if cfg!(windows) {
        link_windows(obj_path, exe_path)
    } else if cfg!(target_os = "macos") {
        link_unix(obj_path, exe_path, true)
    } else {
        link_unix(obj_path, exe_path, false)
    }
}

fn link_unix(obj_path: &Path, exe_path: &Path, is_macos: bool) -> Result<()> {
    let cc = std::env::var_os("CC").unwrap_or_else(|| OsStr::new("cc").to_os_string());
    let mut cmd = Command::new(cc);

    cmd.arg(obj_path).arg("-o").arg(exe_path);

    if is_macos {
        // macOS uses different defaults; keep it minimal.
    } else {
        // Linux/BSD: often need libc only (cc handles it).
    }

    let out = cmd.output().context("invoke system linker")?;
    if !out.status.success() {
        bail!(
            "link failed: {}\nstdout:\n{}\nstderr:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
    }

    Ok(())
}

fn link_windows(obj_path: &Path, exe_path: &Path) -> Result<()> {
    // Prefer clang-cl if available, else fall back to clang/cc.
    // This keeps CI workable on typical Windows runners.
    let clang = std::env::var_os("CC").unwrap_or_else(|| OsStr::new("clang").to_os_string());
    let mut cmd = Command::new(clang);

    cmd.arg(obj_path).arg("-o").arg(exe_path);

    let out = cmd.output().context("invoke system linker")?;
    if !out.status.success() {
        bail!(
            "link failed: {}\nstdout:\n{}\nstderr:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
    }

    Ok(())
}