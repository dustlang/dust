// crates/dust_codegen/src/lib.rs
//
// Native codegen for DPL v0.1 (initial executable milestone)
//
// Goal:
// - Take DIR (Dust Intermediate Representation) and produce a native executable.
// - v0.1 executable subset implemented here:
//     * K-regime process named `main`
//     * body contains zero or more Effect(kind="emit", payload="<string literal>")
// - Q and Φ regimes are not codegen-enabled yet (must be rejected earlier or here).
//
// Backend: Cranelift -> object file -> system linker.

use anyhow::{anyhow, bail, Context, Result};
use cranelift_codegen::ir::{AbiParam, FuncRef, Signature, UserFuncName};
use cranelift_codegen::isa;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use dust_dir::{DirProgram, DirStmt};
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use target_lexicon::Triple;

pub fn generate() {
    // Legacy stub entrypoint (kept for compatibility with early scaffolding).
    // Prefer calling `build_executable(&dir, out_path)`.
}

/// Build a native executable from a DIR program.
/// `out_path` is the desired output executable path (extension will be adjusted on Windows if needed).
pub fn build_executable(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    let exe_path = normalize_exe_path(out_path);

    // Locate K::main in DIR
    let main = find_k_main(dir).context("DIR does not contain a codegen-capable K::main")?;

    // Validate supported statement subset
    let emit_strings = extract_emit_strings(&main.body)?;

    // Build object with Cranelift
    let obj_bytes = build_object_with_main(&emit_strings)?;

    // Write object to temp and link
    let obj_path = write_object_temp(&exe_path, &obj_bytes)?;
    link_executable(&obj_path, &exe_path)?;

    // Best effort: cleanup temp object
    let _ = fs::remove_file(&obj_path);

    Ok(exe_path)
}

fn normalize_exe_path(out_path: &Path) -> PathBuf {
    if cfg!(windows) {
        // If user didn't provide .exe, add it.
        if out_path.extension().is_none() {
            let mut p = out_path.to_path_buf();
            p.set_extension("exe");
            return p;
        }
    }
    out_path.to_path_buf()
}

fn find_k_main(dir: &DirProgram) -> Result<dust_dir::DirProc> {
    for forge in &dir.forges {
        for p in &forge.procs {
            if p.regime == "K" && p.name == "main" {
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
                let decoded = decode_string_literal(payload)
                    .with_context(|| format!("emit payload must be a string literal, got: {}", payload))?;
                out.push(decoded);
            }
            // For the executable milestone, we only support emit statements in main.
            other => bail!("unsupported statement in codegen v0.1 main: {:?}", other),
        }
    }

    Ok(out)
}

/// DIR stores string literals as indicating `format!("{:?}", s)`
/// which produces a quoted, escaped string (Rust debug string style).
/// Example payload: "\"Hello\\nWorld\""
fn decode_string_literal(payload: &str) -> Result<String> {
    // Must begin and end with double-quotes to be treated as a string literal.
    let p = payload.trim();
    if !(p.starts_with('"') && p.ends_with('"') && p.len() >= 2) {
        bail!("not a string literal");
    }

    // Minimal unescape compatible with Rust debug string output:
    // supports: \n \r \t \\ \"
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
            // Rust Debug may emit \u{...} for some chars; we do not support that in v0.1 codegen subset.
            _ => bail!("unsupported escape sequence: \\{}", esc),
        }
    }

    Ok(out)
}

fn build_object_with_main(emit_strings: &[String]) -> Result<Vec<u8>> {
    // Host target
    let triple = Triple::host();
    let isa = isa::lookup(triple.clone())
        .map_err(|e| anyhow!("failed to lookup ISA for {}: {}", triple, e))?
        .finish(cranelift_codegen::settings::Flags::new(cranelift_codegen::settings::builder()));

    let builder = ObjectBuilder::new(
        isa,
        "dust_module".to_string(),
        cranelift_module::default_libcall_names(),
    )
    .context("failed to create ObjectBuilder")?;

    let mut module = ObjectModule::new(builder);

    let ptr_ty = module.target_config().pointer_type();

    // Declare external `puts`:
    // int puts(const char*);
    let mut puts_sig = Signature::new(module.isa().default_call_conv());
    puts_sig.params.push(AbiParam::new(ptr_ty));
    puts_sig.returns.push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let puts_func = module
        .declare_function("puts", Linkage::Import, &puts_sig)
        .context("declare puts")?;

    // Declare main
    let mut main_sig = Signature::new(module.isa().default_call_conv());
    main_sig.returns.push(AbiParam::new(cranelift_codegen::ir::types::I32));

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

        let mut data_ctx = DataContext::new();
        let mut bytes = s.as_bytes().to_vec();
        bytes.push(0); // null terminator for C string
        data_ctx.define(bytes.into_boxed_slice());
        module
            .define_data(data_id, &data_ctx)
            .with_context(|| format!("define data {}", name))?;

        string_data_ids.push(data_id);
    }

    // Build function body
    let mut ctx = module.make_context();
    ctx.func.signature = main_sig;
    ctx.func.name = UserFuncName::user(0, main_func.as_u32());

    let mut fb_ctx = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
        let entry = b.create_block();
        b.switch_to_block(entry);
        b.seal_block(entry);

        // Import puts as a function reference
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

    module.finalize_definitions();

    // Emit object bytes
    let product = module.finish();
    let obj = product.object.write().context("write object")?;
    Ok(obj)
}

fn write_object_temp(exe_path: &Path, obj_bytes: &[u8]) -> Result<PathBuf> {
    let dir = exe_path
        .parent()
        .ok_or_else(|| anyhow!("output path has no parent directory"))?;
    fs::create_dir_all(dir).with_context(|| format!("create {}", dir.display()))?;

    let obj_path = if cfg!(windows) {
        dir.join(format!(
            "{}.obj",
            exe_path.file_name().and_then(OsStr::to_str).unwrap_or("a")
        ))
    } else {
        dir.join(format!(
            "{}.o",
            exe_path.file_name().and_then(OsStr::to_str).unwrap_or("a")
        ))
    };

    fs::write(&obj_path, obj_bytes).with_context(|| format!("write {}", obj_path.display()))?;
    Ok(obj_path)
}

fn link_executable(obj_path: &Path, exe_path: &Path) -> Result<()> {
    if cfg!(windows) {
        // Try link.exe first (MSVC), fallback to clang.
        if try_link_msvc(obj_path, exe_path)? {
            return Ok(());
        }
        if try_link_clang(obj_path, exe_path)? {
            return Ok(());
        }
        bail!("no suitable linker found on Windows (tried link.exe, clang)");
    } else if cfg!(target_os = "macos") {
        // macOS: clang is typically available; cc also works.
        if try_link_cc_like("cc", obj_path, exe_path)? {
            return Ok(());
        }
        if try_link_cc_like("clang", obj_path, exe_path)? {
            return Ok(());
        }
        bail!("no suitable linker found on macOS (tried cc, clang)");
    } else {
        // Linux and others
        if try_link_cc_like("cc", obj_path, exe_path)? {
            return Ok(());
        }
        if try_link_cc_like("clang", obj_path, exe_path)? {
            return Ok(());
        }
        bail!("no suitable linker found (tried cc, clang)");
    }
}

fn try_link_cc_like(tool: &str, obj_path: &Path, exe_path: &Path) -> Result<bool> {
    let status = Command::new(tool)
        .arg(obj_path)
        .arg("-o")
        .arg(exe_path)
        .status();

    match status {
        Ok(s) if s.success() => Ok(true),
        Ok(_) => Ok(false),
        Err(_) => Ok(false),
    }
}

fn try_link_clang(obj_path: &Path, exe_path: &Path) -> Result<bool> {
    // On Windows, clang can link via lld or MSVC depending on setup.
    try_link_cc_like("clang", obj_path, exe_path)
}

fn try_link_msvc(obj_path: &Path, exe_path: &Path) -> Result<bool> {
    // link.exe /nologo <obj> /OUT:<exe>
    let status = Command::new("link.exe")
        .arg("/nologo")
        .arg(obj_path)
        .arg(format!("/OUT:{}", exe_path.display()))
        .status();

    match status {
        Ok(s) if s.success() => Ok(true),
        Ok(_) => Ok(false),
        Err(_) => Ok(false),
    }
}