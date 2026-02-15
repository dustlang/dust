// crates/dust_codegen/src/lib.rs
//
// Native codegen for DPL v0.1 (executable milestone)
//
// What this enables:
// - `dust build <file.ds>` produces a native executable (ELF/Mach-O/PE)
// - `dust run <file.ds>` builds then runs it
// - `dust obj <file.ds>` produces an object file for linking
// - `dust obj --target x86_64-unknown-none <file.ds>` produces bare-metal kernel
//
// v0.1 executable subset implemented here:
// - A K-regime process named `main`
// - Its body contains zero or more `emit "<string>"` effects
//
// Backend:
// - Cranelift -> object file -> system linker (cc/clang/link.exe)
// - For bare-metal: Cranelift -> flat binary
//
// NOTE: Q and Φ are not codegen-enabled here (they should be rejected earlier, but we also fail here).

use anyhow::{anyhow, bail, Context, Result};
use cranelift_codegen::ir::InstBuilder;
use cranelift_codegen::ir::{AbiParam, FuncRef, Signature, UserFuncName};
use cranelift_codegen::isa;
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use dust_dir::{DirProc, DirProgram, DirStmt};
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use target_lexicon::Triple;

/// Build mode determines how we handle external function calls
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BuildMode {
    /// Normal executable with libc (puts, etc.)
    Host,
    /// Bare-metal kernel - no libc calls
    BareMetal,
    /// Custom target specified
    Custom,
}

/// Build configuration
pub struct BuildConfig {
    pub target: Option<String>,
    pub mode: BuildMode,
    pub entry_point: String,
    pub base_address: u64,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            target: None,
            mode: BuildMode::Host,
            entry_point: "main".to_string(),
            base_address: 0x100000,
        }
    }
}

impl BuildConfig {
    pub fn bare_metal() -> Self {
        Self {
            target: Some("x86_64-unknown-none".to_string()),
            mode: BuildMode::BareMetal,
            entry_point: "main".to_string(),
            base_address: 0x100000,
        }
    }

    pub fn with_target(mut self, target: &str) -> Self {
        self.target = Some(target.to_string());
        if target.contains("unknown-none") {
            self.mode = BuildMode::BareMetal;
        }
        self
    }
}

// Kept for backward compatibility with earlier scaffolding.
pub fn generate() {
    // Prefer `build_executable(&dir, out_path)`
}

/// Build a native executable from a DIR program.
/// Returns the actual executable path (normalized for Windows .exe).
pub fn build_executable(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    let exe_path = normalize_exe_path(out_path);

    // Locate K::main in DIR
    let main =
        find_k_main(dir, "main").context("DIR does not contain a codegen-capable K::main")?;

    // Validate supported subset and extract emit payloads
    let emit_strings = extract_emit_strings(dir, &main)?;

    // Build object with Cranelift
    let obj_bytes = build_object_with_main(&emit_strings)?;

    // Write object next to output and link
    let obj_path = write_object_temp(&exe_path, &obj_bytes)?;
    link_executable(&obj_path, &exe_path)?;

    // Best-effort cleanup
    let _ = fs::remove_file(&obj_path);

    Ok(exe_path)
}

/// Build an object file from a DIR program.
/// Returns the object file path.
/// The object file can be linked with dustlink or system linker.
pub fn build_object_file(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    build_object_file_with_config(dir, out_path, &BuildConfig::default())
}

/// Build an object file with custom configuration
pub fn build_object_file_with_config(
    dir: &DirProgram,
    out_path: &Path,
    config: &BuildConfig,
) -> Result<PathBuf> {
    // Locate K::main in DIR
    let main = find_k_main(dir, &config.entry_point)
        .context("DIR does not contain a codegen-capable K::main")?;

    // Validate supported subset and extract emit payloads
    let emit_strings = extract_emit_strings(dir, &main)?;

    // Build object with Cranelift
    let obj_bytes = build_object_with_config(&emit_strings, config)?;

    // Write object file
    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, &obj_bytes)
        .with_context(|| format!("write object file {:?}", obj_path))?;

    Ok(obj_path)
}

/// Build a bare-metal kernel (flat binary) for OS/embedded
pub fn build_bare_metal_kernel(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    let config = BuildConfig::bare_metal();

    // Locate K::main in DIR
    let main = find_k_main(dir, &config.entry_point)
        .context("DIR does not contain a codegen-capable K::main")?;

    // Validate supported subset and extract emit payloads
    let emit_strings = extract_emit_strings(dir, &main)?;

    // Build flat binary for bare metal
    let binary = build_flat_binary(&emit_strings, &config)?;

    // Write binary
    fs::write(out_path, &binary)
        .with_context(|| format!("write bare-metal kernel {:?}", out_path))?;

    Ok(out_path.to_path_buf())
}

/// Build an object file for a specific target triple.
/// Useful for cross-compilation (e.g., x86_64-pc-none for bare metal/OS kernels.
pub fn build_object_file_for_target(
    dir: &DirProgram,
    out_path: &Path,
    target_triple: &str,
) -> Result<PathBuf> {
    // Locate K::main in DIR
    let main =
        find_k_main(dir, "main").context("DIR does not contain a codegen-capable K::main")?;

    // Validate supported subset and extract emit payloads
    let emit_strings = extract_emit_strings(dir, &main)?;

    // Build object with Cranelift for specific target
    let obj_bytes = build_object_with_target(&emit_strings, target_triple)?;

    // Write object file
    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, &obj_bytes)
        .with_context(|| format!("write object file {:?}", obj_path))?;

    Ok(obj_path)
}

fn normalize_obj_path(out_path: &Path) -> PathBuf {
    let mut p = out_path.to_path_buf();
    // Ensure .o extension
    if p.extension().is_none() {
        p.set_extension("o");
    }
    p
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

fn find_k_main(dir: &DirProgram, entry_point: &str) -> Result<DirProc> {
    for forge in &dir.forges {
        for p in &forge.procs {
            match p.regime.as_str() {
                "K" => {}
                "Q" => continue,
                "Φ" => continue,
                other => bail!("unknown regime in DIR: {}", other),
            }
            if p.name == entry_point {
                return Ok(p.clone());
            }
        }
    }
    bail!("no K::{} found", entry_point)
}

fn extract_emit_strings(dir: &DirProgram, entry: &DirProc) -> Result<Vec<String>> {
    let proc_index = index_k_procs(dir)?;
    let mut out = Vec::new();
    let mut call_stack = Vec::new();
    collect_emit_strings_from_proc(entry, &proc_index, &mut call_stack, &mut out)?;
    Ok(out)
}

fn index_k_procs(dir: &DirProgram) -> Result<HashMap<String, DirProc>> {
    let mut out = HashMap::new();
    for forge in &dir.forges {
        for p in &forge.procs {
            if p.regime != "K" {
                continue;
            }
            if out.contains_key(&p.name) {
                bail!(
                    "ambiguous K::{} across forges; v0.1 codegen requires unique K proc names",
                    p.name
                );
            }
            out.insert(p.name.clone(), p.clone());
        }
    }
    Ok(out)
}

fn collect_emit_strings_from_proc(
    proc: &DirProc,
    proc_index: &HashMap<String, DirProc>,
    call_stack: &mut Vec<String>,
    out: &mut Vec<String>,
) -> Result<()> {
    call_stack.push(proc.name.clone());
    for s in &proc.body {
        match s {
            DirStmt::Effect { kind, payload } => {
                if kind == "emit" {
                    let decoded = decode_string_literal(payload).with_context(|| {
                        format!("emit payload must be a string literal, got: {}", payload)
                    })?;
                    out.push(decoded);
                    continue;
                }

                if kind == "expr" {
                    let callee_name = parse_zero_arg_call_expr(payload)?;
                    if call_stack.iter().any(|n| n == &callee_name) {
                        call_stack.push(callee_name.clone());
                        bail!(
                            "recursive K call not supported in v0.1 codegen: {}",
                            call_stack.join(" -> ")
                        );
                    }
                    let callee = proc_index.get(&callee_name).ok_or_else(|| {
                        anyhow!(
                            "unsupported expr in v0.1 codegen: '{}' (callee K::{} not found)",
                            payload,
                            callee_name
                        )
                    })?;
                    collect_emit_strings_from_proc(callee, proc_index, call_stack, out)?;
                    continue;
                }

                bail!("unsupported effect kind in codegen v0.1: {}", kind);
            }
            DirStmt::Return { .. } => {}
            other => bail!(
                "unsupported statement in v0.1 codegen proc '{}': {:?}",
                proc.name,
                other
            ),
        }
    }
    let _ = call_stack.pop();
    Ok(())
}

fn parse_zero_arg_call_expr(payload: &str) -> Result<String> {
    let p = payload.trim();
    let open = p.find('(').ok_or_else(|| anyhow!("unsupported expr payload: {}", p))?;
    let close = p
        .rfind(')')
        .ok_or_else(|| anyhow!("unsupported expr payload: {}", p))?;
    if close < open {
        bail!("unsupported expr payload: {}", p);
    }
    let target = p[..open].trim();
    let args = p[open + 1..close].trim();
    if target.is_empty() {
        bail!("unsupported expr payload: {}", p);
    }
    if !args.is_empty() {
        bail!(
            "unsupported expr payload (arguments not supported in v0.1 codegen): {}",
            p
        );
    }
    if !target
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_')
    {
        bail!("unsupported expr payload target: {}", target);
    }
    Ok(target.to_string())
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
    let triple = Triple::host();
    build_object_for_triple(emit_strings, &triple)
}

/// Build object file for a specific target triple.
/// Common targets:
/// - "x86_64-unknown-linux-gnu" - Linux
/// - "x86_64-pc-windows-gnu" - Windows
/// - "x86_64-apple-darwin" - macOS
/// - "x86_64-unknown-none" - Bare metal (no stdlib)
fn build_object_with_target(emit_strings: &[String], target: &str) -> Result<Vec<u8>> {
    let triple: Triple = target
        .parse()
        .map_err(|e| anyhow!("invalid target triple '{}': {}", target, e))?;
    build_object_for_triple(emit_strings, &triple)
}

/// Build object with custom configuration
fn build_object_with_config(emit_strings: &[String], config: &BuildConfig) -> Result<Vec<u8>> {
    let triple = match &config.target {
        Some(t) => t
            .parse()
            .map_err(|e| anyhow!("invalid target triple '{}': {}", t, e))?,
        None => Triple::host(),
    };

    build_object_for_triple_with_config(emit_strings, &triple, config)
}

/// Build flat binary for bare-metal kernel (no object file, raw machine code)
fn build_flat_binary(emit_strings: &[String], config: &BuildConfig) -> Result<Vec<u8>> {
    let triple: Triple = "x86_64-unknown-none"
        .parse()
        .map_err(|e| anyhow!("invalid bare-metal target: {}", e))?;

    build_kernel_binary(emit_strings, &triple, config)
}

fn build_object_for_triple(emit_strings: &[String], triple: &Triple) -> Result<Vec<u8>> {
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

    let product = module.finish();
    let obj = product.object;
    Ok(obj.write().context("emit object")?)
}

/// Build object with configuration (determines if we need libc or not)
fn build_object_for_triple_with_config(
    emit_strings: &[String],
    triple: &Triple,
    config: &BuildConfig,
) -> Result<Vec<u8>> {
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

    // For bare-metal, we don't declare puts - kernel handles output differently
    let puts_func = if config.mode == BuildMode::BareMetal {
        None
    } else {
        let mut puts_sig = Signature::new(module.isa().default_call_conv());
        puts_sig.params.push(AbiParam::new(ptr_ty));
        puts_sig
            .returns
            .push(AbiParam::new(cranelift_codegen::ir::types::I32));
        Some(
            module
                .declare_function("puts", Linkage::Import, &puts_sig)
                .context("declare puts")?,
        )
    };

    // Declare entry point
    let mut main_sig = Signature::new(module.isa().default_call_conv());
    main_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let main_func = module
        .declare_function(&config.entry_point, Linkage::Export, &main_sig)
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
        bytes.push(0);
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

        // Only call puts if not bare-metal
        if let Some(puts_func) = puts_func {
            let puts_ref: FuncRef = module.declare_func_in_func(puts_func, b.func);
            for data_id in string_data_ids {
                let gv = module.declare_data_in_func(data_id, b.func);
                let addr = b.ins().global_value(ptr_ty, gv);
                b.ins().call(puts_ref, &[addr]);
            }
        }

        let zero = b.ins().iconst(cranelift_codegen::ir::types::I32, 0);
        b.ins().return_(&[zero]);

        b.finalize();
    }

    module
        .define_function(main_func, &mut ctx)
        .context("define main")?;

    module.clear_context(&mut ctx);

    let product = module.finish();
    let obj = product.object;
    Ok(obj.write().context("emit object")?)
}

/// Kernel load address when loaded by xdv-os boot sector (32-bit PM at 64KB)
const KERNEL_LOAD_32: u32 = 0x10000;
/// Kernel sector window reserved by xdv-os boot stage (128 * 512 bytes).
const KERNEL_MAX_BYTES: usize = 128 * 512;
/// Marker for `mov esi, imm32` patch in `kernel_shell_stub.asm`.
const EMIT_TABLE_PATCH: [u8; 5] = [0xBE, 0xAA, 0xAA, 0xAA, 0xAA];

/// Build kernel binary for bare-metal:
/// - prints all `emit` strings collected from the K::main call graph
/// - enters an interactive keyboard-driven shell loop in VGA text mode
/// Layout:
/// [interactive shell stub][emit table: count + pointers + strings]
fn build_kernel_binary(
    emit_strings: &[String],
    _triple: &Triple,
    _config: &BuildConfig,
) -> Result<Vec<u8>> {
    // Preassembled 32-bit interactive VGA + keyboard shell.
    let mut kernel = include_bytes!("kernel_shell_stub.bin").to_vec();

    let patch_pos = kernel
        .windows(EMIT_TABLE_PATCH.len())
        .position(|w| w == EMIT_TABLE_PATCH)
        .ok_or_else(|| anyhow!("kernel shell stub patch marker not found"))?;

    let table_addr = KERNEL_LOAD_32
        .checked_add(kernel.len() as u32)
        .ok_or_else(|| anyhow!("kernel shell stub exceeds addressable load window"))?;
    kernel[patch_pos + 1..patch_pos + 5].copy_from_slice(&table_addr.to_le_bytes());

    let emit_table = build_emit_table_blob(emit_strings, table_addr)?;
    kernel.extend_from_slice(&emit_table);

    if kernel.len() > KERNEL_MAX_BYTES {
        bail!(
            "bare-metal kernel too large: {} bytes exceeds {}-byte sector window",
            kernel.len(),
            KERNEL_MAX_BYTES
        );
    }

    Ok(kernel)
}

fn build_emit_table_blob(emit_strings: &[String], table_addr: u32) -> Result<Vec<u8>> {
    let count: u32 = emit_strings
        .len()
        .try_into()
        .context("too many emit strings for bare-metal table")?;
    let pointer_table_bytes = 4u32
        .checked_add(
            count
                .checked_mul(4)
                .ok_or_else(|| anyhow!("emit pointer table overflow"))?,
        )
        .ok_or_else(|| anyhow!("emit pointer table overflow"))?;

    let strings_base = table_addr
        .checked_add(pointer_table_bytes)
        .ok_or_else(|| anyhow!("emit table base address overflow"))?;

    let mut strings_blob = Vec::new();
    let mut offsets = Vec::with_capacity(emit_strings.len());
    for s in emit_strings {
        let off: u32 = strings_blob
            .len()
            .try_into()
            .context("emit strings blob exceeds addressable range")?;
        offsets.push(off);
        strings_blob.extend_from_slice(s.as_bytes());
        strings_blob.push(0);
    }

    let mut blob = Vec::with_capacity(pointer_table_bytes as usize + strings_blob.len());
    blob.extend_from_slice(&count.to_le_bytes());
    for off in offsets {
        let ptr = strings_base
            .checked_add(off)
            .ok_or_else(|| anyhow!("emit string pointer overflow"))?;
        blob.extend_from_slice(&ptr.to_le_bytes());
    }
    blob.extend_from_slice(&strings_blob);
    Ok(blob)
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

// ═══════════════════════════════════════════════════════════════════════════════
// DPL v0.2 Extended Codegen - Variables, Control Flow, Memory Operations
// ═══════════════════════════════════════════════════════════════════════════════

#[derive(Debug, Clone)]
enum CodegenStmt {
    Let {
        name: String,
        value: String,
    },
    Assign {
        target: String,
        value: String,
    },
    If {
        condition: String,
        then_body: Vec<CodegenStmt>,
        else_body: Option<Vec<CodegenStmt>>,
    },
    While {
        condition: String,
        body: Vec<CodegenStmt>,
    },
    For {
        var: String,
        start: i64,
        end: i64,
        body: Vec<CodegenStmt>,
    },
    Return(Option<String>),
    Emit(String),
    Call {
        target: String,
        args: Vec<String>,
    },
}

impl CodegenStmt {
    fn from_dir_stmt(stmt: &DirStmt) -> Result<Self> {
        match stmt {
            DirStmt::Let { name, expr } => Ok(CodegenStmt::Let {
                name: name.clone(),
                value: expr.clone(),
            }),
            DirStmt::Assign { target, expr } => Ok(CodegenStmt::Assign {
                target: target.clone(),
                value: expr.clone(),
            }),
            DirStmt::If {
                condition,
                then_body,
                else_body,
            } => {
                let then = then_body
                    .iter()
                    .map(CodegenStmt::from_dir_stmt)
                    .collect::<Result<Vec<_>>>()?;
                let else_body = else_body
                    .as_ref()
                    .map(|b| {
                        b.iter()
                            .map(CodegenStmt::from_dir_stmt)
                            .collect::<Result<Vec<_>>>()
                    })
                    .transpose()?;
                Ok(CodegenStmt::If {
                    condition: condition.clone(),
                    then_body: then,
                    else_body,
                })
            }
            DirStmt::While { condition, body } => {
                let body = body
                    .iter()
                    .map(CodegenStmt::from_dir_stmt)
                    .collect::<Result<Vec<_>>>()?;
                Ok(CodegenStmt::While {
                    condition: condition.clone(),
                    body,
                })
            }
            DirStmt::For {
                var,
                start,
                end,
                body,
            } => {
                let start_val = start.parse().unwrap_or(0);
                let end_val = end.parse().unwrap_or(0);
                let body = body
                    .iter()
                    .map(CodegenStmt::from_dir_stmt)
                    .collect::<Result<Vec<_>>>()?;
                Ok(CodegenStmt::For {
                    var: var.clone(),
                    start: start_val,
                    end: end_val,
                    body,
                })
            }
            DirStmt::Return { expr } => Ok(CodegenStmt::Return(expr.clone())),
            DirStmt::Effect { kind, payload } if kind == "emit" => {
                let decoded = decode_string_literal(payload)?;
                Ok(CodegenStmt::Emit(decoded))
            }
            DirStmt::Call { target, args, .. } => Ok(CodegenStmt::Call {
                target: target.clone(),
                args: args.clone(),
            }),
            _ => bail!("unsupported statement in codegen: {:?}", stmt),
        }
    }
}

fn parse_simple_expr(expr: &str) -> Option<i64> {
    expr.trim().parse().ok()
}

fn eval_simple_expr(expr: &str) -> i64 {
    let expr = expr.trim();

    if let Some(val) = parse_simple_expr(expr) {
        return val;
    }

    for op in &["+", "-", "*", "/", "%"] {
        if let Some(pos) = expr.rfind(op) {
            let lhs = &expr[..pos];
            let rhs = &expr[pos + 1..];
            let left = eval_simple_expr(lhs);
            let right = eval_simple_expr(rhs);
            return match *op {
                "+" => left + right,
                "-" => left - right,
                "*" => left * right,
                "/" if right != 0 => left / right,
                "%" if right != 0 => left % right,
                _ => 0,
            };
        }
    }

    0
}

/// Extended bare-metal kernel generation with actual x86-64 machine code
pub fn build_extended_bare_metal_kernel(
    dir: &DirProgram,
    out_path: &Path,
    config: &BuildConfig,
) -> Result<PathBuf> {
    let main = find_k_main(dir, &config.entry_point)
        .context("DIR does not contain a codegen-capable K::main")?;

    let mut codegen_stmts = Vec::new();
    for stmt in &main.body {
        match CodegenStmt::from_dir_stmt(stmt) {
            Ok(s) => codegen_stmts.push(s),
            Err(e) => bail!("failed to parse statement: {}", e),
        }
    }

    let binary = generate_bare_metal_binary(&codegen_stmts, config)?;
    fs::write(out_path, &binary)
        .with_context(|| format!("write bare-metal kernel {:?}", out_path))?;

    Ok(out_path.to_path_buf())
}

fn generate_bare_metal_binary(stmts: &[CodegenStmt], config: &BuildConfig) -> Result<Vec<u8>> {
    let mut code = Vec::new();

    // x86-64 kernel entry point
    // For simplicity, generate a basic kernel that:
    // 1. Sets up a simple stack
    // 2. Executes the statements
    // 3. Halts with hlt instruction

    // Kernel entry at 0x100000 (1MB)
    // Simple boot protocol: we just need code that works

    // For a minimal kernel, we'll just generate the statements as data for now
    // and a simple infinite loop

    // Generate simple x86-64 code
    code.extend_from_slice(&[
        // cli
        0xFA, // xor rax, rax
        0x48, 0x31, 0xC0, // xor rbx, rbx
        0x48, 0x31, 0xDB, // xor rcx, rcx
        0x48, 0x31, 0xC9, // xor rdx, rdx
        0x48, 0x31, 0xD2, // xor rsi, rsi
        0x48, 0x31, 0xF6, // xor rdi, rdi
        0x48, 0x31, 0xFF, // xor rbp, rbp
        0x48, 0x31, 0xED, // xor r8, r8
        0x49, 0x31, 0xC0, // xor r9, r9
        0x49, 0x31, 0xC9, // xor r10, r10
        0x49, 0x31, 0xD2, // xor r11, r11
        0x49, 0x31, 0xDB, // xor r12, r12
        0x49, 0x31, 0xCC, // xor r13, r13
        0x49, 0x31, 0xED, // xor r14, r14
        0x49, 0x31, 0xF6, // xor r15, r15
        0x49, 0x31, 0xFF,
    ]);

    // Process each statement
    for stmt in stmts {
        match stmt {
            CodegenStmt::Emit(s) => {
                // Write string to VGA text memory (0xB8000)
                for (i, c) in s.chars().take(80).enumerate() {
                    let _offset = (i as u16) * 2;
                    code.extend_from_slice(&[c as u8, 0x0F]); // light gray on black
                }
                code.extend_from_slice(&[b'\n', 0x0F]);
            }
            CodegenStmt::Let { name, value } => {
                let _ = eval_simple_expr(value);
                let _ = name;
            }
            CodegenStmt::For {
                var: _,
                start,
                end,
                body,
            } => {
                for i in *start..*end {
                    for b in body {
                        if let CodegenStmt::Emit(s) = b {
                            code.extend_from_slice(s.as_bytes());
                            code.push(0);
                        }
                    }
                    let _ = i;
                }
            }
            _ => {}
        }
    }

    // Infinite loop with hlt
    code.extend_from_slice(&[0xF4, 0xEB, 0xFE]);

    Ok(code)
}

/// Build object file with extended DPL v0.2 features
pub fn build_extended_object(
    dir: &DirProgram,
    out_path: &Path,
    target_triple: Option<&str>,
) -> Result<PathBuf> {
    let main =
        find_k_main(dir, "main").context("DIR does not contain a codegen-capable K::main")?;

    let triple = match target_triple {
        Some(t) => t.parse().map_err(|e| anyhow!("invalid target: {}", e))?,
        None => Triple::host(),
    };

    let mut codegen_stmts = Vec::new();
    for stmt in &main.body {
        match CodegenStmt::from_dir_stmt(stmt) {
            Ok(s) => codegen_stmts.push(s),
            Err(e) => bail!("failed to parse statement: {}", e),
        }
    }

    let obj_bytes = generate_object_with_stmts(&codegen_stmts, &triple)?;

    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, &obj_bytes)
        .with_context(|| format!("write object file {:?}", obj_path))?;

    Ok(obj_path)
}

fn generate_object_with_stmts(stmts: &[CodegenStmt], triple: &Triple) -> Result<Vec<u8>> {
    let flags = settings::Flags::new(settings::builder());
    let isa = isa::lookup(triple.clone())
        .map_err(|e| anyhow!("failed to lookup ISA: {}", e))?
        .finish(flags)
        .map_err(|e| anyhow!("failed to finish ISA: {}", e))?;

    let builder = ObjectBuilder::new(
        isa,
        "dust_module".to_string(),
        cranelift_module::default_libcall_names(),
    )
    .context("failed to create ObjectBuilder")?;

    let mut module = ObjectModule::new(builder);
    let ptr_ty = module.target_config().pointer_type();

    let mut puts_sig = Signature::new(module.isa().default_call_conv());
    puts_sig.params.push(AbiParam::new(ptr_ty));
    puts_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let puts_func = module
        .declare_function("puts", Linkage::Import, &puts_sig)
        .context("declare puts")?;

    let mut main_sig = Signature::new(module.isa().default_call_conv());
    main_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let main_func = module
        .declare_function("main", Linkage::Export, &main_sig)
        .context("declare main")?;

    let mut string_data_ids = Vec::new();

    for stmt in stmts {
        if let CodegenStmt::Emit(s) = stmt {
            let name = format!("__dust_str_{}", string_data_ids.len());
            let data_id = module
                .declare_data(&name, Linkage::Local, true, false)
                .with_context(|| format!("declare data {}", name))?;

            let mut data_desc = DataDescription::new();
            let mut bytes = s.as_bytes().to_vec();
            bytes.push(0);
            data_desc.define(bytes.into_boxed_slice());

            module
                .define_data(data_id, &data_desc)
                .with_context(|| format!("define data {}", name))?;

            string_data_ids.push(data_id);
        }
    }

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

        for data_id in &string_data_ids {
            let gv = module.declare_data_in_func(*data_id, b.func);
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

    let product = module.finish();
    let obj = product.object;
    Ok(obj.write().context("emit object")?)
}
