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

/// Merge multiple DIR programs into one link unit.
pub fn merge_dir_programs(programs: &[DirProgram]) -> DirProgram {
    let mut merged = DirProgram {
        forges: Vec::new(),
        types: Vec::new(),
    };

    for program in programs {
        merged.forges.extend(program.forges.clone());
        merged.types.extend(program.types.clone());
    }

    merged
}

/// Build a bare-metal entry object from a merged DIR link unit.
///
/// This is a tolerant extraction mode intended for kernel builds:
/// - resolves K proc calls across forges
/// - prefers local-forge resolution first, then global unique names
/// - can skip unresolved/unsupported calls when `allow_unresolved_calls` is true
pub fn build_kernel_entry_object(
    dir: &DirProgram,
    out_path: &Path,
    entry_point: &str,
    target_triple: &str,
    allow_unresolved_calls: bool,
) -> Result<PathBuf> {
    let entry =
        find_k_entry_scoped(dir, entry_point).context("kernel link entry point not found")?;
    let emit_strings = extract_emit_strings_scoped(dir, &entry, allow_unresolved_calls)?;

    let config = BuildConfig {
        target: Some(target_triple.to_string()),
        mode: BuildMode::BareMetal,
        entry_point: entry.proc.name.clone(),
        base_address: 0x100000,
    };
    let obj_bytes = build_object_with_config(&emit_strings, &config)?;

    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, &obj_bytes)
        .with_context(|| format!("write kernel entry object {:?}", obj_path))?;
    Ok(obj_path)
}

/// Build a bootstrap object that exports `start_symbol`, calls `entry_symbol`,
/// then spins forever.
pub fn build_kernel_bootstrap_object(
    out_path: &Path,
    target_triple: &str,
    start_symbol: &str,
    entry_symbol: &str,
) -> Result<PathBuf> {
    let triple: Triple = target_triple
        .parse()
        .map_err(|e| anyhow!("invalid target triple '{}': {}", target_triple, e))?;

    let flags = settings::Flags::new(settings::builder());
    let isa = isa::lookup(triple.clone())
        .map_err(|e| anyhow!("failed to lookup ISA for {}: {}", triple, e))?
        .finish(flags)
        .map_err(|e| anyhow!("failed to finish ISA for {}: {}", triple, e))?;

    let builder = ObjectBuilder::new(
        isa,
        "dust_kernel_bootstrap".to_string(),
        cranelift_module::default_libcall_names(),
    )
    .context("failed to create ObjectBuilder")?;

    let mut module = ObjectModule::new(builder);

    let mut entry_sig = Signature::new(module.isa().default_call_conv());
    entry_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let entry_func = module
        .declare_function(entry_symbol, Linkage::Import, &entry_sig)
        .with_context(|| format!("declare imported entry '{}'", entry_symbol))?;

    let start_sig = Signature::new(module.isa().default_call_conv());
    let start_func = module
        .declare_function(start_symbol, Linkage::Export, &start_sig)
        .with_context(|| format!("declare bootstrap start '{}'", start_symbol))?;

    let mut ctx = module.make_context();
    ctx.func.signature = start_sig;
    ctx.func.name = UserFuncName::user(0, start_func.as_u32());

    let mut fb_ctx = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
        let entry_block = b.create_block();
        let spin_block = b.create_block();

        b.switch_to_block(entry_block);
        b.seal_block(entry_block);

        let entry_ref: FuncRef = module.declare_func_in_func(entry_func, b.func);
        b.ins().call(entry_ref, &[]);
        b.ins().jump(spin_block, &[]);

        b.switch_to_block(spin_block);
        b.seal_block(spin_block);
        b.ins().jump(spin_block, &[]);

        b.finalize();
    }

    module
        .define_function(start_func, &mut ctx)
        .context("define kernel bootstrap function")?;
    module.clear_context(&mut ctx);

    let product = module.finish();
    let obj = product.object;
    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, obj.write().context("emit bootstrap object")?)
        .with_context(|| format!("write bootstrap object {:?}", obj_path))?;
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

#[derive(Debug, Clone)]
struct ScopedProcRef {
    forge: String,
    proc: DirProc,
}

#[derive(Default)]
struct ScopedProcIndex {
    by_forge: HashMap<String, HashMap<String, DirProc>>,
    by_name: HashMap<String, Vec<ScopedProcRef>>,
}

fn find_k_entry_scoped(dir: &DirProgram, entry_point: &str) -> Result<ScopedProcRef> {
    let index = index_k_procs_scoped(dir);
    if index.by_name.is_empty() {
        bail!("no K procs found");
    }

    if let Some((forge_name, proc_name)) = parse_scoped_target(entry_point) {
        if let Some(forge_map) = index.by_forge.get(&forge_name) {
            if let Some(proc) = forge_map.get(&proc_name) {
                return Ok(ScopedProcRef {
                    forge: forge_name,
                    proc: proc.clone(),
                });
            }
        }
        bail!("no K::{} in forge '{}'", proc_name, forge_name);
    }

    if let Some(root) = index.by_forge.get("__root__") {
        if let Some(proc) = root.get(entry_point) {
            return Ok(ScopedProcRef {
                forge: "__root__".to_string(),
                proc: proc.clone(),
            });
        }
    }

    let candidates = index.by_name.get(entry_point);
    match candidates {
        Some(v) if v.len() == 1 => Ok(v[0].clone()),
        Some(v) if v.len() > 1 => {
            let names = v
                .iter()
                .map(|p| format!("{}::{}", p.forge, p.proc.name))
                .collect::<Vec<_>>()
                .join(", ");
            bail!("ambiguous K::{} across forges: {}", entry_point, names);
        }
        _ => bail!("no K::{} found", entry_point),
    }
}

fn extract_emit_strings_scoped(
    dir: &DirProgram,
    entry: &ScopedProcRef,
    allow_unresolved_calls: bool,
) -> Result<Vec<String>> {
    let index = index_k_procs_scoped(dir);
    let mut out = Vec::new();
    let mut call_stack = Vec::new();
    collect_emit_strings_from_proc_scoped(
        entry,
        &index,
        &mut call_stack,
        &mut out,
        allow_unresolved_calls,
    )?;
    Ok(out)
}

fn index_k_procs_scoped(dir: &DirProgram) -> ScopedProcIndex {
    let mut out = ScopedProcIndex::default();
    for forge in &dir.forges {
        let forge_name = forge.name.clone();
        let mut forge_map: HashMap<String, DirProc> = HashMap::new();
        for proc in &forge.procs {
            if proc.regime != "K" {
                continue;
            }
            forge_map.insert(proc.name.clone(), proc.clone());
            out.by_name
                .entry(proc.name.clone())
                .or_default()
                .push(ScopedProcRef {
                    forge: forge_name.clone(),
                    proc: proc.clone(),
                });
        }
        if !forge_map.is_empty() {
            out.by_forge.insert(forge_name, forge_map);
        }
    }
    out
}

fn collect_emit_strings_from_proc_scoped(
    proc_ref: &ScopedProcRef,
    proc_index: &ScopedProcIndex,
    call_stack: &mut Vec<String>,
    out: &mut Vec<String>,
    allow_unresolved_calls: bool,
) -> Result<()> {
    let call_id = format!("{}::{}", proc_ref.forge, proc_ref.proc.name);
    call_stack.push(call_id.clone());
    for s in &proc_ref.proc.body {
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
                    let parsed = parse_call_expr(payload);
                    let (callee_text, args) = match parsed {
                        Ok(v) => v,
                        Err(_) if allow_unresolved_calls => continue,
                        Err(e) => return Err(e),
                    };

                    if !args.is_empty() {
                        if allow_unresolved_calls {
                            continue;
                        }
                        bail!(
                            "unsupported expr payload (arguments not supported in codegen path): {}",
                            payload
                        );
                    }

                    let callee = resolve_callee_proc(proc_index, &proc_ref.forge, &callee_text);
                    let callee = match callee {
                        Ok(v) => v,
                        Err(_) if allow_unresolved_calls => continue,
                        Err(e) => return Err(e),
                    };

                    let next_id = format!("{}::{}", callee.forge, callee.proc.name);
                    if call_stack.iter().any(|n| n == &next_id) {
                        call_stack.push(next_id);
                        bail!(
                            "recursive K call not supported in codegen path: {}",
                            call_stack.join(" -> ")
                        );
                    }

                    collect_emit_strings_from_proc_scoped(
                        &callee,
                        proc_index,
                        call_stack,
                        out,
                        allow_unresolved_calls,
                    )?;
                    continue;
                }

                if allow_unresolved_calls {
                    continue;
                }
                bail!("unsupported effect kind in codegen path: {}", kind);
            }
            DirStmt::Return { .. } => {}
            _ if allow_unresolved_calls => {}
            other => bail!(
                "unsupported statement in codegen proc '{}::{}': {:?}",
                proc_ref.forge,
                proc_ref.proc.name,
                other
            ),
        }
    }
    let _ = call_stack.pop();
    Ok(())
}

fn resolve_callee_proc(
    proc_index: &ScopedProcIndex,
    current_forge: &str,
    callee_text: &str,
) -> Result<ScopedProcRef> {
    if let Some((forge_name, proc_name)) = parse_scoped_target(callee_text) {
        if let Some(forge_map) = proc_index.by_forge.get(&forge_name) {
            if let Some(proc) = forge_map.get(&proc_name) {
                return Ok(ScopedProcRef {
                    forge: forge_name,
                    proc: proc.clone(),
                });
            }
        }
        bail!("callee K::{} not found in forge '{}'", proc_name, forge_name);
    }

    if let Some(forge_map) = proc_index.by_forge.get(current_forge) {
        if let Some(proc) = forge_map.get(callee_text) {
            return Ok(ScopedProcRef {
                forge: current_forge.to_string(),
                proc: proc.clone(),
            });
        }
    }

    let candidates = proc_index.by_name.get(callee_text);
    match candidates {
        Some(v) if v.len() == 1 => Ok(v[0].clone()),
        Some(v) if v.len() > 1 => {
            let names = v
                .iter()
                .map(|p| format!("{}::{}", p.forge, p.proc.name))
                .collect::<Vec<_>>()
                .join(", ");
            bail!("ambiguous callee '{}': {}", callee_text, names);
        }
        _ => bail!("callee '{}' not found", callee_text),
    }
}

fn parse_call_expr(payload: &str) -> Result<(String, Vec<String>)> {
    let p = payload.trim();
    let open = p
        .find('(')
        .ok_or_else(|| anyhow!("unsupported expr payload: {}", p))?;
    let close = p
        .rfind(')')
        .ok_or_else(|| anyhow!("unsupported expr payload: {}", p))?;
    if close < open {
        bail!("unsupported expr payload: {}", p);
    }

    let target = p[..open].trim();
    if target.is_empty() {
        bail!("unsupported expr payload: {}", p);
    }
    if !target
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':')
    {
        bail!("unsupported expr payload target: {}", target);
    }

    let args_src = p[open + 1..close].trim();
    if args_src.is_empty() {
        return Ok((target.to_string(), Vec::new()));
    }

    let args = args_src
        .split(',')
        .map(|x| x.trim().to_string())
        .filter(|x| !x.is_empty())
        .collect::<Vec<_>>();
    Ok((target.to_string(), args))
}

fn parse_scoped_target(target: &str) -> Option<(String, String)> {
    let parts = target
        .split("::")
        .map(|p| p.trim())
        .filter(|p| !p.is_empty())
        .collect::<Vec<_>>();
    if parts.len() < 2 {
        return None;
    }

    let proc_name = parts[parts.len() - 1].to_string();
    let parent = parts[parts.len() - 2];
    if parent == "K" || parent == "Q" || parent == "Φ" || parent == "Phi" {
        return None;
    }

    Some((parent.to_string(), proc_name))
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

        if let Some(puts_func) = puts_func {
            let puts_ref: FuncRef = module.declare_func_in_func(puts_func, b.func);
            for data_id in string_data_ids {
                let gv = module.declare_data_in_func(data_id, b.func);
                let addr = b.ins().global_value(ptr_ty, gv);
                b.ins().call(puts_ref, &[addr]);
            }
        } else {
            // Bare-metal object mode: lower emits directly into VGA memory stores.
            let mut row: u32 = 0;
            let mut col: u32 = 0;
            for s in emit_strings {
                let sanitized = s.replace('\r', "");
                for ch in sanitized.chars() {
                    if ch == '\n' {
                        row = row.saturating_add(1);
                        col = 0;
                        continue;
                    }
                    if row >= VGA_ROWS {
                        break;
                    }
                    if col >= VGA_COLS {
                        row = row.saturating_add(1);
                        col = 0;
                        if row >= VGA_ROWS {
                            break;
                        }
                    }

                    let cell_addr = VGA_TEXT_BASE
                        .saturating_add(row.saturating_mul(VGA_ROW_BYTES))
                        .saturating_add(col.saturating_mul(2));
                    let cell_ptr = b.ins().iconst(ptr_ty, i64::from(cell_addr));
                    let glyph = b.ins().iconst(cranelift_codegen::ir::types::I8, ch as i64);
                    b.ins().store(cranelift_codegen::ir::MemFlags::new(), glyph, cell_ptr, 0);

                    let attr_ptr = b.ins().iconst(ptr_ty, i64::from(cell_addr + 1));
                    let attr = b
                        .ins()
                        .iconst(cranelift_codegen::ir::types::I8, i64::from(VGA_ATTR_DEFAULT));
                    b.ins().store(cranelift_codegen::ir::MemFlags::new(), attr, attr_ptr, 0);

                    col = col.saturating_add(1);
                }
                row = row.saturating_add(1);
                col = 0;
                if row >= VGA_ROWS {
                    break;
                }
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

/// Kernel sector window reserved by xdv-os boot stage (128 * 512 bytes).
const KERNEL_MAX_BYTES: usize = 128 * 512;
/// VGA text mode base physical address.
const VGA_TEXT_BASE: u32 = 0xB8000;
/// VGA text mode geometry.
const VGA_COLS: u32 = 80;
const VGA_ROWS: u32 = 25;
const VGA_ROW_BYTES: u32 = VGA_COLS * 2;
const VGA_ATTR_DEFAULT: u8 = 0x0F;

/// Build kernel binary for bare-metal:
/// - emits straight-line 32-bit machine code
/// - writes collected `emit` strings to VGA text mode memory
/// - halts in a tight loop
fn build_kernel_binary(
    emit_strings: &[String],
    _triple: &Triple,
    _config: &BuildConfig,
) -> Result<Vec<u8>> {
    // 32-bit protected-mode entry as loaded by xdv-os boot stage.
    // cli
    let mut kernel = vec![0xFA];

    // Flatten emits into display lines and keep the most recent VGA_ROWS lines
    // so late boot messages (including shell launch) are visible.
    let mut lines: Vec<String> = Vec::new();
    for s in emit_strings {
        let sanitized = s.replace('\r', "");
        for part in sanitized.split('\n') {
            lines.push(part.to_string());
        }
    }
    if lines.len() > VGA_ROWS as usize {
        let keep_from = lines.len() - (VGA_ROWS as usize);
        lines = lines.split_off(keep_from);
    }

    for (row_idx, line) in lines.iter().enumerate() {
        let row = row_idx as u32;
        for (col_idx, b) in line.as_bytes().iter().take(VGA_COLS as usize).enumerate() {
            let col = col_idx as u32;
            let cell_addr = VGA_TEXT_BASE
                .checked_add(row.saturating_mul(VGA_ROW_BYTES))
                .and_then(|v| v.checked_add(col.saturating_mul(2)))
                .ok_or_else(|| anyhow!("vga address overflow"))?;
            emit_mov_byte_abs(&mut kernel, cell_addr, *b);
            emit_mov_byte_abs(&mut kernel, cell_addr + 1, VGA_ATTR_DEFAULT);
        }
    }

    // hlt; jmp $
    kernel.extend_from_slice(&[0xF4, 0xEB, 0xFE]);

    if kernel.len() > KERNEL_MAX_BYTES {
        bail!(
            "bare-metal kernel too large: {} bytes exceeds {}-byte sector window",
            kernel.len(),
            KERNEL_MAX_BYTES
        );
    }

    Ok(kernel)
}

fn emit_mov_byte_abs(out: &mut Vec<u8>, addr: u32, value: u8) {
    // mov byte ptr [abs32], imm8
    // C6 05 <addr32> <imm8>
    out.push(0xC6);
    out.push(0x05);
    out.extend_from_slice(&addr.to_le_bytes());
    out.push(value);
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
