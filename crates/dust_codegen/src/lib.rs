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
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    types, AbiParam, Block, FuncRef, InstBuilder, Signature, Type, UserFuncName, Value,
};
use cranelift_codegen::isa;
use cranelift_codegen::settings;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_frontend::Variable;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use dust_dir::{DirProgram, DirProc, DirStmt};
use std::collections::{BTreeMap, HashMap};
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
    let obj_bytes = build_host_object_from_dir(dir, "main")?;

    // Write object next to output and link
    let obj_path = write_object_temp(&exe_path, &obj_bytes)?;
    let host_runtime_lib = compile_host_runtime_library(&exe_path)?;
    let link_inputs = vec![obj_path.clone(), host_runtime_lib.clone()];
    link_executable(&link_inputs, &exe_path)?;

    // Best-effort cleanup
    let _ = fs::remove_file(&obj_path);
    let _ = fs::remove_file(&host_runtime_lib);

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
    let entry = find_k_entry_scoped(dir, "main").context("no K::main found")?;
    Ok(entry.proc)
}


fn extract_emit_strings(stmts: &[DirStmt]) -> Result<Vec<String>> {
    let mut out = Vec::new();

    for s in stmts {
        if let DirStmt::Effect { kind, payload } = s {
            if kind == "emit" {
                let decoded = decode_string_literal(payload).with_context(|| {
                    format!("emit payload must be a string literal, got: {}", payload)
                })?;
                out.push(decoded);
            }
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

#[derive(Debug, Clone)]
struct HostImport {
    func_id: FuncId,
    argc: usize,
}

#[derive(Default)]
struct HostProcFrame {
    vars: HashMap<String, Variable>,
    next_var: usize,
    break_targets: Vec<Block>,
    continue_targets: Vec<Block>,
    current_forge: String,
}

impl HostProcFrame {
    fn new(current_forge: String) -> Self {
        Self {
            current_forge,
            ..Self::default()
        }
    }

    fn ensure_var(&mut self, builder: &mut FunctionBuilder, name: &str, word_ty: Type) -> Variable {
        if let Some(existing) = self.vars.get(name) {
            return *existing;
        }
        let var = Variable::from_u32(self.next_var as u32);
        self.next_var += 1;
        builder.declare_var(var, word_ty);
        self.vars.insert(name.to_string(), var);
        var
    }

    fn set_var(
        &mut self,
        builder: &mut FunctionBuilder,
        name: &str,
        value: Value,
        word_ty: Type,
    ) {
        let var = self.ensure_var(builder, name, word_ty);
        builder.def_var(var, value);
    }

    fn get_var(&self, builder: &mut FunctionBuilder, name: &str) -> Result<Value> {
        let var = self
            .vars
            .get(name)
            .ok_or_else(|| anyhow!("unknown identifier '{}' in forge '{}'", name, self.current_forge))?;
        Ok(builder.use_var(*var))
    }
}

fn build_host_object_from_dir(dir: &DirProgram, entry_point: &str) -> Result<Vec<u8>> {
    let entry = find_k_entry_scoped(dir, entry_point)
        .context("DIR does not contain a host-codegen-capable K::main")?;

    let triple = Triple::host();
    let flags = settings::Flags::new(settings::builder());
    let isa = isa::lookup(triple.clone())
        .map_err(|e| anyhow!("failed to lookup ISA for {}: {}", triple, e))?
        .finish(flags)
        .map_err(|e| anyhow!("failed to finish ISA for {}: {}", triple, e))?;

    let builder = ObjectBuilder::new(
        isa,
        "dust_host_module".to_string(),
        cranelift_module::default_libcall_names(),
    )
    .context("failed to create ObjectBuilder")?;

    let mut module = ObjectModule::new(builder);
    let word_ty = module.target_config().pointer_type();

    let mut proc_defs: BTreeMap<String, ScopedProcRef> = BTreeMap::new();
    for forge in &dir.forges {
        for proc in &forge.procs {
            if proc.regime != "K" {
                continue;
            }
            let scoped = ScopedProcRef {
                forge: forge.name.clone(),
                proc: proc.clone(),
            };
            proc_defs.insert(scoped_proc_key(&scoped), scoped);
        }
    }
    if proc_defs.is_empty() {
        bail!("no K procs available for host codegen");
    }

    let entry_key = scoped_proc_key(&entry);
    if !proc_defs.contains_key(&entry_key) {
        bail!("entry '{}' not found in host codegen set", entry_key);
    }

    let mut proc_ids: HashMap<String, FuncId> = HashMap::new();
    for (key, proc_ref) in &proc_defs {
        let mut sig = Signature::new(module.isa().default_call_conv());
        for _ in &proc_ref.proc.params {
            sig.params.push(AbiParam::new(word_ty));
        }
        sig.returns.push(AbiParam::new(word_ty));
        let symbol = mangle_host_proc_symbol(&proc_ref.forge, &proc_ref.proc.name);
        let func_id = module
            .declare_function(&symbol, Linkage::Local, &sig)
            .with_context(|| format!("declare host proc {}", key))?;
        proc_ids.insert(key.clone(), func_id);
    }

    let mut main_sig = Signature::new(module.isa().default_call_conv());
    main_sig.returns.push(AbiParam::new(types::I32));
    let main_func = module
        .declare_function("main", Linkage::Export, &main_sig)
        .context("declare host main")?;

    let proc_index = index_k_procs_scoped(dir);
    let mut imports: HashMap<String, HostImport> = HashMap::new();
    let mut string_data: HashMap<String, cranelift_module::DataId> = HashMap::new();

    for (key, proc_ref) in &proc_defs {
        let func_id = *proc_ids
            .get(key)
            .ok_or_else(|| anyhow!("missing proc id for {}", key))?;
        compile_host_proc(
            &mut module,
            &proc_index,
            proc_ref,
            func_id,
            &proc_ids,
            &mut imports,
            &mut string_data,
            word_ty,
        )?;
    }

    compile_host_main_wrapper(
        &mut module,
        main_func,
        *proc_ids
            .get(&entry_key)
            .ok_or_else(|| anyhow!("missing entry function id"))?,
        entry.proc.params.len(),
        word_ty,
    )?;

    let product = module.finish();
    Ok(product.object.write().context("emit host object")?)
}

fn compile_host_main_wrapper(
    module: &mut ObjectModule,
    main_func: FuncId,
    entry_func: FuncId,
    entry_param_count: usize,
    word_ty: Type,
) -> Result<()> {
    if entry_param_count != 0 {
        bail!(
            "K::main host entry must take zero params (found {})",
            entry_param_count
        );
    }

    let mut main_sig = Signature::new(module.isa().default_call_conv());
    main_sig.returns.push(AbiParam::new(types::I32));

    let mut ctx = module.make_context();
    ctx.func.signature = main_sig;
    ctx.func.name = UserFuncName::user(0, main_func.as_u32());

    let mut fb_ctx = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
        let block = b.create_block();
        b.switch_to_block(block);
        b.seal_block(block);

        let entry_ref = module.declare_func_in_func(entry_func, b.func);
        let call = b.ins().call(entry_ref, &[]);
        let ret_word = b
            .inst_results(call)
            .first()
            .copied()
            .unwrap_or_else(|| b.ins().iconst(word_ty, 0));
        let ret_i32 = if word_ty == types::I32 {
            ret_word
        } else {
            b.ins().ireduce(types::I32, ret_word)
        };
        b.ins().return_(&[ret_i32]);
        b.finalize();
    }

    module
        .define_function(main_func, &mut ctx)
        .context("define host main wrapper")?;
    module.clear_context(&mut ctx);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn compile_host_proc(
    module: &mut ObjectModule,
    proc_index: &ScopedProcIndex,
    proc_ref: &ScopedProcRef,
    func_id: FuncId,
    proc_ids: &HashMap<String, FuncId>,
    imports: &mut HashMap<String, HostImport>,
    string_data: &mut HashMap<String, cranelift_module::DataId>,
    word_ty: Type,
) -> Result<()> {
    let mut sig = Signature::new(module.isa().default_call_conv());
    for _ in &proc_ref.proc.params {
        sig.params.push(AbiParam::new(word_ty));
    }
    sig.returns.push(AbiParam::new(word_ty));

    let mut ctx = module.make_context();
    ctx.func.signature = sig;
    ctx.func.name = UserFuncName::user(0, func_id.as_u32());

    let mut fb_ctx = FunctionBuilderContext::new();
    {
        let mut b = FunctionBuilder::new(&mut ctx.func, &mut fb_ctx);
        let entry_block = b.create_block();
        b.append_block_params_for_function_params(entry_block);
        b.switch_to_block(entry_block);
        b.seal_block(entry_block);

        let mut frame = HostProcFrame::new(proc_ref.forge.clone());
        for (idx, param) in proc_ref.proc.params.iter().enumerate() {
            let value = b.block_params(entry_block)[idx];
            frame.set_var(&mut b, &param.name, value, word_ty);
        }

        let reachable = lower_stmt_list(
            module,
            proc_index,
            proc_ids,
            imports,
            string_data,
            &mut b,
            &mut frame,
            &proc_ref.proc.body,
            word_ty,
        )?;
        if reachable {
            let zero = b.ins().iconst(word_ty, 0);
            b.ins().return_(&[zero]);
        }
        b.seal_all_blocks();
        b.finalize();
    }

    module
        .define_function(func_id, &mut ctx)
        .with_context(|| format!("define host proc {}::{}", proc_ref.forge, proc_ref.proc.name))?;
    module.clear_context(&mut ctx);
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn lower_stmt_list(
    module: &mut ObjectModule,
    proc_index: &ScopedProcIndex,
    proc_ids: &HashMap<String, FuncId>,
    imports: &mut HashMap<String, HostImport>,
    string_data: &mut HashMap<String, cranelift_module::DataId>,
    b: &mut FunctionBuilder,
    frame: &mut HostProcFrame,
    stmts: &[DirStmt],
    word_ty: Type,
) -> Result<bool> {
    let mut reachable = true;
    for stmt in stmts {
        if !reachable {
            break;
        }
        reachable = lower_stmt(
            module,
            proc_index,
            proc_ids,
            imports,
            string_data,
            b,
            frame,
            stmt,
            word_ty,
        )?;
    }
    Ok(reachable)
}

#[allow(clippy::too_many_arguments)]
fn lower_stmt(
    module: &mut ObjectModule,
    proc_index: &ScopedProcIndex,
    proc_ids: &HashMap<String, FuncId>,
    imports: &mut HashMap<String, HostImport>,
    string_data: &mut HashMap<String, cranelift_module::DataId>,
    b: &mut FunctionBuilder,
    frame: &mut HostProcFrame,
    stmt: &DirStmt,
    word_ty: Type,
) -> Result<bool> {
    match stmt {
        DirStmt::Let { name, expr } => {
            let value = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, expr, word_ty,
            )?;
            frame.set_var(b, name, value, word_ty);
            Ok(true)
        }
        DirStmt::Assign { target, expr } => {
            let value = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, expr, word_ty,
            )?;
            frame.set_var(b, target, value, word_ty);
            Ok(true)
        }
        DirStmt::Call {
            target,
            args,
            result,
        } => {
            let value = lower_call(
                module, proc_index, proc_ids, imports, string_data, b, frame, target, args, word_ty,
            )?;
            if let Some(name) = result {
                frame.set_var(b, name, value, word_ty);
            }
            Ok(true)
        }
        DirStmt::Effect { kind, payload } if kind == "emit" => {
            let text = decode_string_literal(payload)
                .with_context(|| format!("emit payload must be string literal: {}", payload))?;
            let ptr = materialize_string_literal(module, string_data, b, &text)?;
            let puts = get_or_declare_import(module, imports, "puts", 1, word_ty)?;
            let puts_ref = module.declare_func_in_func(puts, b.func);
            b.ins().call(puts_ref, &[ptr]);
            Ok(true)
        }
        DirStmt::Effect { kind, payload } if kind == "expr" => {
            let _ = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, payload, word_ty,
            )?;
            Ok(true)
        }
        DirStmt::Effect { kind, .. } => bail!("unsupported effect kind in host codegen: {}", kind),
        DirStmt::Return { expr } => {
            let value = if let Some(raw) = expr {
                lower_expr(
                    module, proc_index, proc_ids, imports, string_data, b, frame, raw, word_ty,
                )?
            } else {
                b.ins().iconst(word_ty, 0)
            };
            b.ins().return_(&[value]);
            Ok(false)
        }
        DirStmt::If {
            condition,
            then_body,
            else_body,
        } => {
            let cond_value = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, condition, word_ty,
            )?;
            let cond_bool = truthy_value(b, cond_value, word_ty);
            let then_block = b.create_block();
            let else_block = b.create_block();
            let merge_block = b.create_block();
            b.ins().brif(cond_bool, then_block, &[], else_block, &[]);

            b.switch_to_block(then_block);
            let then_reachable = lower_stmt_list(
                module,
                proc_index,
                proc_ids,
                imports,
                string_data,
                b,
                frame,
                then_body,
                word_ty,
            )?;
            if then_reachable {
                b.ins().jump(merge_block, &[]);
            }
            b.seal_block(then_block);

            b.switch_to_block(else_block);
            let else_reachable = if let Some(stmts) = else_body {
                lower_stmt_list(
                    module,
                    proc_index,
                    proc_ids,
                    imports,
                    string_data,
                    b,
                    frame,
                    stmts,
                    word_ty,
                )?
            } else {
                true
            };
            if else_reachable {
                b.ins().jump(merge_block, &[]);
            }
            b.seal_block(else_block);

            if then_reachable || else_reachable {
                b.switch_to_block(merge_block);
                b.seal_block(merge_block);
                Ok(true)
            } else {
                b.seal_block(merge_block);
                Ok(false)
            }
        }
        DirStmt::While { condition, body } => {
            let header = b.create_block();
            let body_block = b.create_block();
            let exit_block = b.create_block();

            b.ins().jump(header, &[]);
            b.switch_to_block(header);
            let cond_value = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, condition, word_ty,
            )?;
            let cond_bool = truthy_value(b, cond_value, word_ty);
            b.ins().brif(cond_bool, body_block, &[], exit_block, &[]);

            b.switch_to_block(body_block);
            frame.break_targets.push(exit_block);
            frame.continue_targets.push(header);
            let body_reachable = lower_stmt_list(
                module,
                proc_index,
                proc_ids,
                imports,
                string_data,
                b,
                frame,
                body,
                word_ty,
            )?;
            frame.continue_targets.pop();
            frame.break_targets.pop();
            if body_reachable {
                b.ins().jump(header, &[]);
            }
            b.seal_block(body_block);
            b.seal_block(header);

            b.switch_to_block(exit_block);
            b.seal_block(exit_block);
            Ok(true)
        }
        DirStmt::For {
            var,
            start,
            end,
            body,
        } => {
            let start_value = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, start, word_ty,
            )?;
            let end_value = lower_expr(
                module, proc_index, proc_ids, imports, string_data, b, frame, end, word_ty,
            )?;
            frame.set_var(b, var, start_value, word_ty);

            let header = b.create_block();
            let body_block = b.create_block();
            let step_block = b.create_block();
            let exit_block = b.create_block();

            b.ins().jump(header, &[]);
            b.switch_to_block(header);
            let current = frame.get_var(b, var)?;
            let cond = b.ins().icmp(IntCC::UnsignedLessThan, current, end_value);
            b.ins().brif(cond, body_block, &[], exit_block, &[]);

            b.switch_to_block(body_block);
            frame.break_targets.push(exit_block);
            frame.continue_targets.push(step_block);
            let body_reachable = lower_stmt_list(
                module,
                proc_index,
                proc_ids,
                imports,
                string_data,
                b,
                frame,
                body,
                word_ty,
            )?;
            frame.continue_targets.pop();
            frame.break_targets.pop();
            if body_reachable {
                b.ins().jump(step_block, &[]);
            }
            b.seal_block(body_block);

            b.switch_to_block(step_block);
            let cur = frame.get_var(b, var)?;
            let one = b.ins().iconst(word_ty, 1);
            let next = b.ins().iadd(cur, one);
            frame.set_var(b, var, next, word_ty);
            b.ins().jump(header, &[]);
            b.seal_block(step_block);
            b.seal_block(header);

            b.switch_to_block(exit_block);
            b.seal_block(exit_block);
            Ok(true)
        }
        DirStmt::Break => {
            let target = *frame
                .break_targets
                .last()
                .ok_or_else(|| anyhow!("break outside loop"))?;
            b.ins().jump(target, &[]);
            Ok(false)
        }
        DirStmt::Continue => {
            let target = *frame
                .continue_targets
                .last()
                .ok_or_else(|| anyhow!("continue outside loop"))?;
            b.ins().jump(target, &[]);
            Ok(false)
        }
        other => bail!("unsupported statement in host codegen: {:?}", other),
    }
}

#[allow(clippy::too_many_arguments)]
fn lower_expr(
    module: &mut ObjectModule,
    proc_index: &ScopedProcIndex,
    proc_ids: &HashMap<String, FuncId>,
    imports: &mut HashMap<String, HostImport>,
    string_data: &mut HashMap<String, cranelift_module::DataId>,
    b: &mut FunctionBuilder,
    frame: &mut HostProcFrame,
    expr: &str,
    word_ty: Type,
) -> Result<Value> {
    let trimmed = expr.trim();
    if trimmed.is_empty() {
        bail!("empty expression in host codegen");
    }

    if let Some(inner) = strip_wrapping_parens(trimmed) {
        return lower_expr(
            module, proc_index, proc_ids, imports, string_data, b, frame, &inner, word_ty,
        );
    }

    if trimmed.starts_with('"') && trimmed.ends_with('"') {
        let text = decode_string_literal(trimmed)?;
        return materialize_string_literal(module, string_data, b, &text);
    }

    if trimmed == "true" {
        return Ok(b.ins().iconst(word_ty, 1));
    }
    if trimmed == "false" {
        return Ok(b.ins().iconst(word_ty, 0));
    }
    if let Ok(value) = trimmed.parse::<i64>() {
        return Ok(b.ins().iconst(word_ty, value));
    }

    if let Some((op, operand)) = parse_unary_expr(trimmed) {
        let value = lower_expr(
            module, proc_index, proc_ids, imports, string_data, b, frame, &operand, word_ty,
        )?;
        return lower_unary_op(b, &op, value, word_ty);
    }

    if let Some((lhs, op, rhs)) = split_binary_expr(trimmed) {
        let lhs_value = lower_expr(
            module, proc_index, proc_ids, imports, string_data, b, frame, &lhs, word_ty,
        )?;
        let rhs_value = lower_expr(
            module, proc_index, proc_ids, imports, string_data, b, frame, &rhs, word_ty,
        )?;
        return lower_binary_op(b, &op, lhs_value, rhs_value, word_ty);
    }

    if let Some((target, args)) = parse_call_expr_runtime(trimmed)? {
        return lower_call(
            module, proc_index, proc_ids, imports, string_data, b, frame, &target, &args, word_ty,
        );
    }

    if is_identifier_like(trimmed) {
        return frame.get_var(b, trimmed);
    }

    bail!("unsupported expression in host codegen: {}", trimmed)
}

fn lower_unary_op(
    b: &mut FunctionBuilder,
    op: &str,
    operand: Value,
    word_ty: Type,
) -> Result<Value> {
    match op {
        "Neg" | "-" => Ok(b.ins().ineg(operand)),
        "Not" | "!" => {
            let cond = truthy_value(b, operand, word_ty);
            let as_int = b.ins().uextend(word_ty, cond);
            let one = b.ins().iconst(word_ty, 1);
            Ok(b.ins().bxor(as_int, one))
        }
        _ => bail!("unsupported unary operator in host codegen: {}", op),
    }
}

fn lower_binary_op(
    b: &mut FunctionBuilder,
    op: &str,
    lhs: Value,
    rhs: Value,
    word_ty: Type,
) -> Result<Value> {
    match op {
        "Add" | "+" => Ok(b.ins().iadd(lhs, rhs)),
        "Sub" | "-" => Ok(b.ins().isub(lhs, rhs)),
        "Mul" | "*" => Ok(b.ins().imul(lhs, rhs)),
        "Div" | "/" => Ok(b.ins().udiv(lhs, rhs)),
        "BitAnd" | "&" => Ok(b.ins().band(lhs, rhs)),
        "BitOr" | "|" => Ok(b.ins().bor(lhs, rhs)),
        "BitXor" | "^" => Ok(b.ins().bxor(lhs, rhs)),
        "Shl" | "<<" => Ok(b.ins().ishl(lhs, rhs)),
        "Shr" | ">>" => Ok(b.ins().ushr(lhs, rhs)),
        "Eq" | "==" => {
            let cmp = b.ins().icmp(IntCC::Equal, lhs, rhs);
            Ok(b.ins().uextend(word_ty, cmp))
        }
        "Ne" | "!=" => {
            let cmp = b.ins().icmp(IntCC::NotEqual, lhs, rhs);
            Ok(b.ins().uextend(word_ty, cmp))
        }
        "Lt" | "<" => {
            let cmp = b.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs);
            Ok(b.ins().uextend(word_ty, cmp))
        }
        "Le" | "<=" => {
            let cmp = b.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs);
            Ok(b.ins().uextend(word_ty, cmp))
        }
        "Gt" | ">" => {
            let cmp = b.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs);
            Ok(b.ins().uextend(word_ty, cmp))
        }
        "Ge" | ">=" => {
            let cmp = b.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs);
            Ok(b.ins().uextend(word_ty, cmp))
        }
        "And" | "&&" => {
            let lb = truthy_value(b, lhs, word_ty);
            let rb = truthy_value(b, rhs, word_ty);
            let both = b.ins().band(lb, rb);
            Ok(b.ins().uextend(word_ty, both))
        }
        "Or" | "||" => {
            let lb = truthy_value(b, lhs, word_ty);
            let rb = truthy_value(b, rhs, word_ty);
            let either = b.ins().bor(lb, rb);
            Ok(b.ins().uextend(word_ty, either))
        }
        _ => bail!("unsupported binary operator in host codegen: {}", op),
    }
}

fn truthy_value(b: &mut FunctionBuilder, value: Value, word_ty: Type) -> Value {
    let zero = b.ins().iconst(word_ty, 0);
    b.ins().icmp(IntCC::NotEqual, value, zero)
}

#[allow(clippy::too_many_arguments)]
fn lower_call(
    module: &mut ObjectModule,
    proc_index: &ScopedProcIndex,
    proc_ids: &HashMap<String, FuncId>,
    imports: &mut HashMap<String, HostImport>,
    string_data: &mut HashMap<String, cranelift_module::DataId>,
    b: &mut FunctionBuilder,
    frame: &mut HostProcFrame,
    target: &str,
    args: &[String],
    word_ty: Type,
) -> Result<Value> {
    let mut arg_values = Vec::with_capacity(args.len());
    for arg in args {
        let value = lower_expr(
            module, proc_index, proc_ids, imports, string_data, b, frame, arg, word_ty,
        )?;
        arg_values.push(value);
    }

    let func_id = match resolve_callee_proc(proc_index, &frame.current_forge, target) {
        Ok(callee) => {
            if callee.proc.params.len() != args.len() {
                bail!(
                    "call arity mismatch for {}::{}: expected {}, got {}",
                    callee.forge,
                    callee.proc.name,
                    callee.proc.params.len(),
                    args.len()
                );
            }
            let key = scoped_proc_key(&callee);
            *proc_ids
                .get(&key)
                .ok_or_else(|| anyhow!("missing compiled callee id for {}", key))?
        }
        Err(_) => get_or_declare_import(module, imports, target, args.len(), word_ty)?,
    };

    let callee_ref = module.declare_func_in_func(func_id, b.func);
    let call = b.ins().call(callee_ref, &arg_values);
    Ok(b
        .inst_results(call)
        .first()
        .copied()
        .unwrap_or_else(|| b.ins().iconst(word_ty, 0)))
}

fn get_or_declare_import(
    module: &mut ObjectModule,
    imports: &mut HashMap<String, HostImport>,
    symbol: &str,
    argc: usize,
    word_ty: Type,
) -> Result<FuncId> {
    if let Some(existing) = imports.get(symbol) {
        if existing.argc != argc {
            bail!(
                "inconsistent import arity for '{}': first seen {}, now {}",
                symbol,
                existing.argc,
                argc
            );
        }
        return Ok(existing.func_id);
    }

    let mut sig = Signature::new(module.isa().default_call_conv());
    for _ in 0..argc {
        sig.params.push(AbiParam::new(word_ty));
    }
    sig.returns.push(AbiParam::new(word_ty));
    let func_id = module
        .declare_function(symbol, Linkage::Import, &sig)
        .with_context(|| format!("declare import '{}'", symbol))?;
    imports.insert(symbol.to_string(), HostImport { func_id, argc });
    Ok(func_id)
}

fn materialize_string_literal(
    module: &mut ObjectModule,
    string_data: &mut HashMap<String, cranelift_module::DataId>,
    b: &mut FunctionBuilder,
    text: &str,
) -> Result<Value> {
    let data_id = if let Some(existing) = string_data.get(text) {
        *existing
    } else {
        let name = format!("__dust_host_str_{}", string_data.len());
        let data_id = module
            .declare_data(&name, Linkage::Local, true, false)
            .with_context(|| format!("declare host string data {}", name))?;
        let mut desc = DataDescription::new();
        let mut bytes = text.as_bytes().to_vec();
        bytes.push(0);
        desc.define(bytes.into_boxed_slice());
        module
            .define_data(data_id, &desc)
            .with_context(|| format!("define host string data {}", name))?;
        string_data.insert(text.to_string(), data_id);
        data_id
    };

    let gv = module.declare_data_in_func(data_id, b.func);
    let ptr_ty = module.target_config().pointer_type();
    Ok(b.ins().global_value(ptr_ty, gv))
}

fn strip_wrapping_parens(expr: &str) -> Option<String> {
    let trimmed = expr.trim();
    if !(trimmed.starts_with('(') && trimmed.ends_with(')')) {
        return None;
    }
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut escape = false;
    for (idx, ch) in trimmed.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '(' => depth += 1,
            ')' => {
                depth -= 1;
                if depth == 0 && idx != trimmed.len() - 1 {
                    return None;
                }
            }
            _ => {}
        }
    }
    if depth == 0 {
        Some(trimmed[1..trimmed.len() - 1].trim().to_string())
    } else {
        None
    }
}

fn parse_unary_expr(expr: &str) -> Option<(String, String)> {
    let trimmed = expr.trim();
    for op in ["Neg", "Not"] {
        let prefix = format!("{}(", op);
        if trimmed.starts_with(&prefix) && trimmed.ends_with(')') {
            let inner = &trimmed[prefix.len()..trimmed.len() - 1];
            return Some((op.to_string(), inner.trim().to_string()));
        }
    }
    None
}

fn split_binary_expr(expr: &str) -> Option<(String, String, String)> {
    let trimmed = expr.trim();
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut escape = false;
    let mut token_start: Option<usize> = None;
    let mut op_start: Option<(usize, usize)> = None;
    let mut op_text = String::new();

    for (idx, ch) in trimmed.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            c if c.is_whitespace() && depth == 0 => {
                if let Some(start) = token_start.take() {
                    let token = &trimmed[start..idx];
                    if is_binary_operator_token(token) {
                        op_start = Some((start, idx));
                        op_text = token.to_string();
                        break;
                    }
                }
            }
            _ => {
                if depth == 0 && token_start.is_none() {
                    token_start = Some(idx);
                }
            }
        }
    }
    if op_start.is_none() {
        if let Some(start) = token_start {
            let token = &trimmed[start..];
            if is_binary_operator_token(token) {
                op_start = Some((start, trimmed.len()));
                op_text = token.to_string();
            }
        }
    }

    let (start, end) = op_start?;
    let lhs = trimmed[..start].trim();
    let rhs = trimmed[end..].trim();
    if lhs.is_empty() || rhs.is_empty() {
        return None;
    }
    Some((lhs.to_string(), op_text, rhs.to_string()))
}

fn is_binary_operator_token(token: &str) -> bool {
    matches!(
        token,
        "Add"
            | "Sub"
            | "Mul"
            | "Div"
            | "Eq"
            | "Ne"
            | "Lt"
            | "Le"
            | "Gt"
            | "Ge"
            | "And"
            | "Or"
            | "BitAnd"
            | "BitOr"
            | "BitXor"
            | "Shl"
            | "Shr"
            | "+"
            | "-"
            | "*"
            | "/"
            | "=="
            | "!="
            | "<"
            | "<="
            | ">"
            | ">="
            | "&&"
            | "||"
            | "&"
            | "|"
            | "^"
            | "<<"
            | ">>"
    )
}

fn parse_call_expr_runtime(payload: &str) -> Result<Option<(String, Vec<String>)>> {
    let p = payload.trim();
    let open = match p.find('(') {
        Some(idx) => idx,
        None => return Ok(None),
    };
    let close = p
        .rfind(')')
        .ok_or_else(|| anyhow!("unsupported call expression payload: {}", p))?;
    if close != p.len() - 1 || close < open {
        return Ok(None);
    }

    let target = p[..open].trim();
    if target.is_empty() {
        bail!("unsupported call expression payload: {}", p);
    }

    let args_src = p[open + 1..close].trim();
    if args_src.is_empty() {
        return Ok(Some((target.to_string(), Vec::new())));
    }
    let args = split_args_top_level(args_src);
    Ok(Some((target.to_string(), args)))
}

fn split_args_top_level(src: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut depth: i32 = 0;
    let mut in_string = false;
    let mut escape = false;
    let mut start = 0usize;

    for (idx, ch) in src.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == '"' {
                in_string = false;
            }
            continue;
        }
        match ch {
            '"' => in_string = true,
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            ',' if depth == 0 => {
                let arg = src[start..idx].trim();
                if !arg.is_empty() {
                    out.push(arg.to_string());
                }
                start = idx + 1;
            }
            _ => {}
        }
    }

    let tail = src[start..].trim();
    if !tail.is_empty() {
        out.push(tail.to_string());
    }
    out
}

fn is_identifier_like(token: &str) -> bool {
    token
        .chars()
        .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':' || c == '.')
}

fn scoped_proc_key(proc_ref: &ScopedProcRef) -> String {
    format!("{}::{}", proc_ref.forge, proc_ref.proc.name)
}

fn mangle_host_proc_symbol(forge: &str, proc_name: &str) -> String {
    format!(
        "__dust_{}_{}",
        sanitize_symbol_component(forge),
        sanitize_symbol_component(proc_name)
    )
}

fn sanitize_symbol_component(text: &str) -> String {
    text.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
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
    let puts_func = if emit_strings.is_empty() {
        None
    } else {
        // Declare external C `puts`: int puts(const char*).
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

fn write_object_temp(exe_path: &Path, obj_bytes: &[u8]) -> Result<PathBuf> {
    let mut obj_path = exe_path.to_path_buf();
    obj_path.set_extension("o");
    fs::write(&obj_path, obj_bytes).with_context(|| format!("write object {:?}", obj_path))?;
    Ok(obj_path)
}

fn compile_host_runtime_library(exe_path: &Path) -> Result<PathBuf> {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let shim_src = manifest_dir.join("src").join("host_runtime_shim.rs");
    if !shim_src.is_file() {
        bail!("missing host runtime shim source: {}", shim_src.display());
    }

    let mut lib_path = exe_path.to_path_buf();
    lib_path.set_extension(if cfg!(windows) { "lib" } else { "a" });

    let mut cmd = Command::new("rustc");
    cmd.arg("--crate-name")
        .arg("dust_host_runtime")
        .arg("--crate-type")
        .arg("staticlib")
        .arg(&shim_src)
        .arg("-C")
        .arg("panic=abort")
        .arg("-O")
        .arg("-o")
        .arg(&lib_path);

    let output = cmd
        .output()
        .with_context(|| format!("invoke rustc for host runtime shim {}", shim_src.display()))?;
    if !output.status.success() {
        bail!(
            "failed to build host runtime shim with rustc\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(lib_path)
}

fn link_executable(link_inputs: &[PathBuf], exe_path: &Path) -> Result<()> {
    let bootstrap_dustlink = is_dustlink_bootstrap_target(exe_path);
    if cfg!(windows) {
        link_windows(link_inputs, exe_path, bootstrap_dustlink)
    } else if cfg!(target_os = "macos") {
        link_unix(link_inputs, exe_path, true, bootstrap_dustlink)
    } else {
        link_unix(link_inputs, exe_path, false, bootstrap_dustlink)
    }
}

fn is_dustlink_bootstrap_target(exe_path: &Path) -> bool {
    exe_path
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.eq_ignore_ascii_case("dustlink"))
        .unwrap_or(false)
}

#[derive(Clone)]
struct LinkAttempt {
    program: String,
    args: Vec<String>,
    note: &'static str,
}

fn link_unix(
    link_inputs: &[PathBuf],
    exe_path: &Path,
    _is_macos: bool,
    bootstrap_dustlink: bool,
) -> Result<()> {
    if link_inputs.is_empty() {
        bail!("link_unix called without input objects/libraries");
    }
    let inputs = link_inputs
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<_>>();
    let exe = exe_path.to_string_lossy().to_string();

    let mut attempts: Vec<LinkAttempt> = Vec::new();

    if !bootstrap_dustlink {
        let mut args = inputs.clone();
        args.push("-o".to_string());
        args.push(exe.clone());
        attempts.push(LinkAttempt {
            program: "dustlink".to_string(),
            args,
            note: "dustlink-preferred",
        });
    }

    let driver = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());

    let mut driver_lld_args = inputs.clone();
    driver_lld_args.push("-o".to_string());
    driver_lld_args.push(exe.clone());
    driver_lld_args.push("-fuse-ld=lld".to_string());
    attempts.push(LinkAttempt {
        program: driver.clone(),
        args: driver_lld_args,
        note: "driver+lld",
    });

    if driver != "clang" {
        let mut clang_lld_args = inputs.clone();
        clang_lld_args.push("-o".to_string());
        clang_lld_args.push(exe.clone());
        clang_lld_args.push("-fuse-ld=lld".to_string());
        attempts.push(LinkAttempt {
            program: "clang".to_string(),
            args: clang_lld_args,
            note: "clang+lld",
        });
    }

    for rust_lld in rust_lld_candidates() {
        let mut args = vec!["-flavor".to_string(), "gnu".to_string()];
        args.extend(inputs.clone());
        args.push("-o".to_string());
        args.push(exe.clone());
        attempts.push(LinkAttempt {
            program: rust_lld,
            args,
            note: "rust-lld-gnu",
        });
    }

    let mut lld_direct = inputs.clone();
    lld_direct.push("-o".to_string());
    lld_direct.push(exe.clone());
    attempts.push(LinkAttempt {
        program: "ld.lld".to_string(),
        args: lld_direct,
        note: "ld.lld-direct",
    });

    let mut driver_default = inputs;
    driver_default.push("-o".to_string());
    driver_default.push(exe);
    attempts.push(LinkAttempt {
        program: driver,
        args: driver_default,
        note: "driver-default",
    });

    run_link_attempts(attempts)
}

fn link_windows(link_inputs: &[PathBuf], exe_path: &Path, bootstrap_dustlink: bool) -> Result<()> {
    if link_inputs.is_empty() {
        bail!("link_windows called without input objects/libraries");
    }
    let inputs = link_inputs
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect::<Vec<_>>();
    let exe = exe_path.to_string_lossy().to_string();
    let native_libs = windows_host_runtime_native_libs();

    let mut attempts: Vec<LinkAttempt> = Vec::new();

    if !bootstrap_dustlink {
        let mut args = inputs.clone();
        args.push("-o".to_string());
        args.push(exe.clone());
        attempts.push(LinkAttempt {
            program: "dustlink".to_string(),
            args,
            note: "dustlink-preferred",
        });
    }

    let driver = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());

    let mut driver_lld_args = inputs.clone();
    driver_lld_args.push("-o".to_string());
    driver_lld_args.push(exe.clone());
    driver_lld_args.push("-fuse-ld=lld".to_string());
    attempts.push(LinkAttempt {
        program: driver.clone(),
        args: driver_lld_args,
        note: "driver+lld",
    });

    let mut clang_lld_args = inputs.clone();
    clang_lld_args.push("-o".to_string());
    clang_lld_args.push(exe.clone());
    clang_lld_args.push("-fuse-ld=lld".to_string());
    attempts.push(LinkAttempt {
        program: "clang".to_string(),
        args: clang_lld_args,
        note: "clang+lld",
    });

    let mut lld_link_args = vec![
        "/ENTRY:main".to_string(),
        "/SUBSYSTEM:CONSOLE".to_string(),
        format!("/OUT:{}", exe.clone()),
    ];
    lld_link_args.extend(inputs.clone());
    lld_link_args.extend(native_libs.clone());
    attempts.push(LinkAttempt {
        program: "lld-link".to_string(),
        args: lld_link_args,
        note: "lld-link",
    });

    let mut ld_lld_link_args = vec![
        "-flavor".to_string(),
        "link".to_string(),
        "/ENTRY:main".to_string(),
        "/SUBSYSTEM:CONSOLE".to_string(),
        format!("/OUT:{}", exe.clone()),
    ];
    ld_lld_link_args.extend(inputs.clone());
    ld_lld_link_args.extend(native_libs.clone());
    attempts.push(LinkAttempt {
        program: "ld.lld".to_string(),
        args: ld_lld_link_args,
        note: "ld.lld-link-flavor",
    });

    for rust_lld in rust_lld_candidates() {
        let mut args = vec![
            "-flavor".to_string(),
            "link".to_string(),
            "/ENTRY:main".to_string(),
            "/SUBSYSTEM:CONSOLE".to_string(),
            format!("/OUT:{}", exe.clone()),
        ];
        args.extend(inputs.clone());
        args.extend(native_libs.clone());
        attempts.push(LinkAttempt {
            program: rust_lld,
            args,
            note: "rust-lld-link",
        });
    }

    let mut driver_default = inputs;
    driver_default.push("-o".to_string());
    driver_default.push(exe);
    attempts.push(LinkAttempt {
        program: driver,
        args: driver_default,
        note: "driver-default",
    });

    run_link_attempts(attempts)
}

fn windows_host_runtime_native_libs() -> Vec<String> {
    vec![
        "kernel32.lib".to_string(),
        "ntdll.lib".to_string(),
        "userenv.lib".to_string(),
        "ws2_32.lib".to_string(),
        "dbghelp.lib".to_string(),
        "/defaultlib:msvcrt".to_string(),
        "/defaultlib:vcruntime".to_string(),
        "/defaultlib:ucrt".to_string(),
        "/defaultlib:legacy_stdio_definitions".to_string(),
    ]
}

fn rust_lld_candidates() -> Vec<String> {
    let mut out = vec!["rust-lld".to_string()];
    if let Some(path) = rust_lld_from_sysroot() {
        let candidate = path.to_string_lossy().to_string();
        if !out.iter().any(|existing| existing == &candidate) {
            out.push(candidate);
        }
    }
    out
}

fn rust_lld_from_sysroot() -> Option<PathBuf> {
    let sysroot = rustc_cmd_output(&["--print", "sysroot"])?;
    let host = rustc_host_triple()?;

    let mut candidates = vec![
        PathBuf::from(&sysroot).join(format!("lib/rustlib/{}/bin/rust-lld", host)),
        PathBuf::from(&sysroot).join("lib/rustlib/bin/rust-lld"),
    ];

    if cfg!(windows) {
        candidates = candidates
            .into_iter()
            .flat_map(|p| vec![p.with_extension("exe"), p])
            .collect();
    }

    for path in candidates {
        if path.is_file() {
            return Some(path);
        }
    }

    None
}

fn rustc_host_triple() -> Option<String> {
    let version = rustc_cmd_output(&["-vV"])?;
    for line in version.lines() {
        if let Some(rest) = line.strip_prefix("host: ") {
            return Some(rest.trim().to_string());
        }
    }
    None
}

fn rustc_cmd_output(args: &[&str]) -> Option<String> {
    let out = Command::new("rustc").args(args).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if s.is_empty() {
        None
    } else {
        Some(s)
    }
}

fn run_link_attempts(attempts: Vec<LinkAttempt>) -> Result<()> {
    let mut errors: Vec<String> = Vec::new();

    for attempt in attempts {
        let mut cmd = Command::new(&attempt.program);
        cmd.args(&attempt.args);

        match cmd.output() {
            Ok(out) => {
                if out.status.success() {
                    return Ok(());
                }
                errors.push(format!(
                    "{} [{}] failed with {}\\nstdout:\\n{}\\nstderr:\\n{}",
                    attempt.program,
                    attempt.note,
                    out.status,
                    String::from_utf8_lossy(&out.stdout),
                    String::from_utf8_lossy(&out.stderr)
                ));
            }
            Err(err) => {
                errors.push(format!(
                    "{} [{}] not available: {}",
                    attempt.program, attempt.note, err
                ));
            }
        }
    }

    bail!(
        "link failed after {} attempt(s)\\n{}",
        errors.len(),
        errors.join("\\n\\n")
    );
}

pub fn build_object_file(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    let entry =
        find_k_entry_scoped(dir, "main").context("DIR does not contain a codegen-capable K::main")?;
    let emit_strings = extract_emit_strings_scoped(dir, &entry, true)?;
    let obj_bytes = build_object_for_entry(&emit_strings, &Triple::host(), "main", false)?;

    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, obj_bytes).with_context(|| format!("write object {:?}", obj_path))?;
    Ok(obj_path)
}

pub fn build_object_file_for_target(
    dir: &DirProgram,
    out_path: &Path,
    target_triple: &str,
) -> Result<PathBuf> {
    let entry =
        find_k_entry_scoped(dir, "main").context("DIR does not contain a codegen-capable K::main")?;
    let emit_strings = extract_emit_strings_scoped(dir, &entry, true)?;
    let triple: Triple = target_triple
        .parse()
        .map_err(|e| anyhow!("invalid target triple '{}': {}", target_triple, e))?;
    let bare = target_triple.contains("none");
    let obj_bytes = build_object_for_entry(&emit_strings, &triple, "main", bare)?;

    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, obj_bytes).with_context(|| format!("write object {:?}", obj_path))?;
    Ok(obj_path)
}

pub fn build_bare_metal_kernel(dir: &DirProgram, out_path: &Path) -> Result<PathBuf> {
    let entry =
        find_k_entry_scoped(dir, "main").context("DIR does not contain a codegen-capable K::main")?;
    let emit_strings = extract_emit_strings_scoped(dir, &entry, true)?;
    let bin = build_flat_binary(&emit_strings)?;
    fs::write(out_path, bin).with_context(|| format!("write kernel {:?}", out_path))?;
    Ok(out_path.to_path_buf())
}

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

pub fn build_kernel_entry_object(
    dir: &DirProgram,
    out_path: &Path,
    entry_point: &str,
    target_triple: &str,
    allow_unresolved_calls: bool,
) -> Result<PathBuf> {
    let entry = find_k_entry_scoped(dir, entry_point).context("kernel entry point not found")?;
    let emit_strings = extract_emit_strings_scoped(dir, &entry, allow_unresolved_calls)?;
    let triple: Triple = target_triple
        .parse()
        .map_err(|e| anyhow!("invalid target triple '{}': {}", target_triple, e))?;
    let obj_bytes = build_object_for_entry(&emit_strings, &triple, &entry.proc.name, true)?;

    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, obj_bytes).with_context(|| format!("write object {:?}", obj_path))?;
    Ok(obj_path)
}

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
        .context("define bootstrap function")?;
    module.clear_context(&mut ctx);

    let product = module.finish();
    let obj_path = normalize_obj_path(out_path);
    fs::write(&obj_path, product.object.write().context("emit object")?)
        .with_context(|| format!("write object {:?}", obj_path))?;
    Ok(obj_path)
}

fn normalize_obj_path(out_path: &Path) -> PathBuf {
    let mut p = out_path.to_path_buf();
    if p.extension().is_none() {
        p.set_extension("o");
    }
    p
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

    match index.by_name.get(entry_point) {
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

fn extract_emit_strings_scoped(
    dir: &DirProgram,
    entry: &ScopedProcRef,
    allow_unresolved_calls: bool,
) -> Result<Vec<String>> {
    let proc_index = index_k_procs_scoped(dir);
    let mut out = Vec::new();
    let mut call_stack = Vec::new();
    collect_emit_strings_from_proc_scoped(
        entry,
        &proc_index,
        &mut call_stack,
        &mut out,
        allow_unresolved_calls,
    )?;
    Ok(out)
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

    collect_emit_strings_from_stmts_scoped(
        &proc_ref.proc.body,
        &proc_ref.forge,
        proc_index,
        call_stack,
        out,
        allow_unresolved_calls,
    )?;

    let _ = call_stack.pop();
    Ok(())
}

fn collect_emit_strings_from_stmts_scoped(
    stmts: &[DirStmt],
    current_forge: &str,
    proc_index: &ScopedProcIndex,
    call_stack: &mut Vec<String>,
    out: &mut Vec<String>,
    allow_unresolved_calls: bool,
) -> Result<()> {
    for stmt in stmts {
        match stmt {
            DirStmt::Effect { kind, payload } => {
                if kind == "emit" {
                    let decoded = decode_string_literal(payload).with_context(|| {
                        format!("emit payload must be a string literal, got: {}", payload)
                    })?;
                    out.push(decoded);
                    continue;
                }

                if kind == "expr" {
                    let (callee_text, args) = match parse_call_expr(payload) {
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
                    collect_call_target_scoped(
                        current_forge,
                        &callee_text,
                        &args,
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
            DirStmt::Call { target, args, .. } => {
                collect_call_target_scoped(
                    current_forge,
                    target,
                    args,
                    proc_index,
                    call_stack,
                    out,
                    allow_unresolved_calls,
                )?;
            }
            DirStmt::Return { expr } => {
                if let Some(payload) = expr {
                    let (callee_text, args) = match parse_call_expr(payload) {
                        Ok(v) => v,
                        Err(_) if allow_unresolved_calls => continue,
                        Err(e) => return Err(e),
                    };
                    collect_call_target_scoped(
                        current_forge,
                        &callee_text,
                        &args,
                        proc_index,
                        call_stack,
                        out,
                        allow_unresolved_calls,
                    )?;
                }
            }
            DirStmt::If {
                then_body,
                else_body,
                ..
            } => {
                collect_emit_strings_from_stmts_scoped(
                    then_body,
                    current_forge,
                    proc_index,
                    call_stack,
                    out,
                    allow_unresolved_calls,
                )?;
                if let Some(else_branch) = else_body {
                    collect_emit_strings_from_stmts_scoped(
                        else_branch,
                        current_forge,
                        proc_index,
                        call_stack,
                        out,
                        allow_unresolved_calls,
                    )?;
                }
            }
            DirStmt::While { body, .. } => {
                collect_emit_strings_from_stmts_scoped(
                    body,
                    current_forge,
                    proc_index,
                    call_stack,
                    out,
                    allow_unresolved_calls,
                )?;
            }
            DirStmt::For { body, .. } => {
                collect_emit_strings_from_stmts_scoped(
                    body,
                    current_forge,
                    proc_index,
                    call_stack,
                    out,
                    allow_unresolved_calls,
                )?;
            }
            _ if allow_unresolved_calls => {}
            other => bail!(
                "unsupported statement in codegen proc '{}': {:?}",
                current_forge,
                other
            ),
        }
    }

    Ok(())
}

fn collect_call_target_scoped(
    current_forge: &str,
    callee_text: &str,
    args: &[String],
    proc_index: &ScopedProcIndex,
    call_stack: &mut Vec<String>,
    out: &mut Vec<String>,
    allow_unresolved_calls: bool,
) -> Result<()> {
    if !args.is_empty() {
        if allow_unresolved_calls {
            return Ok(());
        }
        bail!(
            "unsupported call payload (arguments not supported in codegen path): {}({})",
            callee_text,
            args.join(", ")
        );
    }

    let callee = match resolve_callee_proc(proc_index, current_forge, callee_text) {
        Ok(v) => v,
        Err(_) if allow_unresolved_calls => return Ok(()),
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
    )
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
    if parent == "K" || parent == "Q" || parent == "Î¦" || parent == "Φ" || parent == "Phi" {
        if parts.len() >= 3 {
            let forge_name = parts[parts.len() - 3].to_string();
            return Some((forge_name, proc_name));
        }
        return None;
    }

    Some((parent.to_string(), proc_name))
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

    match proc_index.by_name.get(callee_text) {
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

fn build_object_for_entry(
    emit_strings: &[String],
    triple: &Triple,
    entry_symbol: &str,
    bare_metal: bool,
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

    let puts_func = if bare_metal || emit_strings.is_empty() {
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

    let mut entry_sig = Signature::new(module.isa().default_call_conv());
    entry_sig
        .returns
        .push(AbiParam::new(cranelift_codegen::ir::types::I32));
    let entry_func = module
        .declare_function(entry_symbol, Linkage::Export, &entry_sig)
        .with_context(|| format!("declare entry '{}'", entry_symbol))?;

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

    let mut ctx = module.make_context();
    ctx.func.signature = entry_sig;
    ctx.func.name = UserFuncName::user(0, entry_func.as_u32());

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
        .define_function(entry_func, &mut ctx)
        .context("define entry function")?;
    module.clear_context(&mut ctx);
    let product = module.finish();
    Ok(product.object.write().context("emit object")?)
}

const KERNEL_MAX_BYTES: usize = 128 * 512;
const VGA_TEXT_BASE: u32 = 0xB8000;
const VGA_COLS: u32 = 80;
const VGA_ROWS: u32 = 25;
const VGA_ROW_BYTES: u32 = VGA_COLS * 2;
const VGA_ATTR_DEFAULT: u8 = 0x0F;

fn build_flat_binary(emit_strings: &[String]) -> Result<Vec<u8>> {
    let mut kernel = vec![0xFA];
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
    out.push(0xC6);
    out.push(0x05);
    out.extend_from_slice(&addr.to_le_bytes());
    out.push(value);
}





