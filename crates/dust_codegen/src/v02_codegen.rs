// dust_codegen/src/v02_codegen.rs
//
// DPL v0.2 Code Generation
//
// This module provides code generation for the expanded K Regime features
// including variables, control flow, functions, and memory operations.

use cranelift_codegen::ir::*;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_module::{Linkage, Module};
use std::collections::HashMap;

// ─────────────────────────────────────────────────────────────────────────────
// Code Generator State
// ─────────────────────────────────────────────────────────────────────────────

pub struct Codegen {
    module: Module,
    functions: HashMap<String, FunctionId>,
    variables: HashMap<String, Variable>,
    variable_types: HashMap<String, Type>,
}

impl Codegen {
    pub fn new(module: Module) -> Self {
        Self {
            module,
            functions: HashMap::new(),
            variables: HashMap::new(),
            variable_types: HashMap::new(),
        }
    }

    // ─────────────────────────────────────────────────────────────────────
    // Variable Operations
    // ─────────────────────────────────────────────────────────────────────

    pub fn declare_variable(&mut self, name: &str, ty: Type) -> Variable {
        let var = Variable::new(self.variables.len());
        self.variables.insert(name.to_string(), var);
        self.variable_types.insert(name.to_string(), ty);
        var
    }

    pub fn get_variable(&self, name: &str) -> Option<Variable> {
        self.variables.get(name).copied()
    }

    pub fn get_variable_type(&self, name: &str) -> Option<Type> {
        self.variable_types.get(name).copied()
    }

    // ─────────────────────────────────────────────────────────────────────
    // Expression Code Generation
    // ─────────────────────────────────────────────────────────────────────

    pub fn gen_literal(&mut self, builder: &mut FunctionBuilder, value: &str) -> Value {
        // Parse literal and generate appropriate value
        if let Ok(i) = value.parse::<i64>() {
            builder.ins().iconst(types::I64, i)
        } else if let Ok(f) = value.parse::<f64>() {
            builder.ins().f64const(f)
        } else if value == "true" {
            builder.ins().iconst(types::I8, 1)
        } else if value == "false" {
            builder.ins().iconst(types::I8, 0)
        } else if value.starts_with('"') {
            // String literal - would need string runtime
            builder.ins().iconst(types::I64, 0)
        } else {
            // Assume variable reference
            if let Some(var) = self.get_variable(value) {
                builder.use_var(var)
            } else {
                builder.ins().iconst(types::I64, 0)
            }
        }
    }

    pub fn gen_binary_op(
        &mut self,
        builder: &mut FunctionBuilder,
        op: &str,
        lhs: Value,
        rhs: Value,
        result_type: Type,
    ) -> Value {
        match op {
            "+" => builder.ins().iadd(lhs, rhs),
            "-" => builder.ins().isub(lhs, rhs),
            "*" => builder.ins().imul(lhs, rhs),
            "/" => builder.ins().sdiv(lhs, rhs),
            "%" => builder.ins().srem(lhs, rhs),
            "==" => builder.ins().icmp(IntCC::Equal, lhs, rhs),
            "!=" => builder.ins().icmp(IntCC::NotEqual, lhs, rhs),
            "<" => builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs),
            "<=" => builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
            ">" => builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs),
            ">=" => builder
                .ins()
                .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
            "&&" => builder.ins().band(lhs, rhs),
            "||" => builder.ins().bor(lhs, rhs),
            _ => lhs,
        }
    }

    pub fn gen_unary_op(
        &mut self,
        builder: &mut FunctionBuilder,
        op: &str,
        operand: Value,
    ) -> Value {
        match op {
            "!" => builder.ins().bnot(operand),
            "-" => builder.ins().ineg(operand),
            _ => operand,
        }
    }

    // ─────────────────────────────────────────────────────────────────────
    // Control Flow Code Generation
    // ─────────────────────────────────────────────────────────────────────

    pub fn gen_if(
        &mut self,
        builder: &mut FunctionBuilder,
        condition: Value,
        then_block: Block,
        else_block: Option<Block>,
    ) {
        builder.ins().brif(
            condition,
            then_block,
            else_block
                .map(|b| b)
                .unwrap_or_else(|| builder.create_block()),
        );
    }

    pub fn gen_while_loop(
        &mut self,
        builder: &mut FunctionBuilder,
        header_block: Block,
        body_block: Block,
        exit_block: Block,
    ) {
        // Jump to header
        builder.ins().jump(header_block, &[]);

        // Set up the header block
        builder.switch_to_block(header_block);

        // (Condition check would be inserted here)

        // Set up body block
        builder.switch_to_block(body_block);

        // (Loop body would be inserted here)

        // Jump back to header
        builder.ins().jump(header_block, &[]);

        // Set up exit block
        builder.switch_to_block(exit_block);
    }

    pub fn gen_for_loop(
        &mut self,
        builder: &mut FunctionBuilder,
        var: &str,
        start: i64,
        end: i64,
        body_block: Block,
        exit_block: Block,
    ) {
        // Initialize counter
        let counter_var = self.declare_variable(var, types::I64);
        builder.def_var(counter_var, builder.ins().iconst(types::I64, start));

        // Create header block for loop condition
        let header_block = builder.create_block();

        // Create exit block
        let exit = exit_block;

        // Jump to header
        builder.ins().jump(header_block, &[]);

        // Set up header block
        builder.switch_to_block(header_block);

        // Check condition: counter < end
        let counter = builder.use_var(counter_var);
        let end_val = builder.ins().iconst(types::I64, end);
        let cond = builder.ins().icmp(IntCC::SignedLessThan, counter, end_val);

        // Branch to body or exit
        builder.ins().brif(cond, body_block, exit);

        // Set up body block
        builder.switch_to_block(body_block);

        // (Loop body would be inserted here)

        // Increment counter
        let counter = builder.use_var(counter_var);
        let next = builder
            .ins()
            .iadd(counter, builder.ins().iconst(types::I64, 1));
        builder.def_var(counter_var, next);

        // Jump back to header
        builder.ins().jump(header_block, &[]);

        // Set up exit block
        builder.switch_to_block(exit);
    }

    // ─────────────────────────────────────────────────────────────────────
    // Function Code Generation
    // ─────────────────────────────────────────────────────────────────────

    pub fn gen_function_call(
        &mut self,
        builder: &mut FunctionBuilder,
        func_name: &str,
        args: Vec<Value>,
    ) -> Option<Value> {
        if let Some(func_id) = self.functions.get(func_name) {
            let call = builder.ins().call(*func_id, &args);
            Some(builder.inst_results(call)[0])
        } else {
            // External function call - generate external reference
            let signature = builder.import_signature(AbiParams::new(vec![]));
            let local_func = builder.create_sized_stack_function(
                signature,
                ExternalName::libcall(func_name.to_string()),
            );
            let call = builder.ins().call(local_func, &args);
            builder.inst_results(call).first().copied()
        }
    }

    // ─────────────────────────────────────────────────────────────────────
    // Memory Operations
    // ─────────────────────────────────────────────────────────────────────

    pub fn gen_alloc(&mut self, builder: &mut FunctionBuilder, size: Value) -> Value {
        // Allocate memory on heap
        // This would call the runtime heap_alloc function
        let heap_alloc_sig = builder.import_signature(vec![AbiParam::new(types::I64)].into());
        let heap_alloc = builder.create_sized_stack_function(
            heap_alloc_sig,
            ExternalName::libcall("heap_alloc".to_string()),
        );
        builder.ins().call(heap_alloc, &[size])
    }

    pub fn gen_dealloc(&mut self, builder: &mut FunctionBuilder, ptr: Value) {
        // Free heap memory
        let heap_free_sig = builder.import_signature(vec![AbiParam::new(types::I64)].into());
        let heap_free = builder.create_sized_stack_function(
            heap_free_sig,
            ExternalName::libcall("heap_free".to_string()),
        );
        builder.ins().call(heap_free, &[ptr]);
    }

    pub fn gen_load(&mut self, builder: &mut FunctionBuilder, ptr: Value, ty: Type) -> Value {
        builder.ins().load(ty, MemFlags::new(), ptr, 0)
    }

    pub fn gen_store(&mut self, builder: &mut FunctionBuilder, ptr: Value, value: Value) {
        builder.ins().store(MemFlags::new(), value, ptr, 0);
    }

    // ─────────────────────────────────────────────────────────────────────
    // Array/Index Operations
    // ─────────────────────────────────────────────────────────────────────

    pub fn gen_index(
        &mut self,
        builder: &mut FunctionBuilder,
        base_ptr: Value,
        index: Value,
        element_size: i32,
    ) -> Value {
        // Calculate offset: base + index * element_size
        let offset = builder
            .ins()
            .imul(index, builder.ins().iconst(types::I64, element_size as i64));
        let elem_ptr = builder.ins().iadd(base_ptr, offset);
        elem_ptr
    }

    // ─────────────────────────────────────────────────────────────────────
    // Return Operations
    // ─────────────────────────────────────────────────────────────────────

    pub fn gen_return(
        &mut self,
        builder: &mut FunctionBuilder,
        value: Option<Value>,
        exit_block: Block,
    ) {
        match value {
            Some(v) => builder.ins().return_(&[v]),
            None => builder.ins().return_(&[]),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Type Conversion Helpers
// ─────────────────────────────────────────────────────────────────────────────

pub fn dust_type_to_clif(ty: &str) -> Type {
    match ty {
        "i8" => types::I8,
        "i16" => types::I16,
        "i32" => types::I32,
        "i64" => types::I64,
        "u8" => types::I8,
        "u16" => types::I16,
        "u32" => types::I32,
        "u64" => types::I64,
        "f32" => types::F32,
        "f64" => types::F64,
        "bool" => types::I8,
        "char" => types::I32,
        _ => types::I64,
    }
}

pub fn clif_type_to_dust(ty: Type) -> String {
    match ty {
        types::I8 => "i8".to_string(),
        types::I16 => "i16".to_string(),
        types::I32 => "i32".to_string(),
        types::I64 => "i64".to_string(),
        types::F32 => "f32".to_string(),
        types::F64 => "f64".to_string(),
        _ => "i64".to_string(),
    }
}
