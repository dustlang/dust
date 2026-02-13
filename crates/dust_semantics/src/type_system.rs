// dust_semantics/src/type_system.rs
//
// DPL v0.2 Type System
//
// This module provides type inference, type checking, and symbol management
// for the Dust Programming Language.

use crate::ast::*;
use std::collections::HashMap;

// ─────────────────────────────────────────────────────────────────────────────
// Type Definitions
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Bool,
    Int8,
    Int16,
    Int32,
    Int64,
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Float32,
    Float64,
    Char,
    String,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Function(Box<Type>, Vec<Type>),
    UserDefined(String),
    Unknown,
}

impl Type {
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::Int8
                | Type::Int16
                | Type::Int32
                | Type::Int64
                | Type::UInt8
                | Type::UInt16
                | Type::UInt32
                | Type::UInt64
                | Type::Float32
                | Type::Float64
        )
    }

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Type::Int8
                | Type::Int16
                | Type::Int32
                | Type::Int64
                | Type::UInt8
                | Type::UInt16
                | Type::UInt32
                | Type::UInt64
        )
    }

    pub fn default_value(&self) -> String {
        match self {
            Type::Unit => "()".to_string(),
            Type::Bool => "false".to_string(),
            Type::Int8 | Type::Int16 | Type::Int32 | Type::Int64 => "0".to_string(),
            Type::UInt8 | Type::UInt16 | Type::UInt32 | Type::UInt64 => "0".to_string(),
            Type::Float32 | Type::Float64 => "0.0".to_string(),
            Type::Char => "'\\0'".to_string(),
            Type::String => "\"\"".to_string(),
            Type::Pointer(_) => "null".to_string(),
            _ => "0".to_string(),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Type Environment
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct TypeEnv {
    bindings: HashMap<String, Type>,
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: TypeEnv) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(Box::new(parent)),
        }
    }

    pub fn insert(&mut self, name: String, ty: Type) {
        self.bindings.insert(name, ty);
    }

    pub fn get(&self, name: &str) -> Option<Type> {
        self.bindings
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(name)))
    }

    pub fn contains(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
            || self
                .parent
                .as_ref()
                .map(|p| p.contains(name))
                .unwrap_or(false)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Type Errors
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub enum TypeError {
    UndefinedVariable(String),
    TypeMismatch(Type, Type),
    NotAFunction(String),
    NotAType(String),
    InvalidOperation(String),
    CannotInfer,
    Unknown,
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UndefinedVariable(name) => write!(f, "undefined variable: {}", name),
            TypeError::TypeMismatch(expected, got) => {
                write!(f, "type mismatch: expected {:?}, got {:?}", expected, got)
            }
            TypeError::NotAFunction(name) => write!(f, "not a function: {}", name),
            TypeError::NotAType(name) => write!(f, "not a type: {}", name),
            TypeError::InvalidOperation(msg) => write!(f, "invalid operation: {}", msg),
            TypeError::CannotInfer => write!(f, "cannot infer type"),
            TypeError::Unknown => write!(f, "unknown type error"),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Type Inference
// ─────────────────────────────────────────────────────────────────────────────

pub struct TypeInferrer {
    env: TypeEnv,
}

impl TypeInferrer {
    pub fn new() -> Self {
        let mut env = TypeEnv::new();

        // Built-in types
        env.insert("bool".to_string(), Type::UserDefined("bool".to_string()));
        env.insert("i8".to_string(), Type::Int8);
        env.insert("i16".to_string(), Type::Int16);
        env.insert("i32".to_string(), Type::Int32);
        env.insert("i64".to_string(), Type::Int64);
        env.insert("u8".to_string(), Type::UInt8);
        env.insert("u16".to_string(), Type::UInt16);
        env.insert("u32".to_string(), Type::UInt32);
        env.insert("u64".to_string(), Type::UInt64);
        env.insert("f32".to_string(), Type::Float32);
        env.insert("f64".to_string(), Type::Float64);
        env.insert("char".to_string(), Type::Char);
        env.insert("string".to_string(), Type::String);

        Self { env }
    }

    pub fn infer_expr(&mut self, expr: &Expr) -> Result<Type, TypeError> {
        match expr {
            Expr::Literal(lit) => self.infer_literal(lit),
            Expr::Ident(id) => self.infer_ident(&id.text),
            Expr::Binary(bin) => self.infer_binary(&bin),
            Expr::Unary(un) => self.infer_unary(&un),
            Expr::Call(call) => self.infer_call(call),
            Expr::Field(field) => self.infer_field(field),
            Expr::Index(idx) => self.infer_index(idx),
            Expr::Array(arr) => self.infer_array(arr),
            Expr::Block(block) => self.infer_block(block),
            Expr::Paren(expr) => self.infer_expr(&expr.node),
            Expr::StructLit(s) => self.infer_struct(s),
        }
    }

    fn infer_literal(&self, lit: &Literal) -> Result<Type, TypeError> {
        match lit {
            Literal::Int(_) => Ok(Type::Int64),
            Literal::Float(_) => Ok(Type::Float64),
            Literal::Bool(_) => Ok(Type::Bool),
            Literal::Char(_) => Ok(Type::Char),
            Literal::String(_) => Ok(Type::String),
        }
    }

    fn infer_ident(&self, name: &str) -> Result<Type, TypeError> {
        self.env
            .get(name)
            .ok_or_else(|| TypeError::UndefinedVariable(name.to_string()))
    }

    fn infer_binary(&mut self, bin: &Spanned<BinaryExpr>) -> Result<Type, TypeError> {
        let lhs_type = self.infer_expr(&bin.node.lhs.node)?;
        let rhs_type = self.infer_expr(&bin.node.rhs.node)?;

        match bin.node.op.node {
            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                if lhs_type.is_numeric() && rhs_type.is_numeric() {
                    // Return the wider type
                    if matches!(lhs_type, Type::Float64) || matches!(rhs_type, Type::Float64) {
                        Ok(Type::Float64)
                    } else if matches!(lhs_type, Type::Float32) || matches!(rhs_type, Type::Float32)
                    {
                        Ok(Type::Float32)
                    } else {
                        Ok(Type::Int64)
                    }
                } else {
                    Err(TypeError::InvalidOperation(
                        "numeric operation on non-numeric types".to_string(),
                    ))
                }
            }
            BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                if lhs_type.is_numeric() && rhs_type.is_numeric() {
                    Ok(Type::Bool)
                } else if lhs_type == rhs_type {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::TypeMismatch(lhs_type, rhs_type))
                }
            }
            BinOp::And | BinOp::Or => {
                if lhs_type == Type::Bool && rhs_type == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::TypeMismatch(Type::Bool, lhs_type))
                }
            }
        }
    }

    fn infer_unary(&mut self, un: &Spanned<UnaryExpr>) -> Result<Type, TypeError> {
        let operand_type = self.infer_expr(&un.node.operand.node)?;

        match un.node.op.node {
            UnOp::Not => {
                if operand_type == Type::Bool {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::TypeMismatch(Type::Bool, operand_type))
                }
            }
            UnOp::Neg => {
                if operand_type.is_numeric() {
                    Ok(operand_type)
                } else {
                    Err(TypeError::InvalidOperation(
                        "negation on non-numeric type".to_string(),
                    ))
                }
            }
        }
    }

    fn infer_call(&mut self, call: &Spanned<CallExpr>) -> Result<Type, TypeError> {
        let callee_type = self.infer_expr(&call.node.callee.node)?;

        if let Type::Function(ret, params) = callee_type {
            // Check argument count
            if params.len() != call.node.args.len() {
                return Err(TypeError::InvalidOperation(
                    "argument count mismatch".to_string(),
                ));
            }
            Ok(*ret)
        } else {
            Err(TypeError::NotAFunction(format!("{:?}", callee_type)))
        }
    }

    fn infer_field(&mut self, field: &Spanned<FieldExpr>) -> Result<Type, TypeError> {
        let base_type = self.infer_expr(&field.node.base.node)?;
        // For now, return Unknown for field access
        // Would need struct type information
        Ok(Type::Unknown)
    }

    fn infer_index(&mut self, idx: &Spanned<IndexExpr>) -> Result<Type, TypeError> {
        let base_type = self.infer_expr(&idx.node.base.node)?;

        if let Type::Array(elem, _) = base_type {
            Ok(*elem)
        } else if let Type::Pointer(elem) = base_type {
            Ok(*elem)
        } else {
            Err(TypeError::InvalidOperation(
                "index on non-array type".to_string(),
            ))
        }
    }

    fn infer_array(&mut self, arr: &[Spanned<Expr>]) -> Result<Type, TypeError> {
        if arr.is_empty() {
            return Ok(Type::Array(Box::new(Type::Unknown), 0));
        }

        // Infer type of first element
        let elem_type = self.infer_expr(&arr[0].node)?;

        // Verify all elements have same type
        for expr in arr.iter().skip(1) {
            let ty = self.infer_expr(&expr.node)?;
            if ty != elem_type {
                return Err(TypeError::TypeMismatch(elem_type.clone(), ty));
            }
        }

        Ok(Type::Array(Box::new(elem_type), arr.len()))
    }

    fn infer_block(&mut self, block: &Block) -> Result<Type, TypeError> {
        let mut block_env = TypeEnv::with_parent(self.env.clone());
        let old_env = std::mem::replace(&mut self.env, block_env);

        let mut result_type = Type::Unit;

        for stmt in &block.stmts {
            result_type = self.infer_stmt(stmt)?;
        }

        block_env = std::mem::replace(&mut self.env, old_env);
        Ok(result_type)
    }

    fn infer_struct(&self, s: &Spanned<StructLitExpr>) -> Result<Type, TypeError> {
        // Would need to look up the struct definition
        Ok(Type::UserDefined(s.node.ty_name.text.clone()))
    }

    pub fn infer_stmt(&mut self, stmt: &Spanned<Stmt>) -> Result<Type, TypeError> {
        match &stmt.node {
            Stmt::Let(let_stmt) => {
                let expr_type = self.infer_expr(&let_stmt.expr.node)?;
                self.env.insert(let_stmt.name.text.clone(), expr_type);
                Ok(Type::Unit)
            }
            Stmt::MutLet(let_stmt) => {
                let expr_type = self.infer_expr(&let_stmt.expr.node)?;
                self.env.insert(let_stmt.name.text.clone(), expr_type);
                Ok(Type::Unit)
            }
            Stmt::Expr(expr_stmt) => self.infer_expr(&expr_stmt.expr.node),
            Stmt::Return(ret) => self.infer_expr(&ret.expr.node),
            Stmt::Effect(eff) => self.infer_expr(&eff.payload.node),
            Stmt::If(if_stmt) => self.infer_if(if_stmt),
            Stmt::For(for_stmt) => self.infer_for(for_stmt),
            Stmt::While(while_stmt) => self.infer_while(while_stmt),
            Stmt::Break(_) => Ok(Type::Unit),
            Stmt::Continue(_) => Ok(Type::Unit),
            Stmt::Constrain(_) => Ok(Type::Unit),
            Stmt::Prove(_) => Ok(Type::Unit),
        }
    }

    fn infer_if(&mut self, if_stmt: &IfStmt) -> Result<Type, TypeError> {
        let cond_type = self.infer_expr(&if_stmt.condition.node)?;
        if cond_type != Type::Bool {
            return Err(TypeError::TypeMismatch(Type::Bool, cond_type));
        }

        let then_type = self.infer_expr(&if_stmt.then_block.node)?;

        if let Some(else_block) = &if_stmt.else_block {
            let else_type = self.infer_expr(&else_block.node)?;
            if then_type == else_type {
                Ok(then_type)
            } else {
                Ok(Type::Unit)
            }
        } else {
            Ok(Type::Unit)
        }
    }

    fn infer_for(&mut self, for_stmt: &ForStmt) -> Result<Type, TypeError> {
        // Add loop variable to environment
        self.env.insert(for_stmt.var.text.clone(), Type::Int64);

        let _ = self.infer_expr(&for_stmt.start.node)?;
        let _ = self.infer_expr(&for_stmt.end.node)?;

        let result = self.infer_expr(&for_stmt.body.node);

        // Remove loop variable
        self.env.bindings.remove(&for_stmt.var.text);

        result
    }

    fn infer_while(&mut self, while_stmt: &WhileStmt) -> Result<Type, TypeError> {
        let cond_type = self.infer_expr(&while_stmt.condition.node)?;
        if cond_type != Type::Bool {
            return Err(TypeError::TypeMismatch(Type::Bool, cond_type));
        }

        self.infer_expr(&while_stmt.body.node)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Type Checking
// ─────────────────────────────────────────────────────────────────────────────

pub struct TypeChecker {
    inferrer: TypeInferrer,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            inferrer: TypeInferrer::new(),
        }
    }

    pub fn check_file(&mut self, file: &FileAst) -> Result<(), TypeError> {
        for forge in &file.forges {
            for item in &forge.node.items {
                match &item.node {
                    Item::Proc(proc) => {
                        self.check_proc(proc)?;
                    }
                    Item::Shape(shape) => {
                        // Type checking for shapes
                    }
                    Item::Bind(_) => {
                        // Type checking for binds
                    }
                }
            }
        }
        Ok(())
    }

    fn check_proc(&mut self, proc: &ProcDecl) -> Result<(), TypeError> {
        // Infer and check the procedure body
        self.inferrer.infer_expr(&proc.body.node.into())?;
        Ok(())
    }
}
