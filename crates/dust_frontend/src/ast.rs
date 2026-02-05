// crates/dust_frontend/src/ast.rs
//
// DPL AST (v0.1) — purely syntactic structures with spans for diagnostics.
// This AST is designed to support:
//
// - forges containing shape/proc/bind items
// - explicit regimes (K, Q, Φ) in process paths
// - statements: let, constrain, prove, observe/emit/seal, return
// - minimal expressions: literals, identifiers, binary ops, calls, field access, struct literals
//
// NOTE: This file is intentionally conservative. It does not encode semantic rules;
// those belong in dust_semantics.

use std::fmt;

/// Source span in bytes (UTF-8) within a single source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

/// A node wrapper that carries a span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

/// Identifier (unqualified).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub text: String,
    pub span: Span,
}

impl Ident {
    pub fn new(text: impl Into<String>, span: Span) -> Self {
        Self {
            text: text.into(),
            span,
        }
    }
}

/// A complete parsed source file.
/// v0.1 assumes one top-level forge per file, but we keep Vec to allow future growth.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileAst {
    pub forges: Vec<Spanned<ForgeDecl>>,
}

/// `forge <Name> { ... }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForgeDecl {
    pub name: Ident,
    pub items: Vec<Spanned<Item>>,
}

/// Top-level forge item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Shape(ShapeDecl),
    Proc(ProcDecl),
    Bind(BindDecl),
}

/// `shape Name { field: Type; ... }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeDecl {
    pub name: Ident,
    pub fields: Vec<Spanned<FieldDecl>>,
}

/// `field_name: Type;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: Spanned<TypeRef>,
}

/// Process declaration (v0.1)
///
/// Example:
/// `proc K::hello_world() uses Console(device="stdout") -> i32 linear { ... }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcDecl {
    pub sig: Spanned<ProcSig>,
    pub body: Spanned<Block>,
}

/// Signature portion of a process.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcSig {
    pub keyword_span: Span, // span of `proc`
    pub path: Spanned<ProcPath>, // e.g. K::name
    pub params: Vec<Spanned<Param>>,
    pub uses: Vec<Spanned<UsesClause>>,
    pub ret: Option<Spanned<TypeRef>>,
    pub qualifiers: Vec<Spanned<ProcQualifier>>,
}

/// `K::foo`, `Q::bar`, `Φ::baz`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcPath {
    pub regime: Spanned<Regime>,
    pub name: Ident,
}

/// Regime tag
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regime {
    K,
    Q,
    Phi, // represents `Φ`
}

impl fmt::Display for Regime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Regime::K => write!(f, "K"),
            Regime::Q => write!(f, "Q"),
            Regime::Phi => write!(f, "Φ"),
        }
    }
}

/// Parameter: `name: Type`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Ident,
    pub ty: Spanned<TypeRef>,
}

/// `uses ResourceName(key=value, ...)`
/// v0.1 examples use simple string/bool/int literals for attributes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UsesClause {
    pub resource: Ident,
    pub args: Vec<Spanned<NamedArg>>,
}

/// `key = <literal>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedArg {
    pub key: Ident,
    pub value: Spanned<Literal>,
}

/// Process qualifiers (v0.1): currently only `linear` appears in examples/spec.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcQualifier {
    Linear,
}

/// Binding declaration:
///
/// `bind <source_proc> -> <target_proc> contract { clause; ... }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindDecl {
    pub source: Spanned<ProcPathRef>,
    pub arrow_span: Span, // span of `->`
    pub target: Spanned<ProcPathRef>,
    pub contract: Spanned<ContractBlock>,
}

/// Process path reference in bindings.
///
/// We allow either fully-qualified `K::name` or unqualified `name` in v0.1 parsing,
/// but semantics should enforce clarity as desired.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcPathRef {
    Qualified(ProcPath),
    Unqualified(Ident),
}

/// `contract { clause; ... }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractBlock {
    pub clauses: Vec<Spanned<ContractClause>>,
}

/// Contract clause: `ident op value;`
///
/// v0.1 uses:
/// - `latency_us < 5000;`
/// - `witness_transfer == true;`
/// - `effects_allowed == true;`
/// - `linearity_enforced == true;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContractClause {
    pub key: Ident,
    pub op: Spanned<ContractOp>,
    pub value: Spanned<ContractValue>,
}

/// Contract operators.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContractOp {
    EqEq, // ==
    Lt,   // <
    Lte,  // <=
    Gt,   // >
    Gte,  // >=
}

/// Values allowed in contracts (v0.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ContractValue {
    Literal(Literal),
    Ident(Ident),
}

/// A block: `{ stmt* }`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

/// Statements (v0.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Let(LetStmt),
    Constrain(ConstrainStmt),
    Prove(ProveStmt),
    Effect(EffectStmt),
    Return(ReturnStmt),
}

/// `let name = expr;`
///
/// (No mutability in v0.1.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStmt {
    pub name: Ident,
    pub expr: Spanned<Expr>,
}

/// `constrain expr;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstrainStmt {
    pub predicate: Spanned<Expr>,
}

/// `prove name from expr;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProveStmt {
    pub name: Ident,
    pub from: Spanned<Expr>,
}

/// `observe expr;` | `emit expr;` | `seal expr;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectStmt {
    pub kind: Spanned<EffectKind>,
    pub payload: Spanned<Expr>,
}

/// Effect kinds (v0.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectKind {
    Observe,
    Emit,
    Seal,
}

/// `return expr;`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ReturnStmt {
    pub expr: Spanned<Expr>,
}

/// Expressions (v0.1 minimal).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    Binary(Box<Spanned<BinaryExpr>>),
    Call(Box<Spanned<CallExpr>>),
    Field(Box<Spanned<FieldExpr>>),
    StructLit(Box<Spanned<StructLitExpr>>),
    Paren(Box<Spanned<Expr>>),
}

/// Binary expression: `lhs op rhs`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryExpr {
    pub lhs: Spanned<Expr>,
    pub op: Spanned<BinaryOp>,
    pub rhs: Spanned<Expr>,
}

/// Binary operators used in examples/spec (extendable).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    EqEq, // ==
    Lt,  // <
    Lte, // <=
    Gt,  // >
    Gte, // >=
    AndAnd, // &&
    OrOr,   // ||
}

/// Call expression: `callee(arg, ...)`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpr {
    pub callee: Spanned<Expr>, // Ident or qualified ref (parsed as Expr::Ident + semantics)
    pub args: Vec<Spanned<Expr>>,
}

/// Field access: `base.field`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldExpr {
    pub base: Spanned<Expr>,
    pub dot_span: Span,
    pub field: Ident,
}

/// Struct literal: `TypeName { field: expr, ... }`
///
/// (The spec examples use `Type { field: value }` with commas optional; parser decides.
/// AST stores Vec and spans; semantics validates field names.)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLitExpr {
    pub ty_name: Ident, // type name token
    pub inits: Vec<Spanned<FieldInit>>,
}

/// `field: expr`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldInit {
    pub name: Ident,
    pub expr: Spanned<Expr>,
}

/// Literals (v0.1).
///
/// NOTE: Strings appear in examples; v0.1 spec may treat them as external payloads.
/// Keep them in AST to support examples and future formalization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Int(i64),
    Bool(bool),
    String(String),
}

/// Type references (v0.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRef {
    Primitive(PrimitiveType),
    Named(Ident),
    Array {
        elem: Box<Spanned<TypeRef>>,
        len: u32,       // v0.1: compile-time literal integer
        len_span: Span, // where the length appeared
    },
}

/// Primitive types (v0.1).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    I32,
    I64,
    F32,
    F64,
    Bool,
}