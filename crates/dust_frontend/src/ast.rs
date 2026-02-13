// crates/dust_frontend/src/ast.rs
//
// DPL v0.1 — Abstract Syntax Tree
//
// This AST is the shared contract between the frontend (lexer/parser) and the
// semantic/lowering passes.

use std::fmt;

// ─────────────────────────────────────────────────────────────────────────────
// Spans
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self { start: 0, end: 0 }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Identifiers
// ─────────────────────────────────────────────────────────────────────────────

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

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.text.fmt(f)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Regimes
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Regime {
    K,
    Q,
    Phi,
}

// ─────────────────────────────────────────────────────────────────────────────
// Program structure
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct FileAst {
    pub forges: Vec<Spanned<ForgeDecl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForgeDecl {
    pub name: Ident,
    pub items: Vec<Spanned<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Shape(ShapeDecl),
    Proc(ProcDecl),
    Bind(BindDecl),
}

// ─────────────────────────────────────────────────────────────────────────────
// Shapes
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct ShapeDecl {
    pub name: Ident,
    pub fields: Vec<Spanned<FieldDecl>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDecl {
    pub name: Ident,
    pub ty: Spanned<TypeRef>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Processes
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct ProcDecl {
    pub sig: Spanned<ProcSig>,
    pub body: Spanned<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcSig {
    pub path: Spanned<ProcPath>,
    pub params: Vec<Spanned<ParamDecl>>,
    pub uses: Vec<Spanned<UsesClause>>,
    pub ret: Option<Spanned<TypeRef>>,
    pub qualifiers: Vec<Spanned<ProcQualifier>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcPath {
    pub regime: Spanned<Regime>,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParamDecl {
    pub name: Ident,
    pub ty: Spanned<TypeRef>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UsesClause {
    pub resource: Ident,
    pub args: Vec<Spanned<NamedArg>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedArg {
    pub key: Ident,
    pub value: Spanned<Literal>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ProcQualifier {
    Linear,
}

// ─────────────────────────────────────────────────────────────────────────────
// Bindings + contracts
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct BindDecl {
    pub source: Spanned<ProcPathRef>,
    pub target: Spanned<ProcPathRef>,
    pub contract: Spanned<ContractBlock>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProcPathRef {
    Qualified(ProcPath),
    Unqualified(Ident),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContractBlock {
    pub clauses: Vec<Spanned<ContractClause>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContractClause {
    pub key: Ident,
    pub op: Spanned<ContractOp>,
    pub value: Spanned<ContractValue>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ContractOp {
    EqEq,
    Lt,
    Lte,
    Gt,
    Gte,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContractValue {
    Ident(Ident),
    Literal(Literal),
}

// ─────────────────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PrimitiveType {
    Unit,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    String,
    Char,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    Primitive(PrimitiveType),
    Named(Ident),
    Array {
        elem: Box<Spanned<TypeRef>>,
        len: usize,
    },
}

// ─────────────────────────────────────────────────────────────────────────────
// Literals
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
}

// ─────────────────────────────────────────────────────────────────────────────
// Blocks + statements
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(LetStmt),
    MutLet(MutLetStmt),
    Constrain(ConstrainStmt),
    Prove(ProveStmt),
    Effect(EffectStmt),
    Return(ReturnStmt),
    If(IfStmt),
    For(ForStmt),
    While(WhileStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Expr(ExprStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub name: Ident,
    pub ty: Option<Spanned<TypeRef>>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MutLetStmt {
    pub name: Ident,
    pub ty: Option<Spanned<TypeRef>>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Spanned<Expr>,
    pub then_block: Spanned<Block>,
    pub else_block: Option<Spanned<Block>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub var: Ident,
    pub start: Spanned<Expr>,
    pub end: Spanned<Expr>,
    pub body: Spanned<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub condition: Spanned<Expr>,
    pub body: Spanned<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BreakStmt;

#[derive(Debug, Clone, PartialEq)]
pub struct ContinueStmt;

#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpr {
    pub expr: Spanned<Expr>,
    pub arms: Vec<Spanned<MatchArm>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Spanned<MatchPattern>,
    pub body: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchPattern {
    Literal(Literal),
    Ident(Ident),
    Wildcard,
    Or(Box<Spanned<MatchPattern>>, Box<Spanned<MatchPattern>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprStmt {
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstrainStmt {
    pub predicate: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProveStmt {
    pub name: Ident,
    pub from: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EffectStmt {
    pub kind: Spanned<EffectKind>,
    pub payload: Spanned<Expr>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EffectKind {
    Observe,
    Emit,
    Seal,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub expr: Spanned<Expr>,
}

// ─────────────────────────────────────────────────────────────────────────────
// Expressions
// ─────────────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    Paren(Box<Spanned<Expr>>),
    Block(Block),
    Binary(Box<Spanned<BinaryExpr>>),
    Unary(Box<Spanned<UnaryExpr>>),
    Call(Box<Spanned<CallExpr>>),
    Field(Box<Spanned<FieldExpr>>),
    Index(Box<Spanned<IndexExpr>>),
    Array(Vec<Spanned<Expr>>),
    StructLit(Box<Spanned<StructLitExpr>>),
    Match(Box<Spanned<MatchExpr>>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: Spanned<UnOp>,
    pub operand: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub base: Spanned<Expr>,
    pub index: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub op: Spanned<BinOp>,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Spanned<Expr>,
    pub args: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr {
    pub base: Spanned<Expr>,
    pub field: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructLitExpr {
    pub ty_name: Ident,
    pub inits: Vec<Spanned<FieldInit>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldInit {
    pub name: Ident,
    pub expr: Spanned<Expr>,
}
