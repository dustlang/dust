// crates/dust_frontend/src/ast.rs
//
// DPL v0.1 AST for the Dust frontend.
//
// This AST is intentionally shaped to match the *current* parser implementation in
// crates/dust_frontend/src/parser.rs (FileAst/ForgeDecl/items as Spanned<Item>,
// Expr variants used by the parser, etc).
//
// © 2026 Dust LLC

use std::fmt;

//
// ─────────────────────────────────────────────────────────────────────────────
// Source spans
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Spanned wrapper
// ─────────────────────────────────────────────────────────────────────────────
//

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

//
// ─────────────────────────────────────────────────────────────────────────────
// Identifiers
// ─────────────────────────────────────────────────────────────────────────────
//

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

//
// ─────────────────────────────────────────────────────────────────────────────
// Regimes
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Regime {
    K,
    Q,
    Phi,
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Top-level file + forge + items
// ─────────────────────────────────────────────────────────────────────────────
//

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

//
// ─────────────────────────────────────────────────────────────────────────────
// Declarations
// ─────────────────────────────────────────────────────────────────────────────
//

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

#[derive(Debug, Clone, PartialEq)]
pub struct ProcDecl {
    pub path: Spanned<ProcPath>,
    pub params: Vec<Spanned<ParamDecl>>,
    pub ret: Option<Spanned<TypeRef>>,
    pub contracts: Vec<Spanned<ContractDecl>>,
    pub body: Spanned<Block>,
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
pub struct BindDecl {
    pub name: Ident,
    pub target: Spanned<ProcPath>,
    pub args: Vec<Spanned<NamedArg>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedArg {
    pub key: Ident,
    pub value: Spanned<Literal>,
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Contracts
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub enum ContractDecl {
    Requires(Spanned<Expr>),
    Ensures(Spanned<Expr>),
    Uses(Ident),
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Blocks + statements
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Let(LetStmt),
    Constrain(Spanned<Expr>),
    Prove(Spanned<Expr>),
    Emit(Spanned<Expr>),
    Observe(Spanned<Expr>),
    Seal(Spanned<Expr>),
    Return(Option<Spanned<Expr>>),
    If(IfStmt),
    Expr(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub name: Ident,
    pub ty: Option<Spanned<TypeRef>>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub cond: Spanned<Expr>,
    pub then_block: Spanned<Block>,
    pub else_block: Option<Spanned<Block>>,
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Types
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub enum TypeRef {
    Named(Ident),
    Unit,
    Bool,
    I64,
    U64,
    F64,
    String,
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Literals
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Unit,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
    String(String),
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Expressions (must match parser.rs usage)
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    Binary(Box<BinaryExpr>),
    Field(Box<FieldExpr>),
    Paren(Box<Spanned<Expr>>),
    Block(Block),
    Call(Box<CallExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpr {
    pub op: BinOp,
    pub lhs: Spanned<Expr>,
    pub rhs: Spanned<Expr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct FieldExpr {
    pub base: Spanned<Expr>,
    pub field: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Spanned<Expr>,
    pub args: Vec<Spanned<Expr>>,
}