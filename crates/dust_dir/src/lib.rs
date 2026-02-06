// Dust Intermediate Representation (DIR)
// Canonical semantic form of DPL programs

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirProgram {
    pub forges: Vec<DirForge>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirForge {
    pub name: String,
    pub shapes: Vec<DirShape>,
    pub procs: Vec<DirProc>,
    pub binds: Vec<DirBind>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirShape {
    pub name: String,
    pub fields: Vec<DirField>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirField {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirProc {
    pub regime: String, // "K" | "Q" | "Φ"
    pub name: String,
    pub params: Vec<DirParam>,
    pub uses: Vec<DirUses>,
    pub ret: Option<String>,
    pub qualifiers: Vec<String>,
    pub body: Vec<DirStmt>, // v0.1: lowered statements only
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirParam {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirUses {
    pub resource: String,
    pub args: Vec<(String, DirLit)>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirLit {
    Int(i64),
    Bool(bool),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirStmt {
    Let { name: String, expr: String },
    Constrain { predicate: String },
    Prove { name: String, from: String },
    Effect { kind: String, payload: String },
    Return { expr: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirBind {
    pub source: String,
    pub target: String,
    pub contract: Vec<DirClause>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirClause {
    pub key: String,
    pub op: String,
    pub value: String,
}