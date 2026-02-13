// Dust Intermediate Representation (DIR)
// Canonical semantic form of DPL programs

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirProgram {
    pub forges: Vec<DirForge>,
    pub types: Vec<DirTypeDef>, // v0.2: type definitions
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirTypeDef {
    pub name: String,
    pub kind: DirTypeKind,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirTypeKind {
    Struct { fields: Vec<DirField> },
    Enum { variants: Vec<DirEnumVariant> },
    Alias { target: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirEnumVariant {
    pub name: String,
    pub fields: Vec<DirField>,
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
    pub body: Vec<DirStmt>,
    pub locals: Vec<DirLocal>, // v0.2: local variable declarations
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DirLocal {
    pub name: String,
    pub ty: String,
    pub is_mutable: bool,
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
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DirStmt {
    // Basic statements
    Let {
        name: String,
        expr: String,
    },
    Assign {
        target: String,
        expr: String,
    },
    Constrain {
        predicate: String,
    },
    Prove {
        name: String,
        from: String,
    },
    Effect {
        kind: String,
        payload: String,
    },
    Return {
        expr: Option<String>,
    },

    // Control flow
    If {
        condition: String,
        then_body: Vec<DirStmt>,
        else_body: Option<Vec<DirStmt>>,
    },
    While {
        condition: String,
        body: Vec<DirStmt>,
    },
    For {
        var: String,
        start: String,
        end: String,
        body: Vec<DirStmt>,
    },
    Break,
    Continue,

    // Function/Method calls
    Call {
        target: String,
        args: Vec<String>,
        result: Option<String>,
    },

    // Memory operations
    Alloc {
        size: String,
        result: String,
    },
    Dealloc {
        ptr: String,
    },
    Load {
        ptr: String,
        result: String,
    },
    Store {
        ptr: String,
        value: String,
    },

    // Index/Field access
    Index {
        target: String,
        index: String,
        result: String,
    },
    Field {
        target: String,
        field: String,
        result: String,
    },
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
