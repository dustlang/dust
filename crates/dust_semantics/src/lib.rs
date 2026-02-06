// Semantic analysis: regimes, types, effects, constraints, contracts

use dust_dir::*;
use dust_frontend::ast::*;
use dust_frontend::{Lexer, Parser};

#[derive(Debug)]
pub struct CheckError {
    pub message: String,
    pub span: Span,
}

pub fn parse_and_check(source: &str) -> Result<FileAst, CheckError> {
    let toks = Lexer::new(source).lex_all().map_err(|e| CheckError {
        message: format!("lex error: {:?}", e.kind),
        span: e.span,
    })?;

    let mut p = Parser::new(toks);
    let file = p.parse_file().map_err(|e| CheckError {
        message: format!("parse error: {}", e.message),
        span: e.span,
    })?;

    // Minimal structural checks (enough for reference examples)
    check_file(&file)?;

    Ok(file)
}

fn check_file(file: &FileAst) -> Result<(), CheckError> {
    for f in &file.forges {
        // forge must have a name
        if f.node.name.text.is_empty() {
            return Err(CheckError { message: "forge name empty".into(), span: f.span });
        }

        // bind must have contract clauses with (key op value)
        for item in &f.node.items {
            if let Item::Bind(b) = &item.node {
                for c in &b.contract.node.clauses {
                    if c.node.key.text.is_empty() {
                        return Err(CheckError { message: "empty contract key".into(), span: c.span });
                    }
                }
            }
        }

        // Q procs should declare linear qualifier in v0.1 examples (enforce for now)
        for item in &f.node.items {
            if let Item::Proc(p) = &item.node {
                if matches!(p.sig.node.path.node.regime.node, Regime::Q) {
                    let has_linear = p.sig.node.qualifiers.iter().any(|q| matches!(q.node, ProcQualifier::Linear));
                    if !has_linear {
                        return Err(CheckError {
                            message: "Q-regime proc must be declared linear in v0.1".into(),
                            span: p.sig.span,
                        });
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn lower_to_dir(file: &FileAst) -> DirProgram {
    let mut forges = Vec::new();

    for f in &file.forges {
        let mut shapes = Vec::new();
        let mut procs = Vec::new();
        let mut binds = Vec::new();

        for item in &f.node.items {
            match &item.node {
                Item::Shape(s) => {
                    shapes.push(DirShape {
                        name: s.name.text.clone(),
                        fields: s.fields.iter().map(|fld| DirField {
                            name: fld.node.name.text.clone(),
                            ty: type_to_string(&fld.node.ty.node),
                        }).collect(),
                    });
                }
                Item::Proc(p) => {
                    procs.push(lower_proc(p));
                }
                Item::Bind(b) => {
                    binds.push(DirBind {
                        source: proc_ref_to_string(&b.source.node),
                        target: proc_ref_to_string(&b.target.node),
                        contract: b.contract.node.clauses.iter().map(|cl| DirClause {
                            key: cl.node.key.text.clone(),
                            op: contract_op_to_string(&cl.node.op.node),
                            value: contract_value_to_string(&cl.node.value.node),
                        }).collect(),
                    });
                }
            }
        }

        // stable ordering for deterministic output
        shapes.sort_by(|a, b| a.name.cmp(&b.name));
        procs.sort_by(|a, b| (a.regime.clone(), a.name.clone()).cmp(&(b.regime.clone(), b.name.clone())));
        binds.sort_by(|a, b| (a.source.clone(), a.target.clone()).cmp(&(b.source.clone(), b.target.clone())));

        forges.push(DirForge {
            name: f.node.name.text.clone(),
            shapes,
            procs,
            binds,
        });
    }

    forges.sort_by(|a, b| a.name.cmp(&b.name));
    DirProgram { forges }
}

fn lower_proc(p: &ProcDecl) -> DirProc {
    let regime = match p.sig.node.path.node.regime.node {
        Regime::K => "K",
        Regime::Q => "Q",
        Regime::Phi => "Φ",
    }.to_string();

    let name = p.sig.node.path.node.name.text.clone();

    let params = p.sig.node.params.iter().map(|pr| DirParam {
        name: pr.node.name.text.clone(),
        ty: type_to_string(&pr.node.ty.node),
    }).collect();

    let uses = p.sig.node.uses.iter().map(|u| DirUses {
        resource: u.node.resource.text.clone(),
        args: u.node.args.iter().map(|a| {
            let k = a.node.key.text.clone();
            let v = match &a.node.value.node {
                Literal::Int(n) => DirLit::Int(*n),
                Literal::Bool(b) => DirLit::Bool(*b),
                Literal::String(s) => DirLit::String(s.clone()),
            };
            (k, v)
        }).collect(),
    }).collect();

    let ret = p.sig.node.ret.as_ref().map(|t| type_to_string(&t.node));
    let qualifiers = p.sig.node.qualifiers.iter().map(|q| match q.node {
        ProcQualifier::Linear => "linear".to_string()
    }).collect();

    let body = p.body.node.stmts.iter().map(|s| lower_stmt(&s.node)).collect();

    DirProc { regime, name, params, uses, ret, qualifiers, body }
}

fn lower_stmt(s: &Stmt) -> DirStmt {
    match s {
        Stmt::Let(x) => DirStmt::Let { name: x.name.text.clone(), expr: expr_to_string(&x.expr.node) },
        Stmt::Constrain(x) => DirStmt::Constrain { predicate: expr_to_string(&x.predicate.node) },
        Stmt::Prove(x) => DirStmt::Prove { name: x.name.text.clone(), from: expr_to_string(&x.from.node) },
        Stmt::Effect(x) => DirStmt::Effect {
            kind: match x.kind.node { EffectKind::Observe => "observe", EffectKind::Emit => "emit", EffectKind::Seal => "seal" }.into(),
            payload: expr_to_string(&x.payload.node),
        },
        Stmt::Return(x) => DirStmt::Return { expr: expr_to_string(&x.expr.node) },
    }
}

fn type_to_string(t: &TypeRef) -> String {
    match t {
        TypeRef::Primitive(p) => format!("{:?}", p),
        TypeRef::Named(id) => id.text.clone(),
        TypeRef::Array { elem, len, .. } => format!("[{}; {}]", type_to_string(&elem.node), len),
    }
}

fn expr_to_string(e: &Expr) -> String {
    match e {
        Expr::Literal(Literal::Int(n)) => n.to_string(),
        Expr::Literal(Literal::Bool(b)) => b.to_string(),
        Expr::Literal(Literal::String(s)) => format!("{:?}", s),
        Expr::Ident(id) => id.text.clone(),
        Expr::Paren(inner) => format!("({})", expr_to_string(&inner.node)),
        Expr::Binary(b) => format!("{} {:?} {}", expr_to_string(&b.node.lhs.node), b.node.op.node, expr_to_string(&b.node.rhs.node)),
        Expr::Call(c) => {
            let args = c.node.args.iter().map(|a| expr_to_string(&a.node)).collect::<Vec<_>>().join(", ");
            format!("{}({})", expr_to_string(&c.node.callee.node), args)
        }
        Expr::Field(f) => format!("{}.{}", expr_to_string(&f.node.base.node), f.node.field.text),
        Expr::StructLit(s) => {
            let inits = s.node.inits.iter().map(|fi| format!("{}: {}", fi.node.name.text, expr_to_string(&fi.node.expr.node))).collect::<Vec<_>>().join(", ");
            format!("{}{{{}}}", s.node.ty_name.text, inits)
        }
    }
}

fn proc_ref_to_string(r: &ProcPathRef) -> String {
    match r {
        ProcPathRef::Unqualified(id) => id.text.clone(),
        ProcPathRef::Qualified(p) => {
            let reg = match p.regime.node { Regime::K => "K", Regime::Q => "Q", Regime::Phi => "Φ" };
            format!("{}::{}", reg, p.name.text)
        }
    }
}

fn contract_op_to_string(op: &ContractOp) -> String {
    match op {
        ContractOp::EqEq => "==",
        ContractOp::Lt => "<",
        ContractOp::Lte => "<=",
        ContractOp::Gt => ">",
        ContractOp::Gte => ">=",
    }.into()
}

fn contract_value_to_string(v: &ContractValue) -> String {
    match v {
        ContractValue::Ident(id) => id.text.clone(),
        ContractValue::Literal(Literal::Int(n)) => n.to_string(),
        ContractValue::Literal(Literal::Bool(b)) => b.to_string(),
        ContractValue::Literal(Literal::String(s)) => format!("{:?}", s),
    }
}