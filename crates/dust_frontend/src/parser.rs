// crates/dust_frontend/src/parser.rs
//
// DPL v0.1 parser (structure + expressions) following spec/03-grammar.md.

use crate::ast::*;
use crate::lexer::{Keyword, Token};
use std::fmt;

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}..{}", self.message, self.span.start, self.span.end)
    }
}

pub struct Parser {
    toks: Vec<Spanned<Token>>,
    i: usize,
    // scratch buffer used by some list parsers (kept to avoid repeated allocs)
    scratch_named_args: Vec<Spanned<NamedArg>>,
}

impl Parser {
    pub fn new(toks: Vec<Spanned<Token>>) -> Self {
        Self {
            toks,
            i: 0,
            scratch_named_args: Vec::new(),
        }
    }

    pub fn parse_file(&mut self) -> Result<FileAst, ParseError> {
        let mut forges = Vec::new();
        while !self.is_eof() {
            forges.push(self.parse_forge()?);
        }
        Ok(FileAst { forges })
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Forges + items
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_forge(&mut self) -> Result<Spanned<ForgeDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Forge)?.span.start;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut items = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            items.push(self.parse_item()?);
        }

        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(ForgeDecl { name, items }, Span::new(start, end)))
    }

    fn parse_item(&mut self) -> Result<Spanned<Item>, ParseError> {
        match &self.peek().node {
            Token::Keyword(Keyword::Shape) => {
                let s = self.parse_shape()?;
                Ok(Spanned::new(Item::Shape(s.node), s.span))
            }
            Token::Keyword(Keyword::Proc) => {
                let p = self.parse_proc()?;
                Ok(Spanned::new(Item::Proc(p.node), p.span))
            }
            Token::Keyword(Keyword::Bind) => {
                let b = self.parse_bind()?;
                Ok(Spanned::new(Item::Bind(b.node), b.span))
            }
            _ => Err(self.err_here("expected forge item (shape/proc/bind)")),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Shapes
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_shape(&mut self) -> Result<Spanned<ShapeDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Shape)?.span.start;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        let mut fields = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            let f_name = self.expect_ident()?;
            let f_start = f_name.span.start;
            self.expect(Token::Colon)?;
            let ty = self.parse_type_ref()?;
            self.expect(Token::Semi)?;
            let f_end = ty.span.end;
            fields.push(Spanned::new(
                FieldDecl { name: f_name, ty },
                Span::new(f_start, f_end),
            ));
        }

        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(ShapeDecl { name, fields }, Span::new(start, end)))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Procs
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_proc(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Proc)?.span.start;
        let path = self.parse_proc_path()?;

        // params
        self.expect(Token::LParen)?;
        let params = if self.peek_is(&Token::RParen) {
            Vec::new()
        } else {
            self.parse_param_list()?
        };
        self.expect(Token::RParen)?;

        // uses*
        let mut uses = Vec::new();
        while self.peek_kw(Keyword::Uses) {
            uses.push(self.parse_uses_clause()?);
        }

        // return_type?
        let ret = if self.peek_is(&Token::Arrow) {
            self.bump();
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        // qualifiers*
        let mut qualifiers = Vec::new();
        while self.peek_kw(Keyword::Linear) {
            let q_sp = self.bump().span;
            qualifiers.push(Spanned::new(ProcQualifier::Linear, q_sp));
        }

        let body = self.parse_block()?;
        let end = body.span.end;

        let sig_span = Span::new(start, end);
        let sig = Spanned::new(
            ProcSig {
                path,
                params,
                uses,
                ret,
                qualifiers,
            },
            sig_span,
        );

        Ok(Spanned::new(ProcDecl { sig, body }, Span::new(start, end)))
    }

    fn parse_proc_path(&mut self) -> Result<Spanned<ProcPath>, ParseError> {
        let reg = self.parse_regime()?;
        self.expect(Token::ColonColon)?;
        let name = self.expect_ident()?;
        let span = Span::new(reg.span.start, name.span.end);
        Ok(Spanned::new(ProcPath { regime: reg, name }, span))
    }

    fn parse_regime(&mut self) -> Result<Spanned<Regime>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Keyword(Keyword::K) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Regime::K, sp))
            }
            Token::Keyword(Keyword::Q) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Regime::Q, sp))
            }
            Token::Keyword(Keyword::Phi) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Regime::Phi, sp))
            }
            _ => Err(self.err_here("expected regime (K/Q/Φ)")),
        }
    }

    fn parse_param_list(&mut self) -> Result<Vec<Spanned<ParamDecl>>, ParseError> {
        let mut out = Vec::new();
        loop {
            let name = self.expect_ident()?;
            let start = name.span.start;
            self.expect(Token::Colon)?;
            let ty = self.parse_type_ref()?;
            let end = ty.span.end;
            out.push(Spanned::new(ParamDecl { name, ty }, Span::new(start, end)));

            if self.peek_is(&Token::Comma) {
                self.bump();
                continue;
            }
            break;
        }
        Ok(out)
    }

    fn parse_uses_clause(&mut self) -> Result<Spanned<UsesClause>, ParseError> {
        let start = self.expect_kw(Keyword::Uses)?.span.start;
        let resource = self.expect_ident()?;
        self.expect(Token::LParen)?;

        let args = if self.peek_is(&Token::RParen) {
            Vec::new()
        } else {
            self.parse_named_arg_list()?
        };

        self.expect(Token::RParen)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(UsesClause { resource, args }, Span::new(start, end)))
    }

    fn parse_named_arg_list(&mut self) -> Result<Vec<Spanned<NamedArg>>, ParseError> {
        self.scratch_named_args.clear();
        loop {
            let key = self.expect_ident()?;
            let start = key.span.start;
            self.expect(Token::Eq)?;
            let lit = self.parse_literal()?;
            let end = lit.span.end;
            self.scratch_named_args.push(Spanned::new(
                NamedArg { key, value: lit },
                Span::new(start, end),
            ));

            if self.peek_is(&Token::Comma) {
                self.bump();
                continue;
            }
            break;
        }
        Ok(std::mem::take(&mut self.scratch_named_args))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Bindings
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_bind(&mut self) -> Result<Spanned<BindDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Bind)?.span.start;
        let source = self.parse_proc_ref()?;
        self.expect(Token::Arrow)?;
        let target = self.parse_proc_ref()?;
        let contract = self.parse_contract_block()?;
        let end = contract.span.end;
        Ok(Spanned::new(
            BindDecl {
                source,
                target,
                contract,
            },
            Span::new(start, end),
        ))
    }

    fn parse_proc_ref(&mut self) -> Result<Spanned<ProcPathRef>, ParseError> {
        // Lookahead: if we have a regime keyword followed by ::, it's qualified.
        let t0 = self.peek().clone();
        let is_reg = matches!(
            t0.node,
            Token::Keyword(Keyword::K) | Token::Keyword(Keyword::Q) | Token::Keyword(Keyword::Phi)
        );
        if is_reg {
            let t1 = self.toks.get(self.i + 1);
            if matches!(t1.map(|x| &x.node), Some(Token::ColonColon)) {
                let p = self.parse_proc_path()?;
                let sp = p.span;
                return Ok(Spanned::new(ProcPathRef::Qualified(p.node), sp));
            }
        }

        let id = self.expect_ident()?;
        let sp = id.span;
        Ok(Spanned::new(ProcPathRef::Unqualified(id), sp))
    }

    fn parse_contract_block(&mut self) -> Result<Spanned<ContractBlock>, ParseError> {
        let start = self.expect_kw(Keyword::Contract)?.span.start;
        self.expect(Token::LBrace)?;
        let mut clauses = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            clauses.push(self.parse_contract_clause()?);
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(ContractBlock { clauses }, Span::new(start, end)))
    }

    fn parse_contract_clause(&mut self) -> Result<Spanned<ContractClause>, ParseError> {
        let key = self.expect_ident()?;
        let start = key.span.start;

        let (op, op_span) = match self.peek().node {
            Token::EqEq => (ContractOp::EqEq, self.bump().span),
            Token::Lt => (ContractOp::Lt, self.bump().span),
            Token::Lte => (ContractOp::Lte, self.bump().span),
            Token::Gt => (ContractOp::Gt, self.bump().span),
            Token::Gte => (ContractOp::Gte, self.bump().span),
            _ => return Err(self.err_here("expected contract operator (==, <, <=, >, >=)")),
        };
        let op = Spanned::new(op, op_span);

        let value = self.parse_contract_value()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;

        Ok(Spanned::new(ContractClause { key, op, value }, Span::new(start, end)))
    }

    fn parse_contract_value(&mut self) -> Result<Spanned<ContractValue>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Ident(_) => {
                let id = self.expect_ident()?;
                let sp = id.span;
                Ok(Spanned::new(ContractValue::Ident(id), sp))
            }
            Token::Int(_) | Token::String(_) | Token::Bool(_) => {
                let lit = self.parse_literal()?;
                let sp = lit.span;
                Ok(Spanned::new(ContractValue::Literal(lit.node), sp))
            }
            _ => Err(self.err_here("expected contract value (identifier or literal)")),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Blocks + statements
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_block(&mut self) -> Result<Spanned<Block>, ParseError> {
        let start = self.expect(Token::LBrace)?.span.start;
        let mut stmts = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(Block { stmts }, Span::new(start, end)))
    }

    fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        match &self.peek().node {
            Token::Keyword(Keyword::Let) => self.parse_let_stmt(),
            Token::Keyword(Keyword::Constrain) => self.parse_constrain_stmt(),
            Token::Keyword(Keyword::Prove) => self.parse_prove_stmt(),
            Token::Keyword(Keyword::Observe)
            | Token::Keyword(Keyword::Emit)
            | Token::Keyword(Keyword::Seal) => self.parse_effect_stmt(),
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(),
            _ => Err(self.err_here("expected statement")),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Let)?.span.start;
        let name = self.expect_ident()?;
        self.expect(Token::Eq)?;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(Stmt::Let(LetStmt { name, expr }), Span::new(start, end)))
    }

    fn parse_constrain_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Constrain)?.span.start;
        let predicate = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Constrain(ConstrainStmt { predicate }),
            Span::new(start, end),
        ))
    }

    fn parse_prove_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Prove)?.span.start;
        let name = self.expect_ident()?;
        self.expect_kw(Keyword::From)?;
        let from = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(Stmt::Prove(ProveStmt { name, from }), Span::new(start, end)))
    }

    fn parse_effect_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let t = self.bump();
        let start = t.span.start;
        let kind = match t.node {
            Token::Keyword(Keyword::Observe) => EffectKind::Observe,
            Token::Keyword(Keyword::Emit) => EffectKind::Emit,
            Token::Keyword(Keyword::Seal) => EffectKind::Seal,
            _ => {
                return Err(ParseError {
                    message: "expected effect kind".into(),
                    span: t.span,
                })
            }
        };
        let kind = Spanned::new(kind, t.span);
        let payload = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Effect(EffectStmt { kind, payload }),
            Span::new(start, end),
        ))
    }

    fn parse_return_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Return)?.span.start;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(Stmt::Return(ReturnStmt { expr }), Span::new(start, end)))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Expressions (Pratt)
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_postfix()?;

        loop {
            let (op_kind, l_bp, r_bp) = match self.peek().node {
                Token::OrOr => (BinOp::Or, 1, 2),
                Token::AndAnd => (BinOp::And, 3, 4),
                Token::EqEq => (BinOp::Eq, 5, 6),
                Token::Lt => (BinOp::Lt, 7, 8),
                Token::Lte => (BinOp::Le, 7, 8),
                Token::Gt => (BinOp::Gt, 7, 8),
                Token::Gte => (BinOp::Ge, 7, 8),
                Token::Plus => (BinOp::Add, 9, 10),
                Token::Minus => (BinOp::Sub, 9, 10),
                Token::Star => (BinOp::Mul, 11, 12),
                Token::Slash => (BinOp::Div, 11, 12),
                _ => break,
            };

            if l_bp < min_bp {
                break;
            }

            let op_span = self.bump().span;
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = Span::new(lhs.span.start, rhs.span.end);
            let op = Spanned::new(op_kind, op_span);
            let node = BinaryExpr { op, lhs, rhs };
            lhs = Spanned::new(Expr::Binary(Box::new(Spanned::new(node, span))), span);
        }

        Ok(lhs)
    }

    fn parse_postfix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut base = self.parse_primary()?;
        loop {
            match self.peek().node.clone() {
                Token::Dot => {
                    self.bump();
                    let field = self.expect_ident()?;
                    let span = Span::new(base.span.start, field.span.end);
                    let node = FieldExpr { base, field };
                    base = Spanned::new(Expr::Field(Box::new(Spanned::new(node, span))), span);
                }
                Token::LParen => {
                    let start = base.span.start;
                    self.bump();
                    let args = if self.peek_is(&Token::RParen) {
                        Vec::new()
                    } else {
                        self.parse_argument_list()?
                    };
                    let end = self.expect(Token::RParen)?.span.end;
                    let span = Span::new(start, end);
                    let node = CallExpr { callee: base, args };
                    base = Spanned::new(Expr::Call(Box::new(Spanned::new(node, span))), span);
                }
                Token::LBrace => {
                    // Struct literal: only valid when base is an identifier (type name)
                    base = self.parse_struct_literal(base)?;
                }
                _ => break,
            }
        }
        Ok(base)
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Spanned<Expr>>, ParseError> {
        let mut out = Vec::new();
        loop {
            out.push(self.parse_expr()?);
            if self.peek_is(&Token::Comma) {
                self.bump();
                continue;
            }
            break;
        }
        Ok(out)
    }

    fn parse_struct_literal(&mut self, ty_expr: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let ty_name = match ty_expr.node {
            Expr::Ident(id) => id,
            _ => {
                return Err(ParseError {
                    message: "struct literal requires type name identifier".into(),
                    span: ty_expr.span,
                })
            }
        };

        let start = ty_name.span.start;
        self.expect(Token::LBrace)?;
        let mut inits = Vec::new();
        if !self.peek_is(&Token::RBrace) {
            loop {
                let name = self.expect_ident()?;
                let i_start = name.span.start;
                self.expect(Token::Colon)?;
                let expr = self.parse_expr()?;
                let i_end = expr.span.end;
                inits.push(Spanned::new(FieldInit { name, expr }, Span::new(i_start, i_end)));
                if self.peek_is(&Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        let end = self.expect(Token::RBrace)?.span.end;
        let span = Span::new(start, end);
        let node = StructLitExpr { ty_name, inits };
        Ok(Spanned::new(Expr::StructLit(Box::new(Spanned::new(node, span))), span))
    }

    fn parse_primary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Int(_) | Token::String(_) | Token::Bool(_) => {
                let lit = self.parse_literal()?;
                Ok(Spanned::new(Expr::Literal(lit.node), lit.span))
            }
            Token::Ident(_) => {
                let id = self.expect_ident()?;
                Ok(Spanned::new(Expr::Ident(id.clone()), id.span))
            }
            Token::LParen => {
                let start = self.bump().span.start;
                let e = self.parse_expr()?;
                let end = self.expect(Token::RParen)?.span.end;
                let span = Span::new(start, end);
                Ok(Spanned::new(Expr::Paren(Box::new(e)), span))
            }
            _ => Err(self.err_here("expected expression")),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Literals + types
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_literal(&mut self) -> Result<Spanned<Literal>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Int(v) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Literal::Int(v), sp))
            }
            Token::String(s) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Literal::String(s), sp))
            }
            Token::Bool(b) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Literal::Bool(b), sp))
            }
            _ => Err(self.err_here("expected literal")),
        }
    }

    fn parse_type_ref(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        // Spec v0.1: type_ref ::= identifier
        // We also support a primitive mapping by conventional names.
        let id = self.expect_ident()?;
        let span = id.span;
        let prim = match id.text.as_str() {
            "Unit" => Some(PrimitiveType::Unit),
            "Bool" => Some(PrimitiveType::Bool),
            "I64" => Some(PrimitiveType::I64),
            "U64" => Some(PrimitiveType::U64),
            "F64" => Some(PrimitiveType::F64),
            "String" => Some(PrimitiveType::String),
            _ => None,
        };
        let node = if let Some(p) = prim {
            TypeRef::Primitive(p)
        } else {
            TypeRef::Named(id)
        };
        Ok(Spanned::new(node, span))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Helpers
    // ─────────────────────────────────────────────────────────────────────────

    fn peek(&self) -> &Spanned<Token> {
        self.toks
            .get(self.i)
            .unwrap_or_else(|| self.toks.last().expect("non-empty token stream"))
    }

    fn bump(&mut self) -> Spanned<Token> {
        let t = self.peek().clone();
        self.i = (self.i + 1).min(self.toks.len());
        t
    }

    fn prev_span(&self) -> Span {
        if self.i == 0 {
            Span::new(0, 0)
        } else {
            self.toks[self.i.saturating_sub(1)].span
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek().node, Token::Eof)
    }

    fn peek_is(&self, want: &Token) -> bool {
        &self.peek().node == want
    }

    fn peek_kw(&self, want: Keyword) -> bool {
        matches!(&self.peek().node, Token::Keyword(k) if *k == want)
    }

    fn expect(&mut self, want: Token) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        if t.node == want {
            Ok(self.bump())
        } else {
            Err(ParseError {
                message: format!("expected {:?}, found {:?}", want, t.node),
                span: t.span,
            })
        }
    }

    fn expect_kw(&mut self, want: Keyword) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Keyword(k) if *k == want => Ok(self.bump()),
            _ => Err(ParseError {
                message: format!("expected keyword {:?}, found {:?}", want, t.node),
                span: t.span,
            }),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Ident(s) => {
                let sp = self.bump().span;
                Ok(Ident::new(s, sp))
            }
            _ => Err(ParseError {
                message: "expected identifier".into(),
                span: t.span,
            }),
        }
    }

    fn err_here(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            span: self.peek().span,
        }
    }
}