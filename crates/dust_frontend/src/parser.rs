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
}

impl Parser {
    pub fn new(toks: Vec<Spanned<Token>>) -> Self {
        Self { toks, i: 0 }
    }

    pub fn parse_file(&mut self) -> Result<FileAst, ParseError> {
        let mut forges = Vec::new();
        while !self.is_eof() {
            forges.push(self.parse_forge()?);
        }
        Ok(FileAst { forges })
    }

    fn parse_forge(&mut self) -> Result<Spanned<ForgeDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Forge)?.span.start;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        let mut items = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            items.push(self.parse_item()?);
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(
            ForgeDecl { name, items },
            Span::new(start, end),
        ))
    }

    fn parse_item(&mut self) -> Result<Spanned<Item>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
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
            _ => Err(ParseError {
                message: "Expected item: shape | proc | bind".into(),
                span: t.span,
            }),
        }
    }

    fn parse_shape(&mut self) -> Result<Spanned<ShapeDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Shape)?.span.start;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            let fname = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.parse_type_ref()?;
            self.expect(Token::Semi)?;
            fields.push(Spanned::new(FieldDecl { name: fname, ty }, Span::new(fname.span.start, ty.span.end)));
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(ShapeDecl { name, fields }, Span::new(start, end)))
    }

    fn parse_proc(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        let proc_kw = self.expect_kw(Keyword::Proc)?;
        let start = proc_kw.span.start;

        let path = self.parse_proc_path()?;

        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        if !self.peek_is(&Token::RParen) {
            loop {
                let pstart = self.peek().span.start;
                let name = self.expect_ident()?;
                self.expect(Token::Colon)?;
                let ty = self.parse_type_ref()?;
                let pend = ty.span.end;
                params.push(Spanned::new(Param { name, ty }, Span::new(pstart, pend)));
                if self.peek_is(&Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        self.expect(Token::RParen)?;

        // uses clauses (optional, can be comma-separated in examples)
        let mut uses = Vec::new();
        if self.peek_is_kw(Keyword::Uses) {
            loop {
                uses.push(self.parse_uses_clause()?);
                if self.peek_is(&Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }

        // return type (optional)
        let ret = if self.peek_is(&Token::Arrow) {
            self.bump();
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        // qualifiers (optional): linear
        let mut qualifiers = Vec::new();
        while self.peek_is_kw(Keyword::Linear) {
            let qtok = self.bump().clone();
            qualifiers.push(Spanned::new(ProcQualifier::Linear, qtok.span));
        }

        let sig_end = self.peek().span.start;

        let sig = ProcSig {
            keyword_span: proc_kw.span,
            path,
            params,
            uses,
            ret,
            qualifiers,
        };

        let body = self.parse_block()?;
        let end = body.span.end;

        Ok(Spanned::new(
            ProcDecl { sig: Spanned::new(sig, Span::new(start, sig_end)), body },
            Span::new(start, end),
        ))
    }

    fn parse_proc_path(&mut self) -> Result<Spanned<ProcPath>, ParseError> {
        let start = self.peek().span.start;

        // Regime token: K, Q, Φ
        let regime_tok = self.bump().clone();
        let regime = match regime_tok.node {
            Token::Keyword(Keyword::K) => Regime::K,
            Token::Keyword(Keyword::Q) => Regime::Q,
            Token::Keyword(Keyword::Phi) => Regime::Phi,
            _ => {
                return Err(ParseError {
                    message: "Expected regime: K | Q | Φ".into(),
                    span: regime_tok.span,
                })
            }
        };

        self.expect(Token::ColonColon)?;
        let name = self.expect_ident()?;

        Ok(Spanned::new(
            ProcPath {
                regime: Spanned::new(regime, regime_tok.span),
                name,
            },
            Span::new(start, name.span.end),
        ))
    }

    fn parse_uses_clause(&mut self) -> Result<Spanned<UsesClause>, ParseError> {
        let start = self.expect_kw(Keyword::Uses)?.span.start;
        let resource = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.peek_is(&Token::RParen) {
            loop {
                let k = self.expect_ident()?;
                self.expect(Token::Eq)?;
                let lit = self.parse_literal()?;
                args.push(Spanned::new(NamedArg { key: k, value: lit }, Span::new(k.span.start, lit.span.end)));
                if self.peek_is(&Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        let end = self.expect(Token::RParen)?.span.end;
        Ok(Spanned::new(UsesClause { resource, args }, Span::new(start, end)))
    }

    fn parse_bind(&mut self) -> Result<Spanned<BindDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Bind)?.span.start;

        let source = self.parse_proc_path_ref()?;
        let arrow = self.expect(Token::Arrow)?;
        let target = self.parse_proc_path_ref()?;

        let contract_kw = self.expect_kw(Keyword::Contract)?;
        let _ = contract_kw;
        let contract = self.parse_contract_block()?;

        let end = contract.span.end;

        Ok(Spanned::new(
            BindDecl {
                source,
                arrow_span: arrow.span,
                target,
                contract,
            },
            Span::new(start, end),
        ))
    }

    fn parse_proc_path_ref(&mut self) -> Result<Spanned<ProcPathRef>, ParseError> {
        // either Qualified (K::name etc.) or Unqualified (name)
        let start = self.peek().span.start;

        if self.peek_is_kw(Keyword::K) || self.peek_is_kw(Keyword::Q) || self.peek_is_kw(Keyword::Phi) {
            let q = self.parse_proc_path()?;
            return Ok(Spanned::new(ProcPathRef::Qualified(q.node), Span::new(start, q.span.end)));
        }

        let id = self.expect_ident()?;
        Ok(Spanned::new(ProcPathRef::Unqualified(id.clone()), Span::new(start, id.span.end)))
    }

    fn parse_contract_block(&mut self) -> Result<Spanned<ContractBlock>, ParseError> {
        let start = self.expect(Token::LBrace)?.span.start;
        let mut clauses = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            let k = self.expect_ident()?;
            let op_tok = self.bump().clone();
            let op = match op_tok.node {
                Token::EqEq => ContractOp::EqEq,
                Token::Lt => ContractOp::Lt,
                Token::Lte => ContractOp::Lte,
                Token::Gt => ContractOp::Gt,
                Token::Gte => ContractOp::Gte,
                _ => {
                    return Err(ParseError {
                        message: "Expected contract operator: == < <= > >=".into(),
                        span: op_tok.span,
                    })
                }
            };

            let value = if let Some(lit) = self.try_parse_literal() {
                Spanned::new(ContractValue::Literal(lit.node), lit.span)
            } else {
                let id = self.expect_ident()?;
                Spanned::new(ContractValue::Ident(id.clone()), id.span)
            };

            self.expect(Token::Semi)?;
            clauses.push(Spanned::new(
                ContractClause {
                    key: k,
                    op: Spanned::new(op, op_tok.span),
                    value,
                },
                Span::new(start, self.peek().span.start),
            ));
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(ContractBlock { clauses }, Span::new(start, end)))
    }

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
        let t = self.peek().clone();
        match &t.node {
            Token::Keyword(Keyword::Let) => {
                let start = self.bump().span.start;
                let name = self.expect_ident()?;
                self.expect(Token::Eq)?;
                let expr = self.parse_expr()?;
                let end = self.expect(Token::Semi)?.span.end;
                Ok(Spanned::new(Stmt::Let(LetStmt { name, expr }), Span::new(start, end)))
            }
            Token::Keyword(Keyword::Constrain) => {
                let start = self.bump().span.start;
                let predicate = self.parse_expr()?;
                let end = self.expect(Token::Semi)?.span.end;
                Ok(Spanned::new(Stmt::Constrain(ConstrainStmt { predicate }), Span::new(start, end)))
            }
            Token::Keyword(Keyword::Prove) => {
                let start = self.bump().span.start;
                let name = self.expect_ident()?;
                self.expect_kw(Keyword::From)?;
                let from = self.parse_expr()?;
                let end = self.expect(Token::Semi)?.span.end;
                Ok(Spanned::new(Stmt::Prove(ProveStmt { name, from }), Span::new(start, end)))
            }
            Token::Keyword(Keyword::Observe) | Token::Keyword(Keyword::Emit) | Token::Keyword(Keyword::Seal) => {
                let kind_tok = self.bump().clone();
                let kind = match kind_tok.node {
                    Token::Keyword(Keyword::Observe) => EffectKind::Observe,
                    Token::Keyword(Keyword::Emit) => EffectKind::Emit,
                    Token::Keyword(Keyword::Seal) => EffectKind::Seal,
                    _ => unreachable!(),
                };
                let payload = self.parse_expr()?;
                let end = self.expect(Token::Semi)?.span.end;
                Ok(Spanned::new(
                    Stmt::Effect(EffectStmt { kind: Spanned::new(kind, kind_tok.span), payload }),
                    Span::new(kind_tok.span.start, end),
                ))
            }
            Token::Keyword(Keyword::Return) => {
                let start = self.bump().span.start;
                let expr = self.parse_expr()?;
                let end = self.expect(Token::Semi)?.span.end;
                Ok(Spanned::new(Stmt::Return(ReturnStmt { expr }), Span::new(start, end)))
            }
            _ => Err(ParseError { message: "Expected statement".into(), span: t.span }),
        }
    }

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        // Minimal Pratt-less parser: parse a primary and then a chain of
        // (field | call | binary) left-associatively with simple precedence.
        // Enough for our examples (arithmetic + comparisons + &&/||).

        self.parse_expr_prec(0)
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_postfix()?;

        loop {
            let (op, prec) = match self.peek().node {
                Token::Plus => (BinaryOp::Add, 10),
                Token::Minus => (BinaryOp::Sub, 10),
                Token::Star => (BinaryOp::Mul, 20),
                Token::Slash => (BinaryOp::Div, 20),
                Token::EqEq => (BinaryOp::EqEq, 7),
                Token::Lt => (BinaryOp::Lt, 8),
                Token::Lte => (BinaryOp::Lte, 8),
                Token::Gt => (BinaryOp::Gt, 8),
                Token::Gte => (BinaryOp::Gte, 8),
                Token::AndAnd => (BinaryOp::AndAnd, 4),
                Token::OrOr => (BinaryOp::OrOr, 3),
                _ => break,
            };

            if prec < min_prec {
                break;
            }

            let op_tok = self.bump().clone();
            let rhs = self.parse_expr_prec(prec + 1)?;

            let span = Span::new(lhs.span.start, rhs.span.end);
            lhs = Spanned::new(
                Expr::Binary(Box::new(Spanned::new(BinaryExpr { lhs, op: Spanned::new(op, op_tok.span), rhs }, span))),
                span,
            );
        }

        Ok(lhs)
    }

    fn parse_postfix(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.peek_is(&Token::Dot) {
                let dot = self.bump().span;
                let field = self.expect_ident()?;
                let span = Span::new(expr.span.start, field.span.end);
                expr = Spanned::new(
                    Expr::Field(Box::new(Spanned::new(FieldExpr { base: expr, dot_span: dot, field }, span))),
                    span,
                );
                continue;
            }

            if self.peek_is(&Token::LParen) {
                let start = expr.span.start;
                self.bump();
                let mut args = Vec::new();
                if !self.peek_is(&Token::RParen) {
                    loop {
                        args.push(self.parse_expr()?);
                        if self.peek_is(&Token::Comma) {
                            self.bump();
                            continue;
                        }
                        break;
                    }
                }
                let end = self.expect(Token::RParen)?.span.end;
                let span = Span::new(start, end);
                expr = Spanned::new(
                    Expr::Call(Box::new(Spanned::new(CallExpr { callee: expr, args }, span))),
                    span,
                );
                continue;
            }

            // Struct literal: Ident { field: expr, ... }
            if matches!(expr.node, Expr::Ident(_)) && self.peek_is(&Token::LBrace) {
                let ty_name = if let Expr::Ident(id) = &expr.node { id.clone() } else { unreachable!() };
                let start = expr.span.start;
                self.bump(); // {
                let mut inits = Vec::new();
                if !self.peek_is(&Token::RBrace) {
                    loop {
                        let n = self.expect_ident()?;
                        self.expect(Token::Colon)?;
                        let e = self.parse_expr()?;
                        inits.push(Spanned::new(FieldInit { name: n, expr: e }, Span::new(start, self.peek().span.start)));
                        if self.peek_is(&Token::Comma) {
                            self.bump();
                            continue;
                        }
                        break;
                    }
                }
                let end = self.expect(Token::RBrace)?.span.end;
                let span = Span::new(start, end);
                expr = Spanned::new(
                    Expr::StructLit(Box::new(Spanned::new(StructLitExpr { ty_name, inits }, span))),
                    span,
                );
                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        let start = t.span.start;

        if let Some(lit) = self.try_parse_literal() {
            return Ok(Spanned::new(Expr::Literal(lit.node), lit.span));
        }

        match &t.node {
            Token::Ident(_) => {
                let id = self.expect_ident()?;
                Ok(Spanned::new(Expr::Ident(id.clone()), Span::new(start, id.span.end)))
            }
            Token::LParen => {
                self.bump();
                let inner = self.parse_expr()?;
                let end = self.expect(Token::RParen)?.span.end;
                Ok(Spanned::new(Expr::Paren(Box::new(inner)), Span::new(start, end)))
            }
            _ => Err(ParseError { message: "Expected expression".into(), span: t.span }),
        }
    }

    fn parse_literal(&mut self) -> Result<Spanned<Literal>, ParseError> {
        self.try_parse_literal().ok_or_else(|| ParseError {
            message: "Expected literal".into(),
            span: self.peek().span,
        })
    }

    fn try_parse_literal(&mut self) -> Option<Spanned<Literal>> {
        let t = self.peek().clone();
        match &t.node {
            Token::Int(n) => { self.bump(); Some(Spanned::new(Literal::Int(*n), t.span)) }
            Token::Bool(b) => { self.bump(); Some(Spanned::new(Literal::Bool(*b), t.span)) }
            Token::String(s) => { self.bump(); Some(Spanned::new(Literal::String(s.clone()), t.span)) }
            _ => None,
        }
    }

    fn parse_type_ref(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        let t = self.peek().clone();
        let start = t.span.start;
        match &t.node {
            Token::Ident(name) => {
                let id = self.expect_ident()?;
                let end = id.span.end;
                Ok(Spanned::new(TypeRef::Named(id), Span::new(start, end)))
            }
            Token::Keyword(k) => {
                // primitives can be keywords later; for now parse via ident tokens only
                Err(ParseError { message: format!("Unexpected keyword in type position: {:?}", k), span: t.span })
            }
            _ => Err(ParseError { message: "Expected type name".into(), span: t.span }),
        }
    }

    // ---- token utils ----

    fn peek(&self) -> &Spanned<Token> {
        &self.toks[self.i]
    }

    fn bump(&mut self) -> &Spanned<Token> {
        let t = &self.toks[self.i];
        self.i += 1;
        t
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek().node, Token::Eof)
    }

    fn peek_is(&self, want: &Token) -> bool {
        std::mem::discriminant(&self.peek().node) == std::mem::discriminant(want)
    }

    fn peek_is_kw(&self, want: Keyword) -> bool {
        matches!(self.peek().node, Token::Keyword(k) if *k == want)
    }

    fn expect(&mut self, want: Token) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        if std::mem::discriminant(&t.node) == std::mem::discriminant(&want) {
            self.i += 1;
            Ok(t)
        } else {
            Err(ParseError { message: format!("Expected {:?}", want), span: t.span })
        }
    }

    fn expect_kw(&mut self, want: Keyword) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Keyword(k) if *k == want => { self.i += 1; Ok(t) }
            _ => Err(ParseError { message: format!("Expected keyword {:?}", want), span: t.span }),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Ident(s) => {
                self.i += 1;
                Ok(Ident::new(s, t.span))
            }
            _ => Err(ParseError { message: "Expected identifier".into(), span: t.span }),
        }
    }
}