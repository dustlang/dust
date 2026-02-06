// crates/dust_frontend/src/parser.rs

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
            let f_start = fname.span.start;

            self.expect(Token::Colon)?;
            let ty = self.parse_type_ref()?;
            let t_end = ty.span.end;

            self.expect(Token::Semi)?;
            fields.push(Spanned::new(
                FieldDecl { name: fname, ty },
                Span::new(f_start, t_end),
            ));
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(
            ShapeDecl { name, fields },
            Span::new(start, end),
        ))
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

        // uses clauses (optional, can be comma-separated)
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

        // contract block (optional)
        let contract = if self.peek_is_kw(Keyword::Contract) {
            Some(self.parse_contract_block()?)
        } else {
            None
        };

        // body
        let body = self.parse_block()?;
        let end = body.span.end;

        let sig_end = if let Some(c) = &contract { c.span.end } else { body.span.start };
        let sig = ProcSig {
            path,
            params,
            uses,
            contract,
        };

        Ok(Spanned::new(
            ProcDecl {
                sig: Spanned::new(sig, Span::new(start, sig_end)),
                body,
            },
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
        let end = name.span.end;

        Ok(Spanned::new(
            ProcPath {
                regime: Spanned::new(regime, regime_tok.span),
                name,
            },
            Span::new(start, end),
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
                let k_start = k.span.start;
                self.expect(Token::Eq)?;
                let lit = self.parse_literal()?;
                let lit_end = lit.span.end;
                args.push(Spanned::new(
                    NamedArg { key: k, value: lit },
                    Span::new(k_start, lit_end),
                ));
                if self.peek_is(&Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        let end = self.expect(Token::RParen)?.span.end;
        Ok(Spanned::new(
            UsesClause { resource, args },
            Span::new(start, end),
        ))
    }

    fn parse_bind(&mut self) -> Result<Spanned<BindDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Bind)?.span.start;

        let source = self.parse_proc_path_ref()?;
        self.expect(Token::Arrow)?;
        let target = self.parse_proc_path_ref()?;

        let mut contract = None;
        if self.peek_is_kw(Keyword::Contract) {
            contract = Some(self.parse_contract_block()?);
        }

        self.expect(Token::Semi)?;
        let end = self.prev_span_end();

        Ok(Spanned::new(
            BindDecl {
                source,
                target,
                contract,
            },
            Span::new(start, end),
        ))
    }

    fn parse_proc_path_ref(&mut self) -> Result<Spanned<ProcPathRef>, ParseError> {
        // Supports:
        // - K::name
        // - Q::name
        // - Φ::name
        // - name (unqualified)
        let start = self.peek().span.start;

        // If next is a regime keyword, parse qualified
        if matches!(&self.peek().node, Token::Keyword(Keyword::K | Keyword::Q | Keyword::Phi)) {
            let q = self.parse_proc_path()?;
            return Ok(Spanned::new(
                ProcPathRef::Qualified(q.node),
                Span::new(start, q.span.end),
            ));
        }

        let id = self.expect_ident()?;
        Ok(Spanned::new(
            ProcPathRef::Unqualified(id.clone()),
            Span::new(start, id.span.end),
        ))
    }

    fn parse_contract_block(&mut self) -> Result<Spanned<ContractBlock>, ParseError> {
        let start = self.expect_kw(Keyword::Contract)?.span.start;
        self.expect(Token::LBrace)?;
        let mut clauses = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            clauses.push(self.parse_contract_clause()?);
        }
        let end = self.expect(Token::RBrace)?.span.end;
        Ok(Spanned::new(
            ContractBlock { clauses },
            Span::new(start, end),
        ))
    }

    fn parse_contract_clause(&mut self) -> Result<Spanned<ContractClause>, ParseError> {
        let start = self.peek().span.start;

        let kind_tok = self.bump().clone();
        let kind = match kind_tok.node {
            Token::Keyword(Keyword::Requires) => ContractKind::Requires,
            Token::Keyword(Keyword::Ensures) => ContractKind::Ensures,
            Token::Keyword(Keyword::Invariant) => ContractKind::Invariant,
            _ => {
                return Err(ParseError {
                    message: "Expected contract clause: requires | ensures | invariant".into(),
                    span: kind_tok.span,
                })
            }
        };

        let pred = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span_end();

        Ok(Spanned::new(
            ContractClause { kind, predicate: pred },
            Span::new(start, end),
        ))
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
        let start = t.span.start;
        match &t.node {
            Token::Keyword(Keyword::Let) => {
                self.bump();
                let name = self.expect_ident()?;
                self.expect(Token::Eq)?;
                let expr = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(
                    Stmt::Let(LetStmt { name, expr }),
                    Span::new(start, end),
                ))
            }
            Token::Keyword(Keyword::Constrain) => {
                self.bump();
                let predicate = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(
                    Stmt::Constrain(ConstrainStmt { predicate }),
                    Span::new(start, end),
                ))
            }
            Token::Keyword(Keyword::Prove) => {
                self.bump();
                let name = self.expect_ident()?;
                self.expect_kw(Keyword::From)?;
                let from = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(
                    Stmt::Prove(ProveStmt { name, from }),
                    Span::new(start, end),
                ))
            }
            Token::Keyword(Keyword::Emit) => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(
                    Stmt::Emit(EmitStmt { expr }),
                    Span::new(start, end),
                ))
            }
            Token::Keyword(Keyword::Observe) => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(
                    Stmt::Observe(ObserveStmt { expr }),
                    Span::new(start, end),
                ))
            }
            Token::Keyword(Keyword::Seal) => {
                self.bump();
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(Stmt::Seal(SealStmt {}), Span::new(start, end)))
            }
            Token::Keyword(Keyword::Return) => {
                self.bump();
                let expr = if self.peek_is(&Token::Semi) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(
                    Stmt::Return(ReturnStmt { expr }),
                    Span::new(start, end),
                ))
            }
            _ => {
                // expression statement
                let expr = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span_end();
                Ok(Spanned::new(Stmt::Expr(expr), Span::new(start, end)))
            }
        }
    }

    // ---- expressions ----

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_prec(0)
    }

    fn parse_expr_prec(&mut self, min_prec: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_expr_primary()?;

        loop {
            let (op, prec) = match &self.peek().node {
                Token::Plus => (BinOp::Add, 10),
                Token::Minus => (BinOp::Sub, 10),
                Token::Star => (BinOp::Mul, 20),
                Token::Slash => (BinOp::Div, 20),
                Token::EqEq => (BinOp::Eq, 5),
                Token::BangEq => (BinOp::Neq, 5),
                Token::Lt => (BinOp::Lt, 6),
                Token::Lte => (BinOp::Lte, 6),
                Token::Gt => (BinOp::Gt, 6),
                Token::Gte => (BinOp::Gte, 6),
                _ => break,
            };

            if prec < min_prec {
                break;
            }

            let op_tok = self.bump().clone();
            let rhs = self.parse_expr_prec(prec + 1)?;
            let start = lhs.span.start;
            let end = rhs.span.end;
            lhs = Spanned::new(
                Expr::Binary(Box::new(BinaryExpr {
                    op: Spanned::new(op, op_tok.span),
                    left: lhs,
                    right: rhs,
                })),
                Span::new(start, end),
            );
        }

        Ok(lhs)
    }

    fn parse_expr_primary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        let start = t.span.start;

        // literal
        if let Some(l) = self.try_parse_literal() {
            return Ok(Spanned::new(Expr::Literal(l.node), l.span));
        }

        match &t.node {
            Token::Ident(_name) => {
                let id = self.expect_ident()?;
                // call or field access chain
                let mut expr = Spanned::new(Expr::Ident(id.clone()), Span::new(start, id.span.end));

                loop {
                    if self.peek_is(&Token::Dot) {
                        self.bump();
                        let field = self.expect_ident()?;
                        let end = field.span.end;
                        expr = Spanned::new(
                            Expr::Field(Box::new(FieldExpr {
                                base: expr,
                                field,
                            })),
                            Span::new(start, end),
                        );
                        continue;
                    }

                    if self.peek_is(&Token::LParen) {
                        let call = self.parse_call(expr)?;
                        expr = call;
                        continue;
                    }

                    break;
                }

                Ok(expr)
            }
            Token::LParen => {
                self.bump();
                let inner = self.parse_expr()?;
                let end = self.expect(Token::RParen)?.span.end;
                Ok(Spanned::new(
                    Expr::Paren(Box::new(inner)),
                    Span::new(start, end),
                ))
            }
            Token::LBrace => {
                // block expr
                let b = self.parse_block()?;
                Ok(Spanned::new(Expr::Block(b.node), b.span))
            }
            _ => Err(ParseError {
                message: "Expected expression".into(),
                span: t.span,
            }),
        }
    }

    fn parse_call(&mut self, callee: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = callee.span.start;
        self.expect(Token::LParen)?;
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
        Ok(Spanned::new(
            Expr::Call(Box::new(CallExpr { callee, args })),
            Span::new(start, end),
        ))
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
            Token::Int(n) => {
                self.bump();
                Some(Spanned::new(Literal::Int(*n), t.span))
            }
            Token::Bool(b) => {
                self.bump();
                Some(Spanned::new(Literal::Bool(*b), t.span))
            }
            Token::String(s) => {
                self.bump();
                Some(Spanned::new(Literal::String(s.clone()), t.span))
            }
            _ => None,
        }
    }

    fn parse_type_ref(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        let t = self.peek().clone();
        let start = t.span.start;
        match &t.node {
            Token::Ident(_name) => {
                let id = self.expect_ident()?;
                let end = id.span.end;
                Ok(Spanned::new(TypeRef::Named(id), Span::new(start, end)))
            }
            Token::Keyword(k) => Err(ParseError {
                message: format!("Unexpected keyword in type position: {:?}", k),
                span: t.span,
            }),
            _ => Err(ParseError {
                message: "Expected type name".into(),
                span: t.span,
            }),
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
        // IMPORTANT: match on a reference, so `k` is `&Keyword` and comparison is valid.
        matches!(&self.peek().node, Token::Keyword(k) if *k == want)
    }

    fn expect(&mut self, want: Token) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        if std::mem::discriminant(&t.node) == std::mem::discriminant(&want) {
            self.i += 1;
            Ok(t)
        } else {
            Err(ParseError {
                message: format!("Expected {:?}", want),
                span: t.span,
            })
        }
    }

    fn expect_kw(&mut self, want: Keyword) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Keyword(k) if *k == want => {
                self.i += 1;
                Ok(t)
            }
            _ => Err(ParseError {
                message: format!("Expected keyword {:?}", want),
                span: t.span,
            }),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Ident(s) => {
                self.i += 1;
                Ok(Ident::new(s, t.span))
            }
            _ => Err(ParseError {
                message: "Expected identifier".into(),
                span: t.span,
            }),
        }
    }

    fn prev_span_end(&self) -> u32 {
        if self.i == 0 {
            0
        } else {
            self.toks[self.i - 1].span.end
        }
    }
}