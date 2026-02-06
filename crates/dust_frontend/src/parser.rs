// crates/dust_frontend/src/parser.rs
//
// DPL v0.1 parser (frontend) — produces AST from lexer tokens.
// Goal: compile cleanly + provide a strict-enough, spec-aligned structure parser.
//
// Notes:
// - We deliberately keep parsing rules conservative.
// - For v0.1, we treat some “future keywords” as identifiers inside blocks
//   (e.g., `requires`, `ensures`, `if`, `else`) to avoid lexer churn.
// - This parser is NOT a full language implementation; it is a correct structural
//   frontend for the current AST.
//
// © 2026 Dust LLC

use crate::ast::*;
use crate::lexer::{Keyword, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    UnexpectedEof,
    UnexpectedToken,
    ExpectedToken,
    ExpectedIdent,
    ExpectedLiteral,
    ExpectedRegime,
    ExpectedPathSep,
    ExpectedBlock,
    Message,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
    pub message: String,
}

impl ParseError {
    fn new(kind: ParseErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            message: message.into(),
        }
    }
}

pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    i: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self { tokens, i: 0 }
    }

    pub fn parse_file(&mut self) -> Result<File, ParseError> {
        let mut items = Vec::new();
        while !self.is_eof() {
            // Skip stray semicolons (tolerant mode)
            if self.peek_is(Token::Semi) {
                self.bump();
                continue;
            }
            items.push(self.parse_item()?);
        }
        Ok(File { items })
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Items
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        if self.peek_is_kw(Keyword::Forge) {
            Ok(Item::Forge(self.parse_forge()?))
        } else if self.peek_is_kw(Keyword::Shape) {
            Ok(Item::Shape(self.parse_shape()?))
        } else if self.peek_is_kw(Keyword::Proc) {
            Ok(Item::Proc(self.parse_proc()?))
        } else if self.peek_is_kw(Keyword::Bind) {
            Ok(Item::Bind(self.parse_bind()?))
        } else {
            Err(self.err_here(
                ParseErrorKind::UnexpectedToken,
                "expected item: forge | shape | proc | bind",
            ))
        }
    }

    fn parse_forge(&mut self) -> Result<Spanned<ForgeDecl>, ParseError> {
        let kw = self.expect_kw(Keyword::Forge)?;
        let name = self.expect_ident()?;
        let start = kw.span.start;

        self.expect(Token::LBrace)?;
        let mut items = Vec::new();
        while !self.peek_is(Token::RBrace) {
            if self.is_eof() {
                return Err(self.err_at(
                    ParseErrorKind::UnexpectedEof,
                    Span::new(start, self.last_end()),
                    "unterminated forge block",
                ));
            }
            if self.peek_is(Token::Semi) {
                self.bump();
                continue;
            }
            items.push(self.parse_item()?);
        }
        let end = self.expect(Token::RBrace)?.span.end;

        Ok(Spanned::new(
            ForgeDecl { name, items },
            Span::new(start, end),
        ))
    }

    fn parse_shape(&mut self) -> Result<Spanned<ShapeDecl>, ParseError> {
        let kw = self.expect_kw(Keyword::Shape)?;
        let name = self.expect_ident()?;
        let start = kw.span.start;

        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();
        while !self.peek_is(Token::RBrace) {
            if self.is_eof() {
                return Err(self.err_at(
                    ParseErrorKind::UnexpectedEof,
                    Span::new(start, self.last_end()),
                    "unterminated shape block",
                ));
            }
            if self.peek_is(Token::Semi) {
                self.bump();
                continue;
            }

            let fname = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let fty = self.parse_type_ref()?;
            let semi = self.expect(Token::Semi)?;

            let fspan = Span::new(fname.span.start, semi.span.end);
            fields.push(Spanned::new(
                FieldDecl {
                    name: fname,
                    ty: fty,
                },
                fspan,
            ));
        }
        let end = self.expect(Token::RBrace)?.span.end;

        Ok(Spanned::new(
            ShapeDecl { name, fields },
            Span::new(start, end),
        ))
    }

    fn parse_proc(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        let kw = self.expect_kw(Keyword::Proc)?;
        let start = kw.span.start;

        let path = self.parse_proc_path()?;

        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        if !self.peek_is(Token::RParen) {
            loop {
                let p = self.parse_param_decl()?;
                params.push(p);
                if self.peek_is(Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        self.expect(Token::RParen)?;

        let ret = if self.peek_is(Token::Arrow) {
            self.bump();
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        // zero or more contract blocks
        let mut contracts = Vec::new();
        while self.peek_is_kw(Keyword::Contract) {
            contracts.extend(self.parse_contract_block()?);
        }

        let body = self.parse_block()?;
        let end = body.span.end;

        Ok(Spanned::new(
            ProcDecl {
                path,
                params,
                ret,
                contracts,
                body,
            },
            Span::new(start, end),
        ))
    }

    fn parse_param_decl(&mut self) -> Result<Spanned<ParamDecl>, ParseError> {
        let name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type_ref()?;
        let span = Span::new(name.span.start, ty.span.end);
        Ok(Spanned::new(ParamDecl { name, ty }, span))
    }

    fn parse_bind(&mut self) -> Result<Spanned<BindDecl>, ParseError> {
        let kw = self.expect_kw(Keyword::Bind)?;
        let start = kw.span.start;

        let name = self.expect_ident()?;

        // allow: bind x = K::foo(...)
        // allow: bind x -> K::foo(...)
        if self.peek_is(Token::Eq) || self.peek_is(Token::Arrow) {
            self.bump();
        } else {
            return Err(self.err_here(
                ParseErrorKind::ExpectedToken,
                "expected '=' or '->' after bind name",
            ));
        }

        let target = self.parse_proc_path()?;

        let mut args = Vec::new();
        if self.peek_is(Token::LParen) {
            self.bump();
            if !self.peek_is(Token::RParen) {
                loop {
                    args.push(self.parse_named_arg()?);
                    if self.peek_is(Token::Comma) {
                        self.bump();
                        continue;
                    }
                    break;
                }
            }
            self.expect(Token::RParen)?;
        }

        let semi = self.expect(Token::Semi)?;
        let end = semi.span.end;

        Ok(Spanned::new(
            BindDecl {
                name,
                target,
                args,
            },
            Span::new(start, end),
        ))
    }

    fn parse_named_arg(&mut self) -> Result<Spanned<NamedArg>, ParseError> {
        let key = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let lit = self.parse_literal()?;
        let span = Span::new(key.span.start, lit.span.end);
        Ok(Spanned::new(
            NamedArg {
                key,
                value: lit,
            },
            span,
        ))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Proc paths + regimes
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_proc_path(&mut self) -> Result<Spanned<ProcPath>, ParseError> {
        let regime = self.parse_regime()?;
        let start = regime.span.start;

        if !self.peek_is(Token::ColonColon) {
            return Err(self.err_here(
                ParseErrorKind::ExpectedPathSep,
                "expected '::' in proc path",
            ));
        }
        self.bump(); // ::

        let name = self.expect_ident()?;
        let end = name.span.end;

        Ok(Spanned::new(
            ProcPath { regime, name },
            Span::new(start, end),
        ))
    }

    fn parse_regime(&mut self) -> Result<Spanned<Regime>, ParseError> {
        let t = self.peek();
        match &t.node {
            Token::Keyword(Keyword::K) => {
                let s = t.span;
                self.bump();
                Ok(Spanned::new(Regime::K, s))
            }
            Token::Keyword(Keyword::Q) => {
                let s = t.span;
                self.bump();
                Ok(Spanned::new(Regime::Q, s))
            }
            Token::Keyword(Keyword::Phi) => {
                let s = t.span;
                self.bump();
                Ok(Spanned::new(Regime::Phi, s))
            }
            _ => Err(self.err_here(
                ParseErrorKind::ExpectedRegime,
                "expected regime keyword: K | Q | Φ",
            )),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Contracts
    // ─────────────────────────────────────────────────────────────────────────
    //
    // Syntax:
    //   contract { requires <expr>; ensures <expr>; uses <Ident>; }
    //
    // We intentionally parse `requires/ensures/uses` as identifiers (not keywords)
    // so the lexer doesn’t need to reserve more keywords in v0.1.
    //

    fn parse_contract_block(&mut self) -> Result<Vec<Spanned<ContractDecl>>, ParseError> {
        let kw = self.expect_kw(Keyword::Contract)?;
        let start = kw.span.start;

        self.expect(Token::LBrace)?;
        let mut out = Vec::new();

        while !self.peek_is(Token::RBrace) {
            if self.is_eof() {
                return Err(self.err_at(
                    ParseErrorKind::UnexpectedEof,
                    Span::new(start, self.last_end()),
                    "unterminated contract block",
                ));
            }
            if self.peek_is(Token::Semi) {
                self.bump();
                continue;
            }

            // clause head must be an identifier: requires | ensures | uses
            let head = self.expect_ident()?;
            let head_txt = head.text.as_str();

            match head_txt {
                "requires" => {
                    let pred = self.parse_expr()?;
                    let semi = self.expect(Token::Semi)?;
                    out.push(Spanned::new(
                        ContractDecl::Requires(pred),
                        Span::new(head.span.start, semi.span.end),
                    ));
                }
                "ensures" => {
                    let pred = self.parse_expr()?;
                    let semi = self.expect(Token::Semi)?;
                    out.push(Spanned::new(
                        ContractDecl::Ensures(pred),
                        Span::new(head.span.start, semi.span.end),
                    ));
                }
                "uses" => {
                    let name = self.expect_ident()?;
                    let semi = self.expect(Token::Semi)?;
                    out.push(Spanned::new(
                        ContractDecl::Uses(name),
                        Span::new(head.span.start, semi.span.end),
                    ));
                }
                _ => {
                    return Err(self.err_at(
                        ParseErrorKind::Message,
                        head.span,
                        "unknown contract clause; expected: requires | ensures | uses",
                    ));
                }
            }
        }

        self.expect(Token::RBrace)?;
        Ok(out)
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Blocks + statements
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_block(&mut self) -> Result<Spanned<Block>, ParseError> {
        if !self.peek_is(Token::LBrace) {
            return Err(self.err_here(ParseErrorKind::ExpectedBlock, "expected '{'"));
        }
        let lb = self.expect(Token::LBrace)?;
        let start = lb.span.start;

        let mut stmts = Vec::new();
        while !self.peek_is(Token::RBrace) {
            if self.is_eof() {
                return Err(self.err_at(
                    ParseErrorKind::UnexpectedEof,
                    Span::new(start, self.last_end()),
                    "unterminated block",
                ));
            }
            if self.peek_is(Token::Semi) {
                self.bump();
                continue;
            }
            stmts.push(self.parse_stmt()?);
        }

        let rb = self.expect(Token::RBrace)?;
        let end = rb.span.end;

        Ok(Spanned::new(Block { stmts }, Span::new(start, end)))
    }

    fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let t = self.peek();
        match &t.node {
            Token::Keyword(Keyword::Let) => self.parse_let_stmt().map(|s| s.map(Stmt::Let)),
            Token::Keyword(Keyword::Constrain) => self.parse_unary_kw_stmt(Keyword::Constrain, Stmt::Constrain),
            Token::Keyword(Keyword::Prove) => self.parse_unary_kw_stmt(Keyword::Prove, Stmt::Prove),
            Token::Keyword(Keyword::Emit) => self.parse_unary_kw_stmt(Keyword::Emit, Stmt::Emit),
            Token::Keyword(Keyword::Observe) => self.parse_unary_kw_stmt(Keyword::Observe, Stmt::Observe),
            Token::Keyword(Keyword::Seal) => self.parse_unary_kw_stmt(Keyword::Seal, Stmt::Seal),
            Token::Keyword(Keyword::Return) => self.parse_return_stmt().map(|s| s.map(Stmt::Return)),
            Token::Ident(s) if s == "if" => self.parse_if_stmt(),
            _ => {
                // expression statement
                let e = self.parse_expr()?;
                let semi = self.expect(Token::Semi)?;
                let span = Span::new(e.span.start, semi.span.end);
                Ok(Spanned::new(Stmt::Expr(e), span))
            }
        }
    }

    fn parse_unary_kw_stmt<F>(
        &mut self,
        kw: Keyword,
        wrap: F,
    ) -> Result<Spanned<Stmt>, ParseError>
    where
        F: FnOnce(Spanned<Expr>) -> Stmt,
    {
        let k = self.expect_kw(kw)?;
        let e = self.parse_expr()?;
        let semi = self.expect(Token::Semi)?;
        let span = Span::new(k.span.start, semi.span.end);
        Ok(Spanned::new(wrap(e), span))
    }

    fn parse_let_stmt(&mut self) -> Result<Spanned<LetStmt>, ParseError> {
        let kw = self.expect_kw(Keyword::Let)?;
        let start = kw.span.start;

        let name = self.expect_ident()?;

        let ty = if self.peek_is(Token::Colon) {
            self.bump();
            Some(self.parse_type_ref()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;
        let expr = self.parse_expr()?;
        let semi = self.expect(Token::Semi)?;
        let end = semi.span.end;

        Ok(Spanned::new(
            LetStmt { name, ty, expr },
            Span::new(start, end),
        ))
    }

    fn parse_return_stmt(&mut self) -> Result<Spanned<Option<Spanned<Expr>>>, ParseError> {
        let kw = self.expect_kw(Keyword::Return)?;
        let start = kw.span.start;

        // allow: return;
        // allow: return <expr>;
        let expr = if self.peek_is(Token::Semi) {
            None
        } else {
            Some(self.parse_expr()?)
        };

        let semi = self.expect(Token::Semi)?;
        let end = semi.span.end;

        Ok(Spanned::new(expr, Span::new(start, end)))
    }

    fn parse_if_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        // `if` is parsed as an identifier token with text "if"
        let if_tok = self.expect_ident_text("if")?;
        let start = if_tok.span.start;

        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;

        let else_block = if self.peek_ident_text("else") {
            self.bump(); // consume ident "else"
            Some(self.parse_block()?)
        } else {
            None
        };

        let end = else_block
            .as_ref()
            .map(|b| b.span.end)
            .unwrap_or(then_block.span.end);

        Ok(Spanned::new(
            Stmt::If(IfStmt {
                cond,
                then_block,
                else_block,
            }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Types
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_type_ref(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        // Minimal v0.1:
        // - &T         => Ref(T)   (spelled as Ident "&"? we do not have '&' token; so omit)
        // - [T]        => List(T)
        // - fn(T,U)->R => Fn(...)
        // - Name       => Named(Ident)
        //
        // Since lexer does not provide '&' token, we omit ref-types in concrete syntax
        // for now; AST retains TypeRef::Ref for future extension.

        let start = self.peek().span.start;

        if self.peek_ident_text("fn") {
            // parse a function type: fn ( T, U ) -> R
            self.bump(); // ident "fn"
            self.expect(Token::LParen)?;
            let mut args = Vec::new();
            if !self.peek_is(Token::RParen) {
                loop {
                    args.push(self.parse_type_ref()?);
                    if self.peek_is(Token::Comma) {
                        self.bump();
                        continue;
                    }
                    break;
                }
            }
            self.expect(Token::RParen)?;
            self.expect(Token::Arrow)?;
            let ret = self.parse_type_ref()?;
            let end = ret.span.end;
            return Ok(Spanned::new(
                TypeRef::Fn(args, Box::new(ret)),
                Span::new(start, end),
            ));
        }

        if self.peek_is(Token::LBracket) {
            self.bump();
            let inner = self.parse_type_ref()?;
            let rb = self.expect(Token::RBracket)?;
            let end = rb.span.end;
            return Ok(Spanned::new(
                TypeRef::List(Box::new(inner)),
                Span::new(start, end),
            ));
        }

        // named type
        let id = self.expect_ident()?;
        let end = id.span.end;
        Ok(Spanned::new(TypeRef::Named(id), Span::new(start, end)))
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Expressions (precedence climbing)
    // ─────────────────────────────────────────────────────────────────────────

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_primary_expr()?;

        loop {
            // postfix: field access
            if self.peek_is(Token::Dot) {
                self.bump();
                let name = self.expect_ident()?;
                let span = Span::new(lhs.span.start, name.span.end);
                lhs = Spanned::new(
                    Expr::Field(FieldExpr {
                        base: Box::new(lhs),
                        name,
                    }),
                    span,
                );
                continue;
            }

            // postfix: call
            if self.peek_is(Token::LParen) {
                let call = self.parse_call_suffix(lhs)?;
                lhs = call;
                continue;
            }

            // binary
            let op = match self.peek().node {
                Token::Plus => Some(BinOp::Add),
                Token::Minus => Some(BinOp::Sub),
                Token::Star => Some(BinOp::Mul),
                Token::Slash => Some(BinOp::Div),
                Token::EqEq => Some(BinOp::Eq),
                Token::Lt => Some(BinOp::Lt),
                Token::Lte => Some(BinOp::Le),
                Token::Gt => Some(BinOp::Gt),
                Token::Gte => Some(BinOp::Ge),
                Token::AndAnd => Some(BinOp::And),
                Token::OrOr => Some(BinOp::Or),
                _ => None,
            };

            let Some(op) = op else { break };

            let (l_bp, r_bp) = infix_binding_power(op);
            if l_bp < min_bp {
                break;
            }

            let op_tok = self.bump();
            let rhs = self.parse_expr_bp(r_bp)?;

            let span = Span::new(lhs.span.start, rhs.span.end);
            lhs = Spanned::new(
                Expr::Binary(BinaryExpr {
                    op,
                    left: Box::new(lhs),
                    right: Box::new(rhs),
                    op_span: op_tok.span,
                }),
                span,
            );
        }

        Ok(lhs)
    }

    fn parse_call_suffix(&mut self, callee: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = callee.span.start;
        self.expect(Token::LParen)?;
        let mut args = Vec::new();
        if !self.peek_is(Token::RParen) {
            loop {
                args.push(self.parse_expr()?);
                if self.peek_is(Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        let rp = self.expect(Token::RParen)?;
        let end = rp.span.end;

        Ok(Spanned::new(
            Expr::Call(CallExpr {
                callee: Box::new(callee),
                args,
            }),
            Span::new(start, end),
        ))
    }

    fn parse_primary_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek();

        // literals
        if matches!(t.node, Token::Int(_) | Token::String(_) | Token::Bool(_)) {
            let lit = self.parse_literal()?;
            let span = lit.span;
            return Ok(Spanned::new(Expr::Literal(lit.node), span));
        }

        // parenthesized
        if self.peek_is(Token::LParen) {
            self.bump();
            let e = self.parse_expr()?;
            self.expect(Token::RParen)?;
            return Ok(e);
        }

        // proc path as a callee/ident-like expression:
        // We represent `K::foo` as Ident("K::foo") at v0.1 expression level to avoid
        // introducing a separate expression variant. Calls like `K::foo()` remain valid.
        if matches!(t.node, Token::Keyword(Keyword::K | Keyword::Q | Keyword::Phi)) {
            let path = self.parse_proc_path()?;
            let span = path.span;
            let text = match path.node.regime.node {
                Regime::K => format!("K::{}", path.node.name.text),
                Regime::Q => format!("Q::{}", path.node.name.text),
                Regime::Phi => format!("Φ::{}", path.node.name.text),
            };
            let id = Ident::new(text, span);
            return Ok(Spanned::new(Expr::Ident(id.clone()), id.span));
        }

        // identifier
        if let Token::Ident(_) = &t.node {
            let id = self.expect_ident()?;
            let span = id.span;
            return Ok(Spanned::new(Expr::Ident(id), span));
        }

        Err(self.err_here(
            ParseErrorKind::UnexpectedToken,
            "expected expression",
        ))
    }

    fn parse_literal(&mut self) -> Result<Spanned<Literal>, ParseError> {
        let t = self.peek();
        match &t.node {
            Token::Int(n) => {
                let tok = self.bump();
                Ok(Spanned::new(Literal::Int(*n), tok.span))
            }
            Token::String(s) => {
                let tok = self.bump();
                Ok(Spanned::new(Literal::String(s.clone()), tok.span))
            }
            Token::Bool(b) => {
                let tok = self.bump();
                Ok(Spanned::new(Literal::Bool(*b), tok.span))
            }
            _ => Err(self.err_here(
                ParseErrorKind::ExpectedLiteral,
                "expected literal",
            )),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────
    // Token utilities
    // ─────────────────────────────────────────────────────────────────────────

    fn peek(&self) -> &Spanned<Token> {
        self.tokens
            .get(self.i)
            .unwrap_or_else(|| self.tokens.last().expect("token stream must be non-empty"))
    }

    fn bump(&mut self) -> Spanned<Token> {
        let t = self.peek().clone();
        if !self.is_eof() {
            self.i += 1;
        }
        t
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek().node, Token::Eof)
    }

    fn last_end(&self) -> u32 {
        self.tokens.last().map(|t| t.span.end).unwrap_or(0)
    }

    fn peek_is(&self, want: Token) -> bool {
        self.peek().node == want
    }

    fn peek_is_kw(&self, want: Keyword) -> bool {
        matches!(self.peek().node, Token::Keyword(k) if k == want)
    }

    fn peek_ident_text(&self, text: &str) -> bool {
        matches!(&self.peek().node, Token::Ident(s) if s == text)
    }

    fn expect(&mut self, want: Token) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        if t.node == want {
            Ok(self.bump())
        } else if matches!(t.node, Token::Eof) {
            Err(self.err_here(ParseErrorKind::UnexpectedEof, "unexpected end of file"))
        } else {
            Err(self.err_here(
                ParseErrorKind::ExpectedToken,
                format!("expected token: {want:?}").as_str(),
            ))
        }
    }

    fn expect_kw(&mut self, want: Keyword) -> Result<Spanned<Token>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Keyword(k) if *k == want => Ok(self.bump()),
            Token::Eof => Err(self.err_here(ParseErrorKind::UnexpectedEof, "unexpected end of file")),
            _ => Err(self.err_here(
                ParseErrorKind::ExpectedToken,
                "expected keyword",
            )),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Ident(s) => {
                self.bump();
                Ok(Ident::new(s.clone(), t.span))
            }
            Token::Eof => Err(self.err_here(ParseErrorKind::UnexpectedEof, "unexpected end of file")),
            _ => Err(self.err_here(ParseErrorKind::ExpectedIdent, "expected identifier")),
        }
    }

    fn expect_ident_text(&mut self, text: &str) -> Result<Ident, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Ident(s) if s == text => {
                self.bump();
                Ok(Ident::new(s.clone(), t.span))
            }
            _ => Err(self.err_here(ParseErrorKind::ExpectedIdent, "expected identifier")),
        }
    }

    fn err_here(&self, kind: ParseErrorKind, msg: &str) -> ParseError {
        let s = self.peek().span;
        ParseError::new(kind, s, msg)
    }

    fn err_at(&self, kind: ParseErrorKind, span: Span, msg: &str) -> ParseError {
        ParseError::new(kind, span, msg)
    }
}

// precedence table
fn infix_binding_power(op: BinOp) -> (u8, u8) {
    match op {
        BinOp::Or => (1, 2),
        BinOp::And => (3, 4),
        BinOp::Eq | BinOp::Neq => (5, 6),
        BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => (7, 8),
        BinOp::Add | BinOp::Sub => (9, 10),
        BinOp::Mul | BinOp::Div => (11, 12),
    }
}

// helper: map on Spanned<T> without importing traits
trait SpannedMap<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U>;
}

impl<T> SpannedMap<T> for Spanned<T> {
    fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned::new(f(self.node), self.span)
    }
}