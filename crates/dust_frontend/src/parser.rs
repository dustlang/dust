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
            Token::Keyword(Keyword::Let) => {
                let l = self.parse_let()?;
                Ok(Spanned::new(Item::Let(l.node), l.span))
            }
            Token::Keyword(Keyword::Contract) => {
                let c = self.parse_contract()?;
                Ok(Spanned::new(Item::Contract(c.node), c.span))
            }
            Token::Keyword(Keyword::Fn) => {
                let f = self.parse_fn()?;
                Ok(Spanned::new(Item::Fn(f.node), f.span))
            }
            Token::Keyword(Keyword::Use) => {
                let u = self.parse_use()?;
                Ok(Spanned::new(Item::Use(u.node), u.span))
            }
            Token::Keyword(Keyword::Emit) => {
                let e = self.parse_emit()?;
                Ok(Spanned::new(Item::Emit(e.node), e.span))
            }
            Token::Keyword(Keyword::Assert) => {
                let a = self.parse_assert()?;
                Ok(Spanned::new(Item::Assert(a.node), a.span))
            }
            Token::Keyword(Keyword::If) => {
                let s = self.parse_if_stmt()?;
                Ok(Spanned::new(Item::Stmt(s.node), s.span))
            }
            Token::Keyword(Keyword::While) => {
                let s = self.parse_while_stmt()?;
                Ok(Spanned::new(Item::Stmt(s.node), s.span))
            }
            Token::Keyword(Keyword::Return) => {
                let s = self.parse_return_stmt()?;
                Ok(Spanned::new(Item::Stmt(s.node), s.span))
            }
            Token::Keyword(Keyword::Fail) => {
                let s = self.parse_fail_stmt()?;
                Ok(Spanned::new(Item::Stmt(s.node), s.span))
            }
            _ => Err(self.err_here("expected item (shape/proc/bind/let/contract/fn/use/emit/assert/stmt)")),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Decls
    // ─────────────────────────────────────────────────────────────────────────────

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
        let start = self.expect_kw(Keyword::Proc)?.span.start;

        // proc <path> <regime> (<args>) : <type> { <body> }
        let path = self.parse_proc_path()?;
        let regime = self.parse_regime()?;

        self.expect(Token::LParen)?;
        let args = self.parse_named_args()?;
        self.expect(Token::RParen)?;

        self.expect(Token::Colon)?;
        let ret_ty = self.parse_type_ref()?;

        let body = self.parse_block()?;
        let end = body.span.end;

        Ok(Spanned::new(
            ProcDecl {
                path,
                regime,
                args,
                ret_ty,
                body,
            },
            Span::new(start, end),
        ))
    }

    fn parse_bind(&mut self) -> Result<Spanned<BindDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Bind)?.span.start;

        // bind <name> = <proc_path> ;
        let name = self.expect_ident()?;
        self.expect(Token::Eq)?;
        let target = self.parse_proc_path()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;

        Ok(Spanned::new(
            BindDecl { name, target },
            Span::new(start, end),
        ))
    }

    fn parse_let(&mut self) -> Result<Spanned<LetDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Let)?.span.start;

        // let <ident> : <type> = <expr> ;
        let name = self.expect_ident()?;
        let name_end = name.span.end;

        self.expect(Token::Colon)?;
        let ty = self.parse_type_ref()?;

        self.expect(Token::Eq)?;
        let value = self.parse_expr()?;

        self.expect(Token::Semi)?;
        let end = self.prev_span().end;

        Ok(Spanned::new(
            LetDecl { name, ty, value },
            Span::new(start, end.max(name_end)),
        ))
    }

    fn parse_contract(&mut self) -> Result<Spanned<ContractDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Contract)?.span.start;

        // contract <ident> { requires: <expr>; ensures: <expr>; }
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;

        self.expect_kw(Keyword::Requires)?;
        self.expect(Token::Colon)?;
        let requires = self.parse_expr()?;
        self.expect(Token::Semi)?;

        self.expect_kw(Keyword::Ensures)?;
        self.expect(Token::Colon)?;
        let ensures = self.parse_expr()?;
        self.expect(Token::Semi)?;

        let end = self.expect(Token::RBrace)?.span.end;

        Ok(Spanned::new(
            ContractDecl {
                name,
                requires,
                ensures,
            },
            Span::new(start, end),
        ))
    }

    fn parse_fn(&mut self) -> Result<Spanned<FnDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Fn)?.span.start;

        // fn <ident> ( <params> ) : <type> { <body> }
        let name = self.expect_ident()?;

        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;

        self.expect(Token::Colon)?;
        let ret_ty = self.parse_type_ref()?;

        let body = self.parse_block()?;
        let end = body.span.end;

        Ok(Spanned::new(
            FnDecl {
                name,
                params,
                ret_ty,
                body,
            },
            Span::new(start, end),
        ))
    }

    fn parse_use(&mut self) -> Result<Spanned<UseDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Use)?.span.start;
        let path = self.parse_proc_path()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(UseDecl { path }, Span::new(start, end)))
    }

    fn parse_emit(&mut self) -> Result<Spanned<EmitStmt>, ParseError> {
        let start = self.expect_kw(Keyword::Emit)?.span.start;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(EmitStmt { expr }, Span::new(start, end)))
    }

    fn parse_assert(&mut self) -> Result<Spanned<AssertStmt>, ParseError> {
        let start = self.expect_kw(Keyword::Assert)?.span.start;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            AssertStmt { expr },
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Paths, regimes, params
    // ─────────────────────────────────────────────────────────────────────────────

    fn parse_proc_path(&mut self) -> Result<Spanned<ProcPath>, ParseError> {
        // <Ident> :: <Ident>
        let start = self.expect_ident()?.span.start;
        self.expect(Token::ColonColon)?;
        let name = self.expect_ident()?;
        let end = name.span.end;

        Ok(Spanned::new(
            ProcPath {
                module: Ident::synthetic("<forge>", Span::new(start, start)),
                name,
            },
            Span::new(start, end),
        ))
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

    fn parse_params(&mut self) -> Result<Vec<Spanned<ParamDecl>>, ParseError> {
        let mut params = Vec::new();
        if self.peek_is(&Token::RParen) {
            return Ok(params);
        }
        loop {
            let p_start = self.peek().span.start;
            let name = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.parse_type_ref()?;
            let p_end = ty.span.end;

            params.push(Spanned::new(
                ParamDecl { name, ty },
                Span::new(p_start, p_end),
            ));

            if self.peek_is(&Token::Comma) {
                self.bump();
                continue;
            }
            break;
        }
        Ok(params)
    }

    fn parse_named_args(&mut self) -> Result<Vec<Spanned<NamedArg>>, ParseError> {
        let mut args = Vec::new();
        if self.peek_is(&Token::RParen) {
            return Ok(args);
        }
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
        Ok(args)
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Types
    // ─────────────────────────────────────────────────────────────────────────────

    fn parse_type_ref(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Ident(_) => {
                let id = self.expect_ident()?;
                Ok(Spanned::new(TypeRef::Named(id.clone()), id.span))
            }
            Token::Keyword(Keyword::I32) => {
                let sp = self.bump().span;
                Ok(Spanned::new(TypeRef::I32, sp))
            }
            Token::Keyword(Keyword::I64) => {
                let sp = self.bump().span;
                Ok(Spanned::new(TypeRef::I64, sp))
            }
            Token::Keyword(Keyword::U32) => {
                let sp = self.bump().span;
                Ok(Spanned::new(TypeRef::U32, sp))
            }
            Token::Keyword(Keyword::U64) => {
                let sp = self.bump().span;
                Ok(Spanned::new(TypeRef::U64, sp))
            }
            Token::Keyword(Keyword::Bool) => {
                let sp = self.bump().span;
                Ok(Spanned::new(TypeRef::Bool, sp))
            }
            Token::Keyword(Keyword::Str) => {
                let sp = self.bump().span;
                Ok(Spanned::new(TypeRef::Str, sp))
            }
            _ => Err(self.err_here("expected type")),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Blocks & statements
    // ─────────────────────────────────────────────────────────────────────────────

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
            Token::Keyword(Keyword::Let) => {
                let l = self.parse_let()?;
                Ok(Spanned::new(Stmt::Let(l.node), l.span))
            }
            Token::Keyword(Keyword::Emit) => {
                let e = self.parse_emit()?;
                Ok(Spanned::new(Stmt::Emit(e.node), e.span))
            }
            Token::Keyword(Keyword::Assert) => {
                let a = self.parse_assert()?;
                Ok(Spanned::new(Stmt::Assert(a.node), a.span))
            }
            Token::Keyword(Keyword::If) => self.parse_if_stmt(),
            Token::Keyword(Keyword::While) => self.parse_while_stmt(),
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(),
            Token::Keyword(Keyword::Fail) => self.parse_fail_stmt(),
            _ => {
                // expr ;
                let start = self.peek().span.start;
                let expr = self.parse_expr()?;
                self.expect(Token::Semi)?;
                let end = self.prev_span().end;
                Ok(Spanned::new(
                    Stmt::Expr(expr),
                    Span::new(start, end),
                ))
            }
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::If)?.span.start;
        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;
        let mut else_block = None;
        let end;
        if self.peek_kw(Keyword::Else) {
            self.bump(); // else
            let eb = self.parse_block()?;
            end = eb.span.end;
            else_block = Some(eb);
        } else {
            end = then_block.span.end;
        }

        Ok(Spanned::new(
            Stmt::If(IfStmt {
                cond,
                then_block,
                else_block,
            }),
            Span::new(start, end),
        ))
    }

    fn parse_while_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::While)?.span.start;
        let cond = self.parse_expr()?;
        let body = self.parse_block()?;
        let end = body.span.end;
        Ok(Spanned::new(
            Stmt::While(WhileStmt { cond, body }),
            Span::new(start, end),
        ))
    }

    fn parse_return_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Return)?.span.start;
        let mut value = None;
        if !self.peek_is(&Token::Semi) {
            value = Some(self.parse_expr()?);
        }
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Return(ReturnStmt { value }),
            Span::new(start, end),
        ))
    }

    fn parse_fail_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Fail)?.span.start;
        let msg = self.parse_literal()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Fail(FailStmt { msg }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Expressions
    // ─────────────────────────────────────────────────────────────────────────────

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_expr_bp(0)
    }

    fn parse_expr_bp(&mut self, min_bp: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_primary()?;

        loop {
            let op = match self.peek().node.clone() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::EqEq => BinOp::Eq,
                Token::BangEq => BinOp::Ne,
                Token::Lt => BinOp::Lt,
                Token::LtEq => BinOp::Le,
                Token::Gt => BinOp::Gt,
                Token::GtEq => BinOp::Ge,
                Token::AndAnd => BinOp::And,
                Token::OrOr => BinOp::Or,
                _ => break,
            };

            let (l_bp, r_bp) = op.binding_power();
            if l_bp < min_bp {
                break;
            }

            let op_span = self.bump().span;
            let rhs = self.parse_expr_bp(r_bp)?;
            let span = Span::new(lhs.span.start, rhs.span.end);
            lhs = Spanned::new(
                Expr::Binary(BinaryExpr {
                    op,
                    op_span,
                    lhs,
                    rhs,
                }),
                span,
            );
        }

        Ok(lhs)
    }

    fn parse_primary(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Int(_) | Token::StrLit(_) | Token::BoolLit(_) => {
                let lit = self.parse_literal()?;
                Ok(Spanned::new(Expr::Literal(lit), lit.span))
            }
            Token::Ident(_) => {
                let id = self.expect_ident()?;
                Ok(Spanned::new(Expr::Ident(id.clone()), id.span))
            }
            Token::LParen => {
                let start = self.bump().span.start;
                let e = self.parse_expr()?;
                let end = self.expect(Token::RParen)?.span.end;
                Ok(Spanned::new(e.node, Span::new(start, end)))
            }
            _ => Err(self.err_here("expected expression")),
        }
    }

    fn parse_literal(&mut self) -> Result<Spanned<Literal>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Int(v) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Literal::Int(v), sp))
            }
            Token::StrLit(s) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Literal::Str(s), sp))
            }
            Token::BoolLit(b) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Literal::Bool(b), sp))
            }
            _ => Err(self.err_here("expected literal")),
        }
    }

    // ─────────────────────────────────────────────────────────────────────────────
    // Helpers
    // ─────────────────────────────────────────────────────────────────────────────

    fn peek(&self) -> &Spanned<Token> {
        self.toks.get(self.i).unwrap_or_else(|| self.toks.last().expect("non-empty token stream"))
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
        matches!(self.peek().node, Token::Keyword(k) if k == want)
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
                Ok(Ident { name: s, span: sp })
            }
            _ => Err(ParseError {
                message: "expected identifier".to_string(),
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