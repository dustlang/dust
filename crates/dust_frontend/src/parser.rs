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
        write!(
            f,
            "{} at {}..{}",
            self.message, self.span.start, self.span.end
        )
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

    // ─────────────────────────────────────────────────────────────
    // Entry point
    // ─────────────────────────────────────────────────────────────

    pub fn parse_file(&mut self) -> Result<FileAst, ParseError> {
        let mut forges: Vec<Spanned<ForgeDecl>> = Vec::new();

        // Implicit root forge for shorthand procs
        let mut root_items: Vec<Spanned<Item>> = Vec::new();
        let mut root_start: Option<u32> = None;
        let mut root_end: u32 = 0;

        while !self.is_eof() {
            match &self.peek().node {
                Token::Keyword(Keyword::Forge) => {
                    // Flush any accumulated shorthand procs
                    if !root_items.is_empty() {
                        let start = root_start.unwrap_or(0);
                        let name = Ident::new("__root__", Span::new(start, start));
                        forges.push(Spanned::new(
                            ForgeDecl {
                                name,
                                items: std::mem::take(&mut root_items),
                            },
                            Span::new(start, root_end),
                        ));
                        root_start = None;
                        root_end = 0;
                    }

                    forges.push(self.parse_forge()?);
                }

                // Top-level shorthand proc: K/Q/Φ main { ... }
                Token::Keyword(Keyword::K)
                | Token::Keyword(Keyword::Q)
                | Token::Keyword(Keyword::Phi) => {
                    let p = self.parse_proc_shorthand()?;
                    if root_start.is_none() {
                        root_start = Some(p.span.start);
                    }
                    root_end = p.span.end;
                    root_items.push(Spanned::new(Item::Proc(p.node), p.span));
                }

                _ => return Err(self.err_here("expected `forge` or top-level `K/Q/Φ` proc")),
            }
        }

        // Flush trailing root forge
        if !root_items.is_empty() {
            let start = root_start.unwrap_or(0);
            let name = Ident::new("__root__", Span::new(start, start));
            forges.push(Spanned::new(
                ForgeDecl {
                    name,
                    items: root_items,
                },
                Span::new(start, root_end),
            ));
        }

        Ok(FileAst { forges })
    }

    // ─────────────────────────────────────────────────────────────
    // Forge parsing
    // ─────────────────────────────────────────────────────────────

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
            Token::Keyword(Keyword::Proc) => {
                let p = self.parse_proc()?;
                Ok(Spanned::new(Item::Proc(p.node), p.span))
            }
            // If you want a better error message than “expected forge item”
            Token::Keyword(Keyword::Shape) => {
                Err(self.err_here("`shape` is not supported by this parser build"))
            }
            Token::Keyword(Keyword::Bind) => {
                Err(self.err_here("`bind` is not supported by this parser build"))
            }
            _ => Err(self.err_here("expected forge item (`proc`)")),
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Shorthand proc (expanded for v0.2)
    // ─────────────────────────────────────────────────────────────

    fn parse_proc_shorthand(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        // Syntax:  K main { ... }  or  K main(params) -> ReturnType { ... }
        let regime = self.parse_regime()?;
        let name = self.expect_ident()?;
        let start = regime.span.start;

        // Parse parameters if present
        let params = if self.peek_is(&Token::LParen) {
            self.bump();
            let mut params = Vec::new();
            while !self.peek_is(&Token::RParen) {
                let param_name = self.expect_ident()?;
                self.expect(Token::Colon)?;
                let param_type = self.parse_type()?;
                params.push(Spanned::new(
                    ParamDecl {
                        name: param_name,
                        ty: param_type,
                    },
                    Span::default(),
                ));
                if !self.peek_is(&Token::RParen) {
                    self.expect(Token::Comma)?;
                }
            }
            self.expect(Token::RParen)?;
            params
        } else {
            Vec::new()
        };

        // Parse return type if present
        let ret = if self.peek_is(&Token::Arrow) {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };

        let body = self.parse_block()?;
        let end = body.span.end;

        let path = Spanned::new(ProcPath { regime, name }, Span::new(start, end));

        let sig = Spanned::new(
            ProcSig {
                path,
                params,
                uses: Vec::new(),
                ret,
                qualifiers: Vec::new(),
            },
            Span::new(start, end),
        );

        Ok(Spanned::new(ProcDecl { sig, body }, Span::new(start, end)))
    }

    // ─────────────────────────────────────────────────────────────
    // Normal proc parsing (unchanged)
    // ─────────────────────────────────────────────────────────────

    fn parse_proc(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        let start = self.expect_kw(Keyword::Proc)?.span.start;
        let path = self.parse_proc_path()?;
        self.expect(Token::LParen)?;
        self.expect(Token::RParen)?;
        let body = self.parse_block()?;
        let end = body.span.end;

        let sig = Spanned::new(
            ProcSig {
                path,
                params: Vec::new(),
                uses: Vec::new(),
                ret: None,
                qualifiers: Vec::new(),
            },
            Span::new(start, end),
        );

        Ok(Spanned::new(ProcDecl { sig, body }, Span::new(start, end)))
    }

    // ─────────────────────────────────────────────────────────────
    // Shared helpers
    // ─────────────────────────────────────────────────────────────

    fn parse_regime(&mut self) -> Result<Spanned<Regime>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::Keyword(Keyword::K) => Ok(Spanned::new(Regime::K, self.bump().span)),
            Token::Keyword(Keyword::Q) => Ok(Spanned::new(Regime::Q, self.bump().span)),
            Token::Keyword(Keyword::Phi) => Ok(Spanned::new(Regime::Phi, self.bump().span)),
            _ => Err(self.err_here("expected regime K/Q/Φ")),
        }
    }

    fn parse_proc_path(&mut self) -> Result<Spanned<ProcPath>, ParseError> {
        let reg = self.parse_regime()?;
        self.expect(Token::ColonColon)?;
        let name = self.expect_ident()?;
        let span = Span::new(reg.span.start, name.span.end);
        Ok(Spanned::new(ProcPath { regime: reg, name }, span))
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
        match &self.peek().node {
            Token::Keyword(Keyword::Let) => self.parse_let_stmt(false),
            Token::Keyword(Keyword::Mut) => self.parse_let_stmt(true),
            Token::Keyword(Keyword::Emit) => self.parse_effect_stmt(),
            Token::Keyword(Keyword::Return) => self.parse_return_stmt(),
            Token::Keyword(Keyword::If) => self.parse_if_stmt(),
            Token::Keyword(Keyword::For) => self.parse_for_stmt(),
            Token::Keyword(Keyword::While) => self.parse_while_stmt(),
            Token::Keyword(Keyword::Break) => self.parse_break_stmt(),
            Token::Keyword(Keyword::Continue) => self.parse_continue_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Variable statements (let, mut let)
    // ─────────────────────────────────────────────────────────────

    fn parse_let_stmt(&mut self, mutable: bool) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.bump().span.start;

        let name = self.expect_ident()?;

        let ty = if self.peek_is(&Token::Colon) {
            self.bump();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(Token::Eq)?;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;

        let stmt = if mutable {
            Stmt::MutLet(MutLetStmt { name, ty, expr })
        } else {
            Stmt::Let(LetStmt { name, ty, expr })
        };

        Ok(Spanned::new(stmt, Span::new(start, end)))
    }

    // ─────────────────────────────────────────────────────────────
    // Effect statement (emit)
    // ─────────────────────────────────────────────────────────────

    fn parse_effect_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.bump().span.start;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Effect(EffectStmt {
                kind: Spanned::new(EffectKind::Emit, Span::new(start, start)),
                payload: expr,
            }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // Return statement
    // ─────────────────────────────────────────────────────────────

    fn parse_return_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.bump().span.start;
        let expr = self.parse_expr()?;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Return(ReturnStmt { expr }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // If statement
    // ─────────────────────────────────────────────────────────────

    fn parse_if_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::If)?.span.start;

        let condition = self.parse_expr()?;
        let then_block = self.parse_block()?;

        let else_block = if self.peek_is(&Token::Keyword(Keyword::Else)) {
            self.bump();
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
                condition,
                then_block,
                else_block,
            }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // For statement
    // ─────────────────────────────────────────────────────────────

    fn parse_for_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::For)?.span.start;

        let var = self.expect_ident()?;
        self.expect(Token::Keyword(Keyword::In))?;

        let start_expr = self.parse_expr()?;
        self.expect(Token::DotDot)?;
        let end_expr = self.parse_expr()?;

        let body = self.parse_block()?;
        let end = body.span.end;

        Ok(Spanned::new(
            Stmt::For(ForStmt {
                var,
                start: start_expr,
                end: end_expr,
                body,
            }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // While statement
    // ─────────────────────────────────────────────────────────────

    fn parse_while_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::While)?.span.start;

        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        let end = body.span.end;

        Ok(Spanned::new(
            Stmt::While(WhileStmt { condition, body }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // Break/Continue statements
    // ─────────────────────────────────────────────────────────────

    fn parse_break_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Break)?.span.start;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(Stmt::Break(BreakStmt), Span::new(start, end)))
    }

    fn parse_continue_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::Continue)?.span.start;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Continue(ContinueStmt),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // Match expression (v0.2)
    // ─────────────────────────────────────────────────────────────

    fn parse_match_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.expect_kw(Keyword::Match)?.span.start;

        let expr = self.parse_expr()?;
        self.expect(Token::LBrace)?;

        let mut arms = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            let pattern = self.parse_match_pattern()?;
            self.expect(Token::FatArrow)?;
            let body = self.parse_expr()?;

            arms.push(Spanned::new(MatchArm { pattern, body }, Span::default()));

            if !self.peek_is(&Token::RBrace) {
                self.expect(Token::Comma)?;
            }
        }

        self.expect(Token::RBrace)?;
        let end = self.prev_span().end;

        let match_expr = Spanned::new(MatchExpr { expr, arms }, Span::new(start, end));

        Ok(Spanned::new(
            Expr::Match(Box::new(match_expr)),
            Span::new(start, end),
        ))
    }

    fn parse_match_pattern(&mut self) -> Result<Spanned<MatchPattern>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Int(n) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(MatchPattern::Literal(Literal::Int(*n)), sp))
            }
            Token::Bool(b) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(MatchPattern::Literal(Literal::Bool(*b)), sp))
            }
            Token::Ident(s) => {
                self.bump();
                let sp = self.prev_span();
                let ident = Ident::new(s.clone(), sp);
                Ok(Spanned::new(MatchPattern::Ident(ident), sp))
            }
            Token::Underscore => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(MatchPattern::Wildcard, sp))
            }
            _ => Err(self.err_here("expected pattern")),
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Expression statement
    // ─────────────────────────────────────────────────────────────

    fn parse_expr_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let expr = self.parse_expr()?;
        let start = expr.span.start;
        self.expect(Token::Semi)?;
        let end = self.prev_span().end;
        Ok(Spanned::new(
            Stmt::Expr(ExprStmt { expr }),
            Span::new(start, end),
        ))
    }

    // ─────────────────────────────────────────────────────────────
    // Expression parsing
    // ─────────────────────────────────────────────────────────────

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_binary_expr()
    }

    fn parse_binary_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_unary_expr()
    }

    fn parse_unary_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        if self.peek_is(&Token::Bang) {
            let start = self.bump().span.start;
            let operand = self.parse_unary_expr()?;
            let end = operand.span.end;
            let unary_expr = Spanned::new(
                UnaryExpr {
                    op: Spanned::new(UnOp::Not, Span::new(start, start)),
                    operand,
                },
                Span::new(start, end),
            );
            return Ok(Spanned::new(
                Expr::Unary(Box::new(unary_expr)),
                Span::new(start, end),
            ));
        }
        if self.peek_is(&Token::Minus) {
            let start = self.bump().span.start;
            let operand = self.parse_unary_expr()?;
            let end = operand.span.end;
            let unary_expr = Spanned::new(
                UnaryExpr {
                    op: Spanned::new(UnOp::Neg, Span::new(start, start)),
                    operand,
                },
                Span::new(start, end),
            );
            return Ok(Spanned::new(
                Expr::Unary(Box::new(unary_expr)),
                Span::new(start, end),
            ));
        }
        self.parse_postfix_expr()
    }

    fn parse_postfix_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut expr = self.parse_primary_expr()?;

        loop {
            if self.peek_is(&Token::LParen) {
                expr = self.parse_call_expr(expr)?;
            } else if self.peek_is(&Token::Dot) {
                expr = self.parse_field_expr(expr)?;
            } else if self.peek_is(&Token::LBracket) {
                expr = self.parse_index_expr(expr)?;
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_call_expr(&mut self, callee: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = callee.span.start;
        self.bump(); // consume (

        let mut args = Vec::new();
        while !self.peek_is(&Token::RParen) {
            args.push(self.parse_expr()?);
            if !self.peek_is(&Token::RParen) {
                self.expect(Token::Comma)?;
            }
        }

        let end = self.expect(Token::RParen)?.span.end;

        let call_expr = Spanned::new(CallExpr { callee, args }, Span::new(start, end));

        Ok(Spanned::new(
            Expr::Call(Box::new(call_expr)),
            Span::new(start, end),
        ))
    }

    fn parse_field_expr(&mut self, base: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = base.span.start;
        self.bump(); // consume .
        let field = self.expect_ident()?;
        let end = field.span.end;

        let field_expr = Spanned::new(FieldExpr { base, field }, Span::new(start, end));

        Ok(Spanned::new(
            Expr::Field(Box::new(field_expr)),
            Span::new(start, end),
        ))
    }

    fn parse_index_expr(&mut self, base: Spanned<Expr>) -> Result<Spanned<Expr>, ParseError> {
        let start = base.span.start;
        self.bump(); // consume [
        let index = self.parse_expr()?;
        let end = self.expect(Token::RBracket)?.span.end;

        let index_expr = Spanned::new(IndexExpr { base, index }, Span::new(start, end));

        Ok(Spanned::new(
            Expr::Index(Box::new(index_expr)),
            Span::new(start, end),
        ))
    }

    fn parse_primary_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Int(n) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(Expr::Literal(Literal::Int(*n)), sp))
            }
            Token::Float(f) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(Expr::Literal(Literal::Float(*f)), sp))
            }
            Token::Char(c) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(Expr::Literal(Literal::Char(*c)), sp))
            }
            Token::String(s) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(Expr::Literal(Literal::String(s.clone())), sp))
            }
            Token::Bool(b) => {
                self.bump();
                let sp = self.prev_span();
                Ok(Spanned::new(Expr::Literal(Literal::Bool(*b)), sp))
            }
            Token::Ident(_) => {
                self.bump();
                let sp = self.prev_span();
                let name = Ident::new(t.node.clone().unwrap_ident(), sp);
                Ok(Spanned::new(Expr::Ident(name), sp))
            }
            Token::LParen => {
                self.bump();
                let expr = self.parse_expr()?;
                self.expect(Token::RParen)?;
                let span = Span::new(t.span.start, self.prev_span().end);
                Ok(Spanned::new(Expr::Paren(Box::new(expr)), span))
            }
            Token::LBracket => self.parse_array_or_slice(),
            Token::LBrace => self.parse_struct_or_block(),
            _ => Err(self.err_here("expected expression")),
        }
    }

    fn parse_array_or_slice(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.bump().span.start;

        let mut elements = Vec::new();
        while !self.peek_is(&Token::RBracket) {
            elements.push(self.parse_expr()?);
            if !self.peek_is(&Token::RBracket) {
                self.expect(Token::Comma)?;
            }
        }

        let end = self.expect(Token::RBracket)?.span.end;

        Ok(Spanned::new(Expr::Array(elements), Span::new(start, end)))
    }

    fn parse_struct_or_block(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let start = self.peek().span.start;

        // Could be a struct literal or a block - look ahead
        // For now, treat as block
        self.parse_block_expr()
    }

    fn parse_block_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let block = self.parse_block()?;
        Ok(Spanned::new(Expr::Block(block.node), block.span))
    }

    // ─────────────────────────────────────────────────────────────
    // Type parsing
    // ─────────────────────────────────────────────────────────────

    fn parse_type(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Ident(s) => {
                self.bump();
                let span = self.prev_span();
                Ok(Spanned::new(
                    TypeRef::Named(Ident::new(s.clone(), span)),
                    span,
                ))
            }
            Token::LBracket => {
                let start = self.bump().span.start;
                let elem = self.parse_type()?;
                self.expect(Token::RBracket)?;
                let end = self.prev_span().end;
                Ok(Spanned::new(
                    TypeRef::Array {
                        elem: Box::new(elem),
                        len: 0,
                    },
                    Span::new(start, end),
                ))
            }
            _ => Err(self.err_here("expected type")),
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Low-level helpers
    // ─────────────────────────────────────────────────────────────

    fn peek(&self) -> &Spanned<Token> {
        &self.toks[self.i]
    }

    fn bump(&mut self) -> Spanned<Token> {
        let t = self.peek().clone();
        self.i += 1;
        t
    }

    fn prev_span(&self) -> Span {
        self.toks[self.i - 1].span
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek().node, Token::Eof)
    }

    fn peek_is(&self, want: &Token) -> bool {
        &self.peek().node == want
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
        match t.node {
            Token::Keyword(k) if k == want => Ok(self.bump()),
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
