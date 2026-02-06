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
            _ => Err(self.err_here("expected forge item")),
        }
    }

    // ─────────────────────────────────────────────────────────────
    // Shorthand proc (NEW)
    // ─────────────────────────────────────────────────────────────

    fn parse_proc_shorthand(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        // Syntax:  K main { ... }
        let regime = self.parse_regime()?;
        let name = self.expect_ident()?;
        let start = regime.span.start;

        let body = self.parse_block()?;
        let end = body.span.end;

        let path = Spanned::new(
            ProcPath { regime, name },
            Span::new(start, end),
        );

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
            Token::Keyword(Keyword::Emit) => {
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
            _ => Err(self.err_here("unsupported statement")),
        }
    }

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        match t.node {
            Token::String(s) => {
                let sp = self.bump().span;
                Ok(Spanned::new(Expr::Literal(Literal::String(s)), sp))
            }
            _ => Err(self.err_here("expected expression")),
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
}}