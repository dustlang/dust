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

    fn peek(&self) -> &Spanned<Token> {
        self.toks.get(self.i).unwrap_or_else(|| self.toks.last().expect("non-empty tokens"))
    }

    fn bump(&mut self) -> Spanned<Token> {
        let t = self.peek().clone();
        if self.i < self.toks.len() {
            self.i += 1;
        }
        t
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek().node, Token::Eof)
    }

    fn err<T>(&self, span: Span, msg: impl Into<String>) -> Result<T, ParseError> {
        Err(ParseError {
            message: msg.into(),
            span,
        })
    }

    fn expect(&mut self, want: Token) -> Result<Spanned<Token>, ParseError> {
        let t = self.bump();
        if t.node == want {
            Ok(t)
        } else {
            self.err(t.span, format!("expected {:?}, found {:?}", want, t.node))
        }
    }

    fn expect_kw(&mut self, want: Keyword) -> Result<Spanned<Token>, ParseError> {
        let t = self.bump();
        match t.node {
            Token::Keyword(k) if k == want => Ok(t),
            _ => self.err(t.span, format!("expected keyword {:?}, found {:?}", want, t.node)),
        }
    }

    fn peek_is(&self, want: &Token) -> bool {
        &self.peek().node == want
    }

    fn peek_is_kw(&self, want: Keyword) -> bool {
        matches!(&self.peek().node, Token::Keyword(k) if *k == want)
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let t = self.bump();
        match t.node {
            Token::Ident(s) => Ok(Ident::new(s, t.span)),
            _ => self.err(t.span, format!("expected identifier, found {:?}", t.node)),
        }
    }

    // ---------- entry ----------

    pub fn parse_file(&mut self) -> Result<File, ParseError> {
        let mut items = Vec::new();
        while !self.is_eof() {
            items.push(self.parse_item()?);
        }
        Ok(File { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        if self.peek_is_kw(Keyword::Forge) {
            Ok(Item::Forge(self.parse_forge()?))
        } else if self.peek_is_kw(Keyword::Shape) {
            Ok(Item::Shape(self.parse_shape()?))
        } else if self.peek_is_kw(Keyword::Proc) {
            Ok(Item::Proc(self.parse_proc_decl()?))
        } else if self.peek_is_kw(Keyword::Bind) {
            Ok(Item::Bind(self.parse_bind()?))
        } else {
            let t = self.peek().clone();
            self.err(t.span, "expected item (forge/shape/proc/bind)")
        }
    }

    // ---------- forge ----------

    fn parse_forge(&mut self) -> Result<Spanned<ForgeDecl>, ParseError> {
        let start_tok = self.expect_kw(Keyword::Forge)?;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        let mut items = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            items.push(self.parse_item()?);
        }
        let end_tok = self.expect(Token::RBrace)?;
        Ok(Spanned::new(
            ForgeDecl { name, items },
            Span::new(start_tok.span.start, end_tok.span.end),
        ))
    }

    // ---------- shape ----------

    fn parse_shape(&mut self) -> Result<Spanned<ShapeDecl>, ParseError> {
        let start_tok = self.expect_kw(Keyword::Shape)?;
        let name = self.expect_ident()?;
        self.expect(Token::LBrace)?;
        let mut fields = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            let fname = self.expect_ident()?;
            self.expect(Token::Colon)?;
            let ty = self.parse_type_ref()?;
            self.expect(Token::Semi)?;
            let f_start = fname.span.start;
            let t_end = ty.span.end;
            fields.push(Spanned::new(
                FieldDecl { name: fname, ty },
                Span::new(f_start, t_end),
            ));
        }
        let end_tok = self.expect(Token::RBrace)?;
        Ok(Spanned::new(
            ShapeDecl { name, fields },
            Span::new(start_tok.span.start, end_tok.span.end),
        ))
    }

    // ---------- proc decl ----------

    fn parse_proc_decl(&mut self) -> Result<Spanned<ProcDecl>, ParseError> {
        let start_tok = self.expect_kw(Keyword::Proc)?;
        let path = self.parse_proc_path()?;
        self.expect(Token::LParen)?;
        let mut params = Vec::new();
        if !self.peek_is(&Token::RParen) {
            loop {
                params.push(self.parse_param()?);
                if self.peek_is(&Token::Comma) {
                    self.bump();
                    continue;
                }
                break;
            }
        }
        let rparen = self.expect(Token::RParen)?;
        let mut ret: Option<Spanned<TypeRef>> = None;
        if self.peek_is(&Token::Arrow) {
            self.bump();
            ret = Some(self.parse_type_ref()?);
        }
        let sig_end = ret.as_ref().map(|t| t.span.end).unwrap_or(rparen.span.end);
        let mut contracts: Vec<Spanned<ContractDecl>> = Vec::new();
        if self.peek_is_kw(Keyword::Contract) {
            contracts = self.parse_contract_block()?;
        }
        let body = self.parse_block()?;
        Ok(Spanned::new(
            ProcDecl {
                path,
                params,
                ret,
                contracts,
                body,
            },
            Span::new(start_tok.span.start, body.span.end),
        ))
    }

    fn parse_param(&mut self) -> Result<Spanned<ParamDecl>, ParseError> {
        let start = self.peek().span.start;
        let name = self.expect_ident()?;
        self.expect(Token::Colon)?;
        let ty = self.parse_type_ref()?;
        let end = ty.span.end;
        Ok(Spanned::new(
            ParamDecl { name, ty },
            Span::new(start, end),
        ))
    }

    fn parse_proc_path(&mut self) -> Result<Spanned<ProcPath>, ParseError> {
        let start = self.peek().span.start;
        let regime_tok = self.bump();
        let regime = match regime_tok.node {
            Token::Ident(s) if s == "K" => Regime::K,
            Token::Ident(s) if s == "Q" => Regime::Q,
            Token::Ident(s) if s == "Φ" => Regime::Phi,
            Token::Ident(s) if s == "Phi" => Regime::Phi,
            Token::Ident(s) if s == "phi" => Regime::Phi,
            _ => return self.err(regime_tok.span, "expected regime identifier (K/Q/Φ)"),
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

    // ---------- bind ----------

    fn parse_bind(&mut self) -> Result<Spanned<BindDecl>, ParseError> {
        let start_tok = self.expect_kw(Keyword::Bind)?;
        let name = self.expect_ident()?;
        self.expect(Token::Eq)?;
        let target = self.parse_proc_path()?;
        let mut args: Vec<Spanned<NamedArg>> = Vec::new();
        if self.peek_is(&Token::LParen) {
            self.bump();
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
            self.expect(Token::RParen)?;
        }
        let semi = self.expect(Token::Semi)?;
        Ok(Spanned::new(
            BindDecl { name, target, args },
            Span::new(start_tok.span.start, semi.span.end),
        ))
    }

    // ---------- contracts ----------

    fn parse_contract_block(&mut self) -> Result<Vec<Spanned<ContractDecl>>, ParseError> {
        self.expect_kw(Keyword::Contract)?;
        self.expect(Token::LBrace)?;
        let mut decls = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            decls.push(self.parse_contract_decl()?);
        }
        self.expect(Token::RBrace)?;
        Ok(decls)
    }

    fn parse_contract_decl(&mut self) -> Result<Spanned<ContractDecl>, ParseError> {
        let start = self.peek().span.start;
        if self.peek_is_kw(Keyword::Requires) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                ContractDecl::Requires(e),
                Span::new(start, semi.span.end),
            ))
        } else if self.peek_is_kw(Keyword::Ensures) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                ContractDecl::Ensures(e),
                Span::new(start, semi.span.end),
            ))
        } else if self.peek_is_kw(Keyword::Uses) {
            self.bump();
            let r = self.expect_ident()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                ContractDecl::Uses(r),
                Span::new(start, semi.span.end),
            ))
        } else {
            let t = self.peek().clone();
            self.err(t.span, "expected contract decl (requires/ensures/uses)")
        }
    }

    // ---------- block / statements ----------

    fn parse_block(&mut self) -> Result<Spanned<Block>, ParseError> {
        let lbrace = self.expect(Token::LBrace)?;
        let mut stmts = Vec::new();
        while !self.peek_is(&Token::RBrace) {
            stmts.push(self.parse_stmt()?);
        }
        let rbrace = self.expect(Token::RBrace)?;
        Ok(Spanned::new(
            Block { stmts },
            Span::new(lbrace.span.start, rbrace.span.end),
        ))
    }

    fn parse_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.peek().span.start;

        if self.peek_is_kw(Keyword::Let) {
            self.bump();
            let name = self.expect_ident()?;
            let mut ty: Option<Spanned<TypeRef>> = None;
            if self.peek_is(&Token::Colon) {
                self.bump();
                ty = Some(self.parse_type_ref()?);
            }
            self.expect(Token::Eq)?;
            let expr = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                Stmt::Let(LetStmt { name, ty, expr }),
                Span::new(start, semi.span.end),
            ))
        } else if self.peek_is_kw(Keyword::Constrain) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                Stmt::Constrain(e),
                Span::new(start, semi.span.end),
            ))
        } else if self.peek_is_kw(Keyword::Prove) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(Stmt::Prove(e), Span::new(start, semi.span.end)))
        } else if self.peek_is_kw(Keyword::Emit) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(Stmt::Emit(e), Span::new(start, semi.span.end)))
        } else if self.peek_is_kw(Keyword::Observe) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                Stmt::Observe(e),
                Span::new(start, semi.span.end),
            ))
        } else if self.peek_is_kw(Keyword::Seal) {
            self.bump();
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(Stmt::Seal(e), Span::new(start, semi.span.end)))
        } else if self.peek_is_kw(Keyword::Return) {
            self.bump();
            let e = if self.peek_is(&Token::Semi) {
                None
            } else {
                Some(self.parse_expr()?)
            };
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                Stmt::Return(e),
                Span::new(start, semi.span.end),
            ))
        } else if self.peek_is_kw(Keyword::If) {
            Ok(self.parse_if_stmt()?)
        } else {
            // expression statement
            let e = self.parse_expr()?;
            let semi = self.expect(Token::Semi)?;
            Ok(Spanned::new(
                Stmt::Expr(e),
                Span::new(start, semi.span.end),
            ))
        }
    }

    fn parse_if_stmt(&mut self) -> Result<Spanned<Stmt>, ParseError> {
        let start = self.expect_kw(Keyword::If)?.span.start;
        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;
        let mut else_block: Option<Spanned<Block>> = None;
        if self.peek_is_kw(Keyword::Else) {
            self.bump();
            else_block = Some(self.parse_block()?);
        }
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

    // ---------- types ----------

    fn parse_type_ref(&mut self) -> Result<Spanned<TypeRef>, ParseError> {
        let start = self.peek().span.start;

        // Very small v0.1 type grammar:
        // - identifiers (e.g. Int, Bool, String, Unit, MyShape)
        // - &T
        // - [T]
        // - (T1, T2, ...) -> T
        // - Φ{...} / Q{...} / K{...} wrappers are deferred (front-end only for now)

        if self.peek_is(&Token::Amp) {
            let amp = self.bump();
            let inner = self.parse_type_ref()?;
            Ok(Spanned::new(
                TypeRef::Ref(Box::new(inner.clone())),
                Span::new(amp.span.start, inner.span.end),
            ))
        } else if self.peek_is(&Token::LBracket) {
            let lb = self.bump();
            let inner = self.parse_type_ref()?;
            let rb = self.expect(Token::RBracket)?;
            Ok(Spanned::new(
                TypeRef::List(Box::new(inner.clone())),
                Span::new(lb.span.start, rb.span.end),
            ))
        } else if self.peek_is(&Token::LParen) {
            // function type
            let lp = self.bump();
            let mut args = Vec::new();
            if !self.peek_is(&Token::RParen) {
                loop {
                    args.push(self.parse_type_ref()?);
                    if self.peek_is(&Token::Comma) {
                        self.bump();
                        continue;
                    }
                    break;
                }
            }
            self.expect(Token::RParen)?;
            self.expect(Token::Arrow)?;
            let ret = self.parse_type_ref()?;
            Ok(Spanned::new(
                TypeRef::Fn(args, Box::new(ret.clone())),
                Span::new(lp.span.start, ret.span.end),
            ))
        } else {
            let id = self.expect_ident()?;
            Ok(Spanned::new(
                TypeRef::Named(id.clone()),
                Span::new(start, id.span.end),
            ))
        }
    }

    // ---------- expressions ----------

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        self.parse_binop_expr(0)
    }

    fn parse_binop_expr(&mut self, min_prec: u8) -> Result<Spanned<Expr>, ParseError> {
        let mut lhs = self.parse_postfix_expr()?;

        loop {
            let (op, prec, right_assoc) = match self.peek().node {
                Token::Plus => (BinOp::Add, 10, false),
                Token::Minus => (BinOp::Sub, 10, false),
                Token::Star => (BinOp::Mul, 20, false),
                Token::Slash => (BinOp::Div, 20, false),
                Token::EqEq => (BinOp::Eq, 5, false),
                Token::BangEq => (BinOp::Neq, 5, false),
                Token::Lt => (BinOp::Lt, 6, false),
                Token::Le => (BinOp::Le, 6, false),
                Token::Gt => (BinOp::Gt, 6, false),
                Token::Ge => (BinOp::Ge, 6, false),
                Token::AndAnd => (BinOp::And, 3, false),
                Token::OrOr => (BinOp::Or, 2, false),
                _ => break,
            };

            if prec < min_prec {
                break;
            }

            let op_tok = self.bump();
            let next_min = if right_assoc { prec } else { prec + 1 };
            let rhs = self.parse_binop_expr(next_min)?;
            let span = Span::new(lhs.span.start, rhs.span.end);
            lhs = Spanned::new(
                Expr::Binary(BinaryExpr {
                    op: Spanned::new(op, op_tok.span),
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }),
                span,
            );
        }

        Ok(lhs)
    }

    fn parse_postfix_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let mut e = self.parse_primary_expr()?;

        loop {
            if self.peek_is(&Token::LParen) {
                // call
                let lp = self.bump();
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
                let rp = self.expect(Token::RParen)?;
                let span = Span::new(e.span.start, rp.span.end);
                e = Spanned::new(
                    Expr::Call(CallExpr {
                        callee: Box::new(e),
                        args,
                        call_span: Span::new(lp.span.start, rp.span.end),
                    }),
                    span,
                );
            } else if self.peek_is(&Token::Dot) {
                // field access
                self.bump();
                let name = self.expect_ident()?;
                let span = Span::new(e.span.start, name.span.end);
                e = Spanned::new(
                    Expr::Field(FieldExpr {
                        base: Box::new(e),
                        name,
                    }),
                    span,
                );
            } else {
                break;
            }
        }

        Ok(e)
    }

    fn parse_primary_expr(&mut self) -> Result<Spanned<Expr>, ParseError> {
        let t = self.peek().clone();
        match &t.node {
            Token::Int(_) | Token::Float(_) | Token::Str(_) | Token::Bool(_) => {
                let lit = self.parse_literal()?;
                Ok(Spanned::new(Expr::Literal(lit.node), lit.span))
            }
            Token::Ident(_name) => {
                let id = self.expect_ident()?;
                Ok(Spanned::new(Expr::Ident(id.clone()), id.span))
            }
            Token::LParen => {
                self.bump();
                let e = self.parse_expr()?;
                let rp = self.expect(Token::RParen)?;
                Ok(Spanned::new(e.node, Span::new(t.span.start, rp.span.end)))
            }
            Token::LBrace => {
                // block expression is deferred for v0.1; treat as error to keep grammar strict
                self.err(t.span, "block expressions are not supported in v0.1")
            }
            _ => self.err(t.span, format!("unexpected token in expression: {:?}", t.node)),
        }
    }

    fn parse_literal(&mut self) -> Result<Spanned<Literal>, ParseError> {
        let t = self.bump();
        match t.node {
            Token::Int(s) => Ok(Spanned::new(Literal::Int(s), t.span)),
            Token::Float(s) => Ok(Spanned::new(Literal::Float(s), t.span)),
            Token::Str(s) => Ok(Spanned::new(Literal::Str(s), t.span)),
            Token::Bool(b) => Ok(Spanned::new(Literal::Bool(b), t.span)),
            _ => self.err(t.span, "expected literal"),
        }
    }
}