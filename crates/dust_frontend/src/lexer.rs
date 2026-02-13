// crates/dust_frontend/src/lexer.rs
//
// Minimal DPL lexer (v0.1) — tokens only.
// - Produces Spanned<Token> with byte spans.
// - Supports: identifiers, keywords, integers, strings, booleans
// - Symbols: ::, ->, ==, <=, >=, &&, ||, + - * / < > = : ; , . { } ( ) [ ]
// - Skips whitespace and comments:
//     // line comments
//     /* block comments */ (non-nesting)
//
// NOTE: This lexer is deliberately conservative and ASCII-oriented for operators/keywords.
// Identifiers support ASCII letters/underscore + digits after first char. The Φ regime
// is tokenized as a keyword if the exact character 'Φ' appears.

use crate::ast::{Span, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexErrorKind {
    UnexpectedChar(char),
    UnterminatedString,
    UnterminatedBlockComment,
    InvalidEscape(char),
    IntOverflow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    Forge,
    Shape,
    Proc,
    Bind,
    Contract,
    Uses,
    Let,
    Mut,
    Constrain,
    Prove,
    From,
    Observe,
    Emit,
    Seal,
    Return,
    Linear,
    If,
    Else,
    For,
    While,
    Break,
    Continue,
    In,
    Match,
    // Regimes
    K,
    Q,
    Phi, // Φ
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Atoms
    Ident(String),
    Int(i64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),
    Keyword(Keyword),

    // Punctuation / operators
    LBrace,   // {
    RBrace,   // }
    LParen,   // (
    RParen,   // )
    LBracket, // [
    RBracket, // ]
    Colon,    // :
    Semi,     // ;
    Comma,    // ,
    Dot,      // .
    Eq,       // =
    Plus,     // +
    Minus,    // -
    Star,     // *
    Slash,    // /

    ColonColon, // ::
    Arrow,      // ->
    EqEq,       // ==
    Lt,         // <
    Lte,        // <=
    Gt,         // >
    Gte,        // >=
    AndAnd,     // &&
    OrOr,       // ||
    DotDot,     // ..
    Bang,       // !
    Underscore, // _
    FatArrow,   // =>

    Eof,
}

impl Token {
    pub fn unwrap_ident(self) -> String {
        match self {
            Token::Ident(s) => s,
            _ => panic!("Token is not an Ident"),
        }
    }
}

pub struct Lexer<'a> {
    src: &'a str,
    bytes: &'a [u8],
    i: usize,
    len: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            bytes: src.as_bytes(),
            i: 0,
            len: src.len(),
        }
    }

    pub fn lex_all(mut self) -> Result<Vec<Spanned<Token>>, LexError> {
        let mut out = Vec::new();
        loop {
            let t = self.next_token()?;
            let is_eof = matches!(t.node, Token::Eof);
            out.push(t);
            if is_eof {
                break;
            }
        }
        Ok(out)
    }

    pub fn next_token(&mut self) -> Result<Spanned<Token>, LexError> {
        self.skip_ws_and_comments()?;

        let start = self.i as u32;
        if self.i >= self.len {
            return Ok(Spanned::new(Token::Eof, Span::new(start, start)));
        }

        // Fast path: single-byte dispatch
        let b = self.peek_u8().unwrap();
        match b {
            b'{' => return Ok(self.one(Token::LBrace)),
            b'}' => return Ok(self.one(Token::RBrace)),
            b'(' => return Ok(self.one(Token::LParen)),
            b')' => return Ok(self.one(Token::RParen)),
            b'[' => return Ok(self.one(Token::LBracket)),
            b']' => return Ok(self.one(Token::RBracket)),
            b':' => {
                self.bump();
                if self.peek_u8() == Some(b':') {
                    self.bump();
                    return Ok(self.span(Token::ColonColon, start, self.i as u32));
                }
                return Ok(self.span(Token::Colon, start, self.i as u32));
            }
            b';' => return Ok(self.one(Token::Semi)),
            b',' => return Ok(self.one(Token::Comma)),
            b'_' => return Ok(self.one(Token::Underscore)),
            b'+' => return Ok(self.one(Token::Plus)),
            b'*' => return Ok(self.one(Token::Star)),
            b'/' => return Ok(self.one(Token::Slash)),
            b'-' => {
                self.bump();
                if self.peek_u8() == Some(b'>') {
                    self.bump();
                    return Ok(self.span(Token::Arrow, start, self.i as u32));
                }
                return Ok(self.span(Token::Minus, start, self.i as u32));
            }
            b'=' => {
                self.bump();
                if self.peek_u8() == Some(b'=') {
                    self.bump();
                    return Ok(self.span(Token::EqEq, start, self.i as u32));
                }
                if self.peek_u8() == Some(b'>') {
                    self.bump();
                    return Ok(self.span(Token::FatArrow, start, self.i as u32));
                }
                return Ok(self.span(Token::Eq, start, self.i as u32));
            }
            b'<' => {
                self.bump();
                if self.peek_u8() == Some(b'=') {
                    self.bump();
                    return Ok(self.span(Token::Lte, start, self.i as u32));
                }
                return Ok(self.span(Token::Lt, start, self.i as u32));
            }
            b'>' => {
                self.bump();
                if self.peek_u8() == Some(b'=') {
                    self.bump();
                    return Ok(self.span(Token::Gte, start, self.i as u32));
                }
                return Ok(self.span(Token::Gt, start, self.i as u32));
            }
            b'&' => {
                self.bump();
                if self.peek_u8() == Some(b'&') {
                    self.bump();
                    return Ok(self.span(Token::AndAnd, start, self.i as u32));
                }
                return Err(self.err(LexErrorKind::UnexpectedChar('&'), start, self.i as u32));
            }
            b'|' => {
                self.bump();
                if self.peek_u8() == Some(b'|') {
                    self.bump();
                    return Ok(self.span(Token::OrOr, start, self.i as u32));
                }
                return Err(self.err(LexErrorKind::UnexpectedChar('|'), start, self.i as u32));
            }
            b'.' => {
                // Check for range operator ..
                if self.peek2_u8() == Some(b'.') {
                    self.bump();
                    self.bump();
                    return Ok(self.span(Token::DotDot, start, self.i as u32));
                }
                return Ok(self.one(Token::Dot));
            }
            b'!' => {
                self.bump();
                return Ok(self.span(Token::Bang, start, self.i as u32));
            }
            b'"' => return self.lex_string(),
            b'\'' => return self.lex_char(),
            b'0'..=b'9' => {
                // Check if it's a float (digit followed by dot)
                let save = self.i;
                // Read first digit
                self.bump();
                // Read more digits
                while let Some(b) = self.peek_u8() {
                    if (b'0'..=b'9').contains(&b) {
                        self.bump();
                    } else if b == b'.' {
                        // It's a float!
                        return self.lex_float();
                    } else {
                        break;
                    }
                }
                // Not a float, restore and lex as int
                self.i = save;
                return self.lex_int();
            }
            _ => {}
        }

        // Identifiers / keywords / Φ
        if self.starts_with_phi() {
            // Consume UTF-8 'Φ' (2 bytes: 0xCE 0xA6)
            self.i += 2;
            return Ok(self.span(Token::Keyword(Keyword::Phi), start, self.i as u32));
        }

        let ch = self.peek_char().unwrap();
        if is_ident_start(ch) {
            return Ok(self.lex_ident_or_keyword());
        }

        // Unknown character
        self.bump_char();
        Err(self.err(LexErrorKind::UnexpectedChar(ch), start, self.i as u32))
    }

    // ---- lexing helpers ----

    fn one(&mut self, t: Token) -> Spanned<Token> {
        let start = self.i as u32;
        self.bump();
        self.span(t, start, self.i as u32)
    }

    fn span(&self, node: Token, start: u32, end: u32) -> Spanned<Token> {
        Spanned::new(node, Span::new(start, end))
    }

    fn err(&self, kind: LexErrorKind, start: u32, end: u32) -> LexError {
        LexError {
            kind,
            span: Span::new(start, end),
        }
    }

    fn skip_ws_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            self.skip_ws();

            // Line comment
            if self.peek_u8() == Some(b'/') && self.peek2_u8() == Some(b'/') {
                self.bump();
                self.bump();
                while let Some(c) = self.peek_u8() {
                    self.bump();
                    if c == b'\n' {
                        break;
                    }
                }
                continue;
            }

            // Block comment (non-nesting)
            if self.peek_u8() == Some(b'/') && self.peek2_u8() == Some(b'*') {
                let start = self.i as u32;
                self.bump();
                self.bump();
                loop {
                    if self.i >= self.len {
                        return Err(self.err(
                            LexErrorKind::UnterminatedBlockComment,
                            start,
                            self.i as u32,
                        ));
                    }
                    if self.peek_u8() == Some(b'*') && self.peek2_u8() == Some(b'/') {
                        self.bump();
                        self.bump();
                        break;
                    }
                    self.bump_char();
                }
                continue;
            }

            break;
        }
        Ok(())
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == ' ' || c == '\t' || c == '\n' || c == '\r' {
                self.bump_char();
            } else {
                break;
            }
        }
    }

    fn lex_int(&mut self) -> Result<Spanned<Token>, LexError> {
        let start = self.i as u32;
        let mut val: i64 = 0;
        while let Some(b) = self.peek_u8() {
            if (b'0'..=b'9').contains(&b) {
                let digit = (b - b'0') as i64;
                self.bump();
                val = val
                    .checked_mul(10)
                    .ok_or_else(|| self.err(LexErrorKind::IntOverflow, start, self.i as u32))?
                    .checked_add(digit)
                    .ok_or_else(|| self.err(LexErrorKind::IntOverflow, start, self.i as u32))?;
            } else {
                break;
            }
        }
        Ok(self.span(Token::Int(val), start, self.i as u32))
    }

    fn lex_float(&mut self) -> Result<Spanned<Token>, LexError> {
        let start = self.i as u32;

        // Parse integer part first
        let mut int_part: f64 = 0.0;
        while let Some(b) = self.peek_u8() {
            if (b'0'..=b'9').contains(&b) {
                let digit = (b - b'0') as f64;
                self.bump();
                int_part = int_part * 10.0 + digit;
            } else {
                break;
            }
        }

        // Must have decimal point
        if self.peek_u8() != Some(b'.') {
            return Err(self.err(LexErrorKind::UnexpectedChar('.'), start, self.i as u32));
        }
        self.bump(); // consume '.'

        // Parse fractional part
        let mut frac_part: f64 = 0.0;
        let mut frac_div: f64 = 1.0;
        while let Some(b) = self.peek_u8() {
            if (b'0'..=b'9').contains(&b) {
                let digit = (b - b'0') as f64;
                self.bump();
                frac_div *= 10.0;
                frac_part = frac_part * 10.0 + digit;
            } else {
                break;
            }
        }

        let val = int_part + (frac_part / frac_div);
        Ok(self.span(Token::Float(val), start, self.i as u32))
    }

    fn lex_char(&mut self) -> Result<Spanned<Token>, LexError> {
        let start = self.i as u32;
        self.bump(); // consume opening '

        let ch = if self.peek_u8() == Some(b'\\') {
            self.bump(); // consume '\'
            match self.peek_u8() {
                Some(b'n') => {
                    self.bump();
                    '\n'
                }
                Some(b't') => {
                    self.bump();
                    '\t'
                }
                Some(b'r') => {
                    self.bump();
                    '\r'
                }
                Some(b'\'') => {
                    self.bump();
                    '\''
                }
                Some(b'\\') => {
                    self.bump();
                    '\\'
                }
                Some(c) => {
                    self.bump();
                    c as char
                }
                None => {
                    return Err(self.err(LexErrorKind::UnterminatedString, start, self.i as u32))
                }
            }
        } else {
            match self.peek_char() {
                Some(c) if c != '\'' => {
                    self.bump_char();
                    c
                }
                _ => {
                    return Err(self.err(LexErrorKind::UnexpectedChar('\''), start, self.i as u32))
                }
            }
        };

        // Expect closing '
        if self.peek_u8() != Some(b'\'') {
            return Err(self.err(LexErrorKind::UnterminatedString, start, self.i as u32));
        }
        self.bump(); // consume closing '

        Ok(self.span(Token::Char(ch), start, self.i as u32))
    }

    fn lex_string(&mut self) -> Result<Spanned<Token>, LexError> {
        let start = self.i as u32;
        // consume opening quote
        self.bump();
        let mut out = String::new();
        loop {
            if self.i >= self.len {
                return Err(self.err(LexErrorKind::UnterminatedString, start, self.i as u32));
            }
            let c = self.peek_char().unwrap();
            match c {
                '"' => {
                    self.bump_char();
                    break;
                }
                '\\' => {
                    self.bump_char();
                    if self.i >= self.len {
                        return Err(self.err(
                            LexErrorKind::UnterminatedString,
                            start,
                            self.i as u32,
                        ));
                    }
                    let esc = self.peek_char().unwrap();
                    self.bump_char();
                    match esc {
                        '"' => out.push('"'),
                        '\\' => out.push('\\'),
                        'n' => out.push('\n'),
                        't' => out.push('\t'),
                        'r' => out.push('\r'),
                        other => {
                            return Err(self.err(
                                LexErrorKind::InvalidEscape(other),
                                start,
                                self.i as u32,
                            ));
                        }
                    }
                }
                _ => {
                    out.push(c);
                    self.bump_char();
                }
            }
        }
        Ok(self.span(Token::String(out), start, self.i as u32))
    }

    fn lex_ident_or_keyword(&mut self) -> Spanned<Token> {
        let start = self.i as u32;
        let mut s = String::new();
        // first char
        let c0 = self.peek_char().unwrap();
        s.push(c0);
        self.bump_char();
        // rest
        while let Some(c) = self.peek_char() {
            if is_ident_continue(c) {
                s.push(c);
                self.bump_char();
            } else {
                break;
            }
        }

        let tok = match s.as_str() {
            "forge" => Token::Keyword(Keyword::Forge),
            "shape" => Token::Keyword(Keyword::Shape),
            "proc" => Token::Keyword(Keyword::Proc),
            "bind" => Token::Keyword(Keyword::Bind),
            "contract" => Token::Keyword(Keyword::Contract),
            "uses" => Token::Keyword(Keyword::Uses),
            "let" => Token::Keyword(Keyword::Let),
            "mut" => Token::Keyword(Keyword::Mut),
            "constrain" => Token::Keyword(Keyword::Constrain),
            "prove" => Token::Keyword(Keyword::Prove),
            "from" => Token::Keyword(Keyword::From),
            "observe" => Token::Keyword(Keyword::Observe),
            "emit" => Token::Keyword(Keyword::Emit),
            "seal" => Token::Keyword(Keyword::Seal),
            "return" => Token::Keyword(Keyword::Return),
            "linear" => Token::Keyword(Keyword::Linear),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "for" => Token::Keyword(Keyword::For),
            "while" => Token::Keyword(Keyword::While),
            "break" => Token::Keyword(Keyword::Break),
            "continue" => Token::Keyword(Keyword::Continue),
            "in" => Token::Keyword(Keyword::In),
            "match" => Token::Keyword(Keyword::Match),
            "K" => Token::Keyword(Keyword::K),
            "Q" => Token::Keyword(Keyword::Q),
            "true" => Token::Bool(true),
            "false" => Token::Bool(false),
            _ => Token::Ident(s),
        };

        self.span(tok, start, self.i as u32)
    }

    // ---- cursor helpers ----

    fn peek_u8(&self) -> Option<u8> {
        self.bytes.get(self.i).copied()
    }

    fn peek2_u8(&self) -> Option<u8> {
        self.bytes.get(self.i + 1).copied()
    }

    fn bump(&mut self) {
        self.i += 1;
    }

    fn peek_char(&self) -> Option<char> {
        self.src[self.i..].chars().next()
    }

    fn bump_char(&mut self) {
        if let Some(ch) = self.peek_char() {
            self.i += ch.len_utf8();
        }
    }

    fn starts_with_phi(&self) -> bool {
        // UTF-8 bytes for 'Φ' are 0xCE 0xA6
        self.bytes.get(self.i) == Some(&0xCE) && self.bytes.get(self.i + 1) == Some(&0xA6)
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    is_ident_start(c) || c.is_ascii_digit()
}
