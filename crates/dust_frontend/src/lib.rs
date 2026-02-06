// crates/dust_frontend/src/lib.r
// Frontend: lexing + parsing .ds source into AST
// v0.1: structure only

pub mod ast;
pub mod lexer;

pub use lexer::{Lexer, Token, Keyword, LexError, LexErrorKind};

pub fn parse_source(_src: &str) {
    // TODO: implement lexer + parser
}
