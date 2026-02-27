// File: lib.rs - This file is part of the DPL Toolchain
// Copyright (c) 2026 Dust LLC, and Contributors
// Description:
//   Frontend library for the Dust Programming Language.
//   Provides lexing (tokenization) and parsing of .ds source files into AST.
//   v0.1: Basic structure only
//   v0.2+: Extended with additional syntax and features

pub mod ast;
pub mod lexer;
pub mod parser;

pub use lexer::{Keyword, LexError, LexErrorKind, Lexer, Token};
pub use parser::{ParseError, Parser};
