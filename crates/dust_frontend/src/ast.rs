// crates/dust_frontend/src/ast.rs
//
// Abstract Syntax Tree definitions for the Dust Programming Language (DPL)
// Frontend.
//
// This file is intentionally self-contained and replacement-safe.
// It resolves CI errors requiring `Span: Hash` and provides the canonical
// AST surface for v0.1.
//
// © 2026 Dust LLC

use std::fmt;
use std::hash::Hash;

//
// ─────────────────────────────────────────────────────────────────────────────
// Source Spans
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Spanned Wrapper
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Identifiers
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident {
    pub text: String,
    pub span: Span,
}

impl Ident {
    pub fn new(text: impl Into<String>, span: Span) -> Self {
        Self {
            text: text.into(),
            span,
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.text.fmt(f)
    }
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Regimes
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Regime {
    K,
    Q,
    Phi,
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Program Structure
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub forges: Vec<Forge>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Forge {
    pub name: Ident,
    pub processes: Vec<Process>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Process {
    pub regime: Regime,
    pub name: Ident,
    pub body: Vec<Stmt>,
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Statements
// ─────────────────────────────────────────────────────────────────────────────
//

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Emit(Spanned<String>),
}

//
// ─────────────────────────────────────────────────────────────────────────────
// Notes
// ─────────────────────────────────────────────────────────────────────────────
//
// • `Span` derives `Hash` to allow `Spanned<T>: Hash`.
// • AST nodes intentionally avoid lifetimes for simpler DIR lowering.
// • This AST matches the v0.1 executable subset (K-regime + emit).
// • Q / Φ constructs are parsed but not codegen-enabled yet.
//