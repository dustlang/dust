# Compiler Overview

## Scope

`dust` is the CLI compiler driver for Dust source files (`.ds`, `.dust`).
It orchestrates:

1. frontend lexing and parsing
2. semantic checks
3. lowering into DIR (Dust Intermediate Representation)
4. code generation and optional linking

## Workspace Crates

- `crates/dust_driver`: CLI entrypoint (`dust` binary).
- `crates/dust_frontend`: AST, lexer, parser.
- `crates/dust_semantics`: parse/check wrapper and AST -> DIR lowering.
- `crates/dust_dir`: serializable DIR model.
- `crates/dust_codegen`: Cranelift/object/native and bare-metal emission.

## High-Level Flow

```text
.ds/.dust source
  -> dust_frontend (lexer + parser)
  -> dust_semantics::parse_and_check
  -> dust_semantics::lower_to_dir
  -> dust_codegen (build/obj/bare-metal helpers)
  -> system linker or external linker path
```

## Command Surface

`dust` currently exposes:

- `check`
- `dir`
- `build`
- `obj`
- `run`
- `kernel-link` (deprecated in code path)

## Current Practical Profile

The compiler supports multiple output modes, but runtime codegen coverage is still subset-oriented in key paths:

- native executable path focuses on K-regime `emit`-centric extraction
- object and bare-metal paths support kernel-oriented workflows
- several v0.2-related structures and helper modules exist but are not fully wired end-to-end
