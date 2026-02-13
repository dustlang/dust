# Changelog - Dust Compiler

All notable changes to the Dust compiler are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - Unreleased (DPL v0.2)

### Added

#### Frontend Enhancements
- Enhanced parser for K Regime v0.2 syntax
- Variable declarations (`let`, `mut let`)
- Control flow parsing (`if/else`, `for`, `while`, `break`, `continue`)
- Match expression parsing (`match`, `_`, `=>`)
- Function definition parsing with parameters and return types
- Structure definition parsing
- Memory operation parsing (`alloc`, `dealloc`, pointer operations)

#### Lexer Updates
- Float literal support (`3.14`)
- Char literal support (`'a'`)
- New keywords: `mut`, `if`, `else`, `for`, `while`, `break`, `continue`, `in`, `match`
- New tokens: `..` (range), `!` (not), `_` (underscore), `=>` (fat arrow)

#### AST Updates
- New statement types: `MutLet`, `If`, `For`, `While`, `Break`, `Continue`, `Expr`
- New expression types: `Unary`, `Index`, `Array`, `Block`, `Match`
- New pattern types: `MatchExpr`, `MatchArm`, `MatchPattern`
- Extended primitive types: Float, Char

#### Type System
- Extended type checking for all K Regime v0.2 types
- Pointer type validation
- Array and structure type checking
- Function signature type checking
- Type inference support
- Type environment (`TypeEnv`) implementation
- Type inferrer and checker

#### Memory Safety Analysis
- Use-after-free detection
- Buffer overflow prevention
- Dangling pointer elimination
- Proper alignment checking

#### Backend Improvements
- Enhanced x64 code generation
- Extended ELF object file generation
- Structure layout optimization
- Function call optimization
- Extended DIR IR with v0.2 statements
- New codegen framework (`v02_codegen.rs`)

#### Build System
- Project structure support (State.toml, sector directories)
- Multi-sector project compilation
- Dependency resolution
- Cross-sector optimization (LTO)

#### Runtime Support
- Heap allocator implementation (`runtime.rs`)
- String operations (alloc, concat, len)
- Error handling (panic, assert, unreachable)
- Type conversions (int<->float, char<->int)
- Array operations

#### File Format Support
- State.toml project configuration parsing
- Support for both `.ds` and `.dust` file extensions
- DPL project structure (sector directories)

### Changed

- Minimum Rust version updated to support new features
- Default build output directory structure updated
- Error messages improved for new syntax

### Fixed

- Memory safety issues in generated code
- Type inference edge cases
- Code generation for complex control flow

## [0.1.0] - 2026-02-12

### Added

- Initial compiler implementation v0.1
- Basic K Regime parsing (`K main { ... }`)
- Emit effect handling
- Dust IR (DIR) generation
- Native executable generation (ELF/Mach-O/PE)
- Basic type checking
- Example programs in `examples/K/`

### Known Issues

- Limited to emit-only K Regime in v0.1
- No function definitions or user types
- No memory operations

---

Copyright © 2026 Dust LLC