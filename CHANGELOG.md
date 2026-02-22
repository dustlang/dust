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

#### Host Link and Toolchain Integration
- Host link resolution now prefers `dustlink` for general executable builds, with ordered `lld` fallbacks.
- Bootstrap exception for building `dustlink` itself to avoid recursive self-linking.
- Expanded host runtime shim coverage for Dust-built host tooling workflows (argv/fs/path/string/linker helper intrinsics).
- Host runtime shim linker parity extensions:
  - shared-symbol ingest for ELF dynsym, PE export tables, COFF external definitions, and Mach-O external definitions
  - block-aware linker-script statement splitting (brace/paren-aware) instead of naive split behavior
  - script parsing additions for `SECTIONS` output-address forms and `ENTRY(symbol)` required-symbol registration
  - script parsing additions for `SEARCH_DIR(=...)` sysroot-aware resolution and `INPUT` token handling for `-L`/`-l`
  - script parsing additions for expression evaluation (`ORIGIN/LENGTH/ADDR/LOADADDR/SIZEOF/ALIGN` + `+/-`) and `ASSERT(...)`
  - linker-script compatibility handling for `PHDRS` and `VERSION` blocks now validates block structure
  - script parsing additions for `SECTIONS ... AT(<expr>)` load-address capture
  - compatibility-flag state wiring for `--hash-style`, `--threads`, `--thread-count`, `--eh-frame-hdr`, fatal/color diagnostics toggles, print-gc toggles, and `--icf=*` mode
  - broader host CLI compatibility handling for ld/lld-style script/export flags (`--version-script`, `--dynamic-list`, `--trace-symbol`, `--print-map`, `--start-lib`, `--end-lib`)
  - broader host CLI compatibility handling for `lld-link` slash-option families (`/OUT`, `/ENTRY`, `/MACHINE`, `/LIBPATH`, `/DEFAULTLIB`, `/MAP`, `/DLL`, `/SUBSYSTEM`, `/OPT`, `/WX`) and common slash metadata options
  - dynamic-policy alias coverage for `--no-allow-shlib-undefined` and split-value parsing parity for `--dynamic-linker` / `--soname`
  - expanded `-z` option semantics in host runtime (`defs`/`undefs`) plus accepted compatibility tokens (`text`/`notext`/`origin`)
  - additional `lld-link` no-value compatibility acceptance (`/NOENTRY`, `/DYNAMICBASE`, `/NXCOMPAT`, `/LARGEADDRESSAWARE`)
  - target alias expansion to accept `aarch64`/`arm64` triples in host linker target parsing
  - COFF/Mach-O object-format probe and symbol-ingest acceptance for arm64 machine/cpu IDs
  - ELF writer flow now emits a complete executable during header/finalize stages (instead of ident-only priming), and output-section stream calls validate section-index bounds
  - PE and Mach-O host writers now emit sectionized images from alloc chunks rather than single synthetic text payload sections
  - parity-oriented relocation/machine support surfaced to Dust linker modules (including additional x86_64 relocation IDs and ELF `EM_AARCH64` acceptance in validator paths)
  - architecture-aware target identity is preserved end-to-end (`x86_64` vs `aarch64`) instead of collapsing all targets into x86_64 families
  - ELF/PE/Mach-O host writers now stamp architecture-correct machine/cpu fields based on resolved linker target
  - host runtime relocation pipeline is machine-aware per object via `host_linker_object_machine`
  - baseline AArch64 relocation handling is available in Dust linker relocation logic (`R_AARCH64_NONE`, `R_AARCH64_ABS64`, `R_AARCH64_ABS32`, `R_AARCH64_PREL32`)
  - `lld-link` compatibility toggles `/NOENTRY`, `/DYNAMICBASE`, `/NXCOMPAT`, and `/LARGEADDRESSAWARE` are state-wired into PE header emission behavior
  - host CLI compatibility coverage expanded with soft-compatibility families for common ld/lld/lld-link metadata/profiling options (`--warn-*`, `--time-trace*`, `--lto-*`, `/GUARD:*`, `/TIMESTAMP:*`, `/MERGE:*`, `/SECTION:*`, etc.)
  - linker-script expression evaluator now covers unary, multiplicative, shift, and bitwise operators in addition to existing additive/script builtins
  - linker-script parsing now supports direct symbol assignments (`SYMBOL = <expr>`) and rejects unknown directive heads instead of silently accepting them
  - linker-script `OUTPUT_FORMAT`, `TARGET`, and `OUTPUT_ARCH` directives now return invalid/unsupported errors for bad values instead of silently succeeding
  - host runtime shim now exposes/consumes parity state for `fatal-warnings`, `color-diagnostics`, `print-gc-sections`, `--dependency-file`, and `--emit-relocs`
  - ELF writer now consumes `--hash-style` state to emit hash-table dynamic tags (`DT_HASH`, `DT_GNU_HASH`) in generated ELF outputs
  - host object ingestion uses refined machine-aware relocation mapping for COFF and Mach-O relocation records
  - target alias parsing expanded for musl triples, Windows GNU triples, and `*-none[-elf]` bare-metal aliases used by Dust-built tools
  - host runtime shim / Dust linker parity coverage expanded for AArch64 ELF instruction relocations:
    - branch/literal/ADR/ADRP/ADD/LDST (including `LDST128`) bitfield patching
    - MOVW `UABS` / `SABS` / `PREL` families
    - starter TLS instruction-form relocation ID support (`TLSGD`, `TLSLD`, `TLSDESC`) with strict unsupported apply-path handling for non-implemented descriptor semantics
    - host-runtime-backed AArch64 TLS data relocation values for `TLS_DTPMOD`, `TLS_DTPREL`, and `TLS_TPREL` in non-shared links using deterministic TLS layout metadata
  - host runtime shared-object ingestion now returns `ERR_INVALID_FORMAT` for unknown/unsupported shared-object payloads instead of silently succeeding

### Changed

- Minimum Rust version updated to support new features
- Default build output directory structure updated
- Error messages improved for new syntax
- `dust kernel-link` is explicitly deprecated in favor of `dust obj` + `dustlink` workflow guidance.

### Fixed

- Memory safety issues in generated code
- Type inference edge cases
- Code generation for complex control flow
- Host linker-script runtime no longer silently accepts unknown linker-script directives.
- Host linker compatibility/no-op flag handling now surfaces diagnostics instead of silent acceptance.
- Dust-built linker relocation parsing/validation now accepts and processes a broader AArch64 ELF relocation set (including MOVW and TLS starter forms) instead of rejecting them during ingest.

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
