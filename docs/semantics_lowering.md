# Semantics and Lowering

Source: `crates/dust_semantics/src/lib.rs`

## `parse_and_check`

Sequence:

1. lex all tokens
2. parse file AST
3. run minimal structural checks

Checks currently enforced include:

- forge name non-empty
- bind contract key non-empty
- Q-regime procs must include `linear` qualifier

Errors are normalized as:

- `CheckError { message, span }`

## `lower_to_dir`

Converts AST into `DirProgram`.

Current lowering behavior:

- emits forges/shapes/procs/binds
- constants are not represented as standalone DIR nodes (treated as compile-time inline values)
- sorts shapes/procs/binds/forges for deterministic output ordering

Statement lowering includes canonicalization, for example:

- `Let` and `MutLet` both lower to `DirStmt::Let`
- complex control flow can be represented as effect payload strings in current lowering path

Expression lowering is string-oriented for many nodes, which constrains downstream codegen paths that expect specific formats.

## Type-system Module

`crates/dust_semantics/src/type_system.rs` contains a richer type inference/checking framework (types, env, inferrer, checker), but it is not the primary path used by `parse_and_check` in current driver command execution.
