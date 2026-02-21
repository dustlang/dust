# Status and Limitations

## Spec and Implementation State

- The normative language specification under `dust/spec` is currently organized around v0.1 documents.
- Repository narratives and helper modules include v0.2-oriented work and expansion paths.

## Current Effective Constraints

- many default compile paths still rely on emit/call subset extraction
- host executable expression/statement lowering is still partial for full-feature CLI tools
- full coverage of all parser/AST constructs in codegen is not yet end-to-end
- Q and Phi are not first-class executable codegen targets in standard paths

## Recent Progress

- host executable link path now prefers `dustlink` with explicit `lld` fallback order
- bootstrap guard for building `dustlink` avoids recursive self-link
- kernel workflows are documented around `obj` + `dustlink` with `kernel-link` as compatibility mode
- host runtime shim grew linker-oriented intrinsics used by Dust-built `dustlink`
- host runtime shim shared-symbol ingestion now includes ELF, PE, COFF, and Mach-O metadata paths
- linker-script application now uses block-aware statement splitting and additional `SECTIONS`/`ENTRY(symbol)` semantics used by dustlink parity work

## Important Mismatches to Track

- DIR model includes richer statements than default lowering/codegen paths fully consume
- `type_system.rs` provides a larger type-checking framework than the minimal checks used in `parse_and_check`
- `v02_codegen.rs` contains additional routines but is not wired through `lib.rs`

## Operational Guidance

Treat current compiler behavior as a staged implementation:

1. verify syntax and minimal semantics with `check`
2. inspect lowered form via `dir`
3. use `obj` + `dustlink` for multi-object kernel workflows
4. validate unsupported constructs early using targeted fixtures
