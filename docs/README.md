# dust Compiler Documentation

This directory contains complete Markdown documentation for the Dust compiler (`dust`).

## Documentation Index

- `compiler_overview.md`: compiler scope, crate layout, and execution model.
- `cli_reference.md`: full `dust` command reference and option behavior.
- `compiler_pipeline.md`: end-to-end compilation flow by command.
- `frontend_lexer_parser.md`: lexer/parser behavior and AST surface.
- `semantics_lowering.md`: semantic checks and lowering into DIR.
- `dir_ir_reference.md`: `dust_dir` structures and statement model.
- `codegen_reference.md`: native/object/bare-metal code generation behavior.
- `kernel_workflow.md`: kernel object and link workflows (`obj` + `dustlink`).
- `diagnostics_and_failures.md`: common failure modes and messages.
- `status_and_limitations.md`: implemented subset, gaps, and integration status.
- `developer_guide.md`: local build/test/docs workflow for compiler maintainers.

## Existing Analysis Docs

The following existing files remain part of the repository history and analysis context:

- `IMPLEMENTATION_ANALYSIS.md`
- `V02_COMPLIANCE.md`

## Release Notes

Compiler change history is tracked in `../CHANGELOG.md`.
Recent host-link/runtime parity work for Dust-built tooling is summarized in `status_and_limitations.md`, including stricter linker-script semantics, expanded target alias coverage, compatibility-state behavior wiring, and newer AArch64 ELF relocation/TLS starter coverage used by Dust-built `dustlink`.

## Normative Language Reference

The language specification is normative and lives under `dust/spec`.
If any narrative documentation conflicts with `dust/spec`, the spec is authoritative.
