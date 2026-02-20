# Developer Guide

## Build the Compiler Workspace

From `dust/`:

```bash
cargo build --workspace --verbose
```

## Run Tests

```bash
cargo test --workspace --verbose
```

## Local CLI Smoke Checks

```bash
cargo run -p dust -- check examples
cargo run -p dust -- dir examples/K --out target/dir --print
cargo run -p dust -- build examples/K/k_hello_world.ds
cargo run -p dust -- obj examples/K/k_hello_world.ds --out target/dust/hello.o
```

## Suggested Development Flow

1. update frontend AST/parser/lexer behavior
2. update semantic checks and lowering
3. update DIR structures only when required by spec/runtime contracts
4. update codegen extraction and emission
5. add fixtures and command-level validation
6. refresh docs in `dust/docs`

## Documentation Maintenance

When changing compiler behavior:

- update `cli_reference.md` if flags or defaults change
- update `compiler_pipeline.md` for path changes
- update `status_and_limitations.md` when integration gaps close
- keep references to `dust/spec` as normative source
