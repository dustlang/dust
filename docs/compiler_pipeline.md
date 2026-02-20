# Compiler Pipeline

## Input Collection

`dust_driver` recursively collects files when input path is a directory.
Accepted extensions:

- `.ds`
- `.dust`

For multi-input commands (`obj`, `kernel-link`):

- file list is sorted and deduplicated
- optional test-filter excludes stems ending with `_tests`

## Parsing and Checking

All compile commands use:

- `dust_semantics::parse_and_check(source)`

This wraps lexer + parser and then minimal semantic checks.

## AST to DIR Lowering

After successful checks, the driver calls:

- `dust_semantics::lower_to_dir(ast)`

DIR is used as the input contract for code generation and JSON emission.

## Command-specific Codegen Paths

## `build`

- `dust_codegen::build_executable(dir, out_path)`
- codegen builds a temporary object and links via system toolchain

## `obj`

Depending on flags:

- `--bare-metal`: `build_bare_metal_kernel`
- `--target <contains none>`: kernel-entry object path using `build_kernel_entry_object`
- `--target <other>`: `build_object_file_for_target`
- no target: `build_object_file`

## `kernel-link`

Pipeline:

1. parse/check/lower each source
2. merge DIR programs
3. build entry object (`build_kernel_entry_object`)
4. build bootstrap object (`build_kernel_bootstrap_object`)
5. invoke external linker with fixed bare-metal flags

## Link Step (Native Build)

Native executable linking uses:

- Unix/macOS: `cc` (or env `CC`)
- Windows: `clang` (or env `CC`)

## Link Step (Kernel Link)

`kernel-link` external linker invocation includes:

- `-m elf_x86_64`
- `-nostdlib`
- `--oformat=binary`
- `--image-base 0x100000`
- `-Ttext 0x100000`
- `-e <start_symbol>`
