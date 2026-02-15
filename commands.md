# commands.md

This document describes the `dust` CLI for the Dust Programming Language (DPL) toolchain.

DPL source files use the default extension: **`.ds`**.

---

## `dust check`

Parse and validate `.ds` input(s) (no code generation).

### Usage

```bash
dust check <path>
```

- If `<path>` is a file: it MUST be a `.ds` file.
- If `<path>` is a directory: all `.ds` files under it are checked.

### Examples

```bash
dust check examples/
dust check examples/K/k_hello_world.ds
```

---

## `dust dir`

Emit canonical DIR (Dust Intermediate Representation) for `.ds` input(s).

### Usage

```bash
dust dir <path> --out <dir> [--print]
```

- Writes one `*.dir.json` per input module to the output directory.
- `--print` also prints the JSON to stdout.

### Examples

```bash
dust dir examples/ --out target/dir
dust dir examples/K/k_hello_world.ds --out target/dir --print
```

---

## `dust build`

Build a **native executable** from a `.ds` program.

### Usage

```bash
dust build <path> [--out <exe-path>]
```

- `<path>` may be:
  - a single `.ds` file, or
  - a directory containing **exactly one** `.ds` file (v0.1 behavior)
- `--out` sets the output executable path.
  - If omitted, the default is: `target/dust/<stem>` (and `.exe` is added on Windows if missing)

### v0.1 Executable Subset (Current)

The v0.1 compiler currently produces native executables for a constrained, spec-aligned subset:

- Entry point: `K main { ... }`
- Supported statements in `main`:
  - ordered `emit "<string>"` effects
- Q-regime and Φ-regime are not codegen-enabled yet.

If your program uses unsupported constructs, `dust build` MUST fail deterministically with an explanatory error.

### Examples

```bash
dust build examples/K/k_hello_world.ds
./target/dust/k_hello_world
```

```bash
dust build examples/K/k_multiple_emits.ds --out target/dust/my_program
./target/dust/my_program
```

---

## `dust run`

Build then run a `.ds` program.

### Usage

```bash
dust run <path> -- <args...>
```

- Builds the program using the same rules as `dust build`.
- Any arguments after `--` are passed to the produced executable.

### Examples

```bash
dust run examples/K/k_hello_world.ds
```

```bash
dust run examples/K/k_hello_world.ds -- --help
```

---

## `dust kernel-link`

Compile multiple DPL modules to relocatable objects and link a bare-metal kernel image.

### Usage

```bash
dust kernel-link <input> [<input> ...] [options]
```

Where each `<input>` is a `.ds` file or a directory.

### Key options

- `--out <path>`: output kernel binary (default: `target/dust/kernel.bin`)
- `--obj-dir <path>`: intermediate object directory
- `--entry <name>`: K entry proc name (default: `main`)
- `--start-symbol <name>`: linked start symbol (default: `_dust_kernel_start`)
- `--target <triple>`: object target triple (default: `x86_64-pc-none-elf`)
- `--linker <cmd>`: external linker command (default: `ld.lld`)
- `--skip-tests <bool>`: skip `*_tests.ds` modules (default: `true`)

### Example

```bash
dust kernel-link xdv-kernel/sector/xdv_kernel/src \
                 xdv-runtime/src \
                 xdv-xdvfs/src \
                 --entry main \
                 --out target/dust/xdv-kernel.bin
```

---

## Output Layout

Default build outputs:

- Executables: `target/dust/<stem>` (or `target/dust/<stem>.exe` on Windows)
- DIR (if emitted): `target/dir/*.dir.json`

---

## Notes

- `dust` is the toolchain entrypoint and the authoritative compiler for DPL.
- The specification is normative and lives in `spec/`.
- Reference examples live in `examples/`.
