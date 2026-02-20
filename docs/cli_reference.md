# CLI Reference

Source: `crates/dust_driver/src/main.rs`

## Command Summary

```text
dust check [path]
dust dir [path] [-o|--out <dir>] [--print]
dust build [path] [-o|--out <exe>]
dust obj <inputs...> [--out <obj>] [--out-dir <dir>] [--entry <name>] [--target <triple>] [--bare-metal] [--auto-entry <bool>] [--skip-tests <bool>]
dust run [path] [-- <args...>]
dust kernel-link <inputs...> [--out <bin>] [--obj-dir <dir>] [--entry <name>] [--start-symbol <name>] [--target <triple>] [--linker <cmd>] [--skip-tests <bool>]
```

## `check`

Parse and validate all source files under `path` (default `.`).

Behavior:

- recursively collects `.ds` and `.dust`
- runs `parse_and_check` per file
- prints `OK` on success

## `dir`

Emit DIR JSON files from each source file.

Defaults:

- `path = .`
- `out = target/dir`

Behavior:

- writes `<stem>.dir.json` for each input file
- `--print` additionally prints emitted JSON to stdout

## `build`

Build native executable from exactly one source file.

Defaults:

- `path = .`
- output path = `target/dust/<stem>` (adds `.exe` on Windows when extension omitted)

Behavior:

- requires exactly one `.ds`/`.dust` file after collection
- parses/checks, lowers to DIR, emits executable

## `obj`

Compile one or more sources into object or binary outputs.

Key options:

- `--out <path>`: explicit single output (valid only with exactly one source)
- `--out-dir <dir>`: multi-source output root (default `target/dust/obj`)
- `--entry <name>`: bare-metal object entry selection (default `main`)
- `--target <triple>`: cross target selection
- `--bare-metal`: emit flat kernel binary path
- `--auto-entry <bool>`: fallback to first K proc when entry missing in bare-metal target object mode (default `true`)
- `--skip-tests <bool>`: exclude `*_tests.ds` (default `true`)

Output naming for multi-source mode:

- deterministic `<stem>_<pathhash>.o` (or `.bin` when `--bare-metal`)

## `run`

Build then execute one source program.

Behavior:

- uses same single-file selection constraints as `build`
- forwards args after `--` to spawned executable

## `kernel-link` (Deprecated)

The command still exists but prints a deprecation warning:

```text
warning: 'dust kernel-link' is deprecated; use 'dust obj' and link with 'dustlink' instead
```

It performs a multi-source compile and external link sequence for a flat kernel binary.
