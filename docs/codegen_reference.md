# Code Generation Reference

Primary source: `crates/dust_codegen/src/lib.rs`

## Public Build APIs

- `build_executable(dir, out_path)`
- `build_object_file(dir, out_path)`
- `build_object_file_for_target(dir, out_path, target_triple)`
- `build_bare_metal_kernel(dir, out_path)`
- `merge_dir_programs(programs)`
- `build_kernel_entry_object(dir, out_path, entry_point, target_triple, allow_unresolved_calls)`
- `build_kernel_bootstrap_object(out_path, target_triple, start_symbol, entry_symbol)`

## Native Executable Path

`build_executable`:

1. finds `K::<entry>` (`main` by default from driver path)
2. extracts supported emit/call subset from DIR
3. generates object via Cranelift
4. links host executable via ordered linker attempts

## Host Linker Attempt Order

General host executable build:

1. `dustlink` (preferred)
2. compiler driver + `-fuse-ld=lld`
3. `rust-lld`
4. `ld.lld`
5. compiler driver default linker

Bootstrap build of `dustlink` executable itself:

1. compiler driver + `-fuse-ld=lld`
2. `rust-lld`
3. `ld.lld` (platform-dependent)
4. compiler driver default linker

This bootstrap exception prevents recursive self-linking while producing `dustlink.exe`.

## Host Runtime Shim Surface

`crates/dust_codegen/src/host_runtime_shim.rs` provides host intrinsics used by Dust-built host executables (including `dustlink`):

- argv/string/path/fs helpers
- archive probing/extraction helpers
- linker state accessors for object/symbol/relocation operations
- output writers for ELF/flat/MBR/PE/Mach-O image paths
- linker script apply hook used by `-T/--script`
- shared-object symbol ingestion across ELF, PE, COFF, and Mach-O paths
- block-aware linker-script statement splitting for multi-line `MEMORY`/`SECTIONS` scripts
- script semantics for `SECTIONS` output-address parse and `ENTRY(symbol)` required-symbol registration

## Object Path

`build_object_file*` emits object bytes via Cranelift object backend.

- host mode imports `puts`
- bare-metal object mode writes emits directly as VGA memory stores in generated function body

## Bare-metal Flat Binary Path

`build_bare_metal_kernel` emits a raw binary with simple direct machine bytes:

- writes lines to VGA text memory (`0xB8000`)
- ends with `hlt; jmp $`
- enforces image cap `128 * 512` bytes

## Scoped Kernel Entry Resolution

Kernel-entry object building supports scoped and unscoped K-proc resolution across forges, including ambiguity detection and optional unresolved-call skipping.

## Extended v0.2 Helpers

The crate also contains extended codegen helpers (`build_extended_bare_metal_kernel`, `build_extended_object`, and `v02_codegen.rs` utilities).

Current integration status:

- these helpers are not the default path invoked by `dust build` or standard `dust obj` host mode
- `v02_codegen.rs` exists as a separate source file and is not wired as a module in `lib.rs`
