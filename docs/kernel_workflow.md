# Kernel Workflow

## Recommended Path

The codebase itself marks `dust kernel-link` as deprecated.
Recommended workflow is:

1. compile Dust modules into objects via `dust obj`
2. link with `dustlink`

## Object Emission Example

```bash
dust obj xdv-kernel/sector/xdv_kernel/src xdv-runtime/src xdv-xdvfs/src --target x86_64-pc-none-elf --out-dir target/dust/kernel_objs
```

## Linking Example

```bash
dustlink -m elf_x86_64 -nostdlib --oformat=binary --image-base 0x100000 -Ttext 0x100000 -e _dust_kernel_start -o target/dust/kernel.bin <objects...>
```

## Legacy `kernel-link`

Still available and useful for quick integration, but now emits a deprecation warning and should be treated as compatibility path.

## Entry Selection Notes

In `obj` command logic:

- `--bare-metal` path uses `build_bare_metal_kernel` (entry handling follows that helper's behavior)
- bare-metal target object path (`--target` containing `none`) uses `build_kernel_entry_object` and supports fallback entry when `--auto-entry true`

## Test Module Filtering

For kernel builds, `--skip-tests` defaults to `true` and excludes files where stem ends in `_tests`.
