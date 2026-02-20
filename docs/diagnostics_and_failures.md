# Diagnostics and Failure Modes

## Common Driver Errors

- no source files found in path/input set
- expected exactly one source file for `build`/`run`
- invalid use of `--out` with multiple `obj` sources
- parse/check span errors propagated with file context

## Common Codegen Errors

- missing `K::<entry>` procedure
- unsupported statement/effect payload in v0.1 extraction path
- recursive K call graph in emit extraction
- ambiguous K proc names across forges in non-scoped paths
- invalid or unresolved scoped call targets in kernel entry extraction
- target triple parse errors
- external linker invocation failure with captured stdout/stderr

## Kernel Link Errors

- no source files
- no objects to link
- external linker command unavailable or fails

## Behavior Notes

- `check` prints only `OK` on success.
- `kernel-link` prints deprecation warning to stderr before processing.
- `run` returns an error when spawned executable exits unsuccessfully.
