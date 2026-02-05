Here’s a v0.1 CLI design for the dust tool that matches the spec (DIR is canonical, determinism, explicit failures) and keeps the surface small but future-proof.

Command shape

dust <command> [options] [--] [args…]
	•	Default target input: . (current directory) or an explicit file/dir.
	•	DPL source files are .ds.
	•	All commands MUST be deterministic given identical inputs and environment assumptions.

⸻

v0.1 core commands

1) dust help

Shows global help, command help, and examples.
	•	dust help
	•	dust help <command>

2) dust version

Prints:
	•	dust version (semver)
	•	DIR version supported
	•	spec version targeted (e.g., DPL spec v0.1)
	•	host triple

3) dust check

Parse + validate (syntax, regimes, types, effects, constraints, bindings/contracts) without producing codegen artifacts.

Usage
	•	dust check
	•	dust check path/to/file.ds
	•	dust check examples/

Key flags
	•	--spec v0.1 (default; rejects if unsupported)
	•	--warn-as-error (treat warnings as errors)
	•	--format {text,json} (diagnostics format)
	•	--quiet / -q
	•	--verbose / -v

Outputs
	•	diagnostics only
	•	exit codes (see below)

4) dust dir

Emit canonical DIR for one or more .ds inputs.

Usage
	•	dust dir (discovers .ds under current forge/module rules)
	•	dust dir path/to/file.ds -o out/
	•	dust dir examples/Φ/phi_*.ds --print

Key flags
	•	--emit {dir} (default dir)
	•	-o, --out <path> (default: target/dir/)
	•	--print (write DIR to stdout)
	•	--format {json,text} (pick one canonical in v0.1)
	•	--stable (default on): guarantees stable ordering in emitted DIR
	•	--hash (prints content hash of DIR output for reproducibility)

Notes
	•	dust dir implies dust check first; if check fails, no DIR emitted.

5) dust build

Builds an artifact from DIR using the selected backend.

Usage
	•	dust build (build default package/forge)
	•	dust build path/to/file.ds
	•	dust build --target x86_64-unknown-linux-gnu

Key flags
	•	--target <triple> (host default)
	•	--release (default is debug-ish)
	•	-o, --out <path> (default: target/<triple>/<profile>/)
	•	--emit {bin,lib,dir} (default: bin; may also write DIR)
	•	--backend {llvm,cranelift,interp} (v0.1 default: llvm or “dir-only” if backend not implemented yet)
	•	--offline (no network; should be the default posture)

Outputs
	•	binaries and/or libraries plus optional DIR.
	•	MUST preserve DIR semantics; no observable changes.

6) dust run

Convenience wrapper: build then execute.

Usage
	•	dust run
	•	dust run -- <program-args>

Key flags
	•	all build flags apply
	•	-- separates tool args from program args

Notes
	•	If runtime is not yet part of v0.1, run can be gated or implemented only for host targets.

⸻

v0.1 “must-have” global flags
	•	--spec <version> (default v0.1)
	•	--color {auto,always,never}
	•	--format {text,json} (diagnostics)
	•	-q/-v
	•	--config <path> (optional; default dust.toml if present)
	•	--no-config (ignore config)

⸻

Exit codes (stable)
	•	0 success
	•	1 compile-time error (syntax/type/regime/effect/constraint/contract invalid)
	•	2 usage error (bad CLI args)
	•	3 internal error (bug)
	•	4 backend unavailable / unsupported target
	•	5 artifact I/O failure (permissions, disk, etc.)

(Keep this list short and never reuse meanings.)

⸻

Reserved for v0.x (not required in v0.1, but names should be protected)

These names should be reserved now so you don’t fragment later:
	•	dust fmt (formatting)
	•	dust lint (style + best practices)
	•	dust test (conformance + examples)
	•	dust verify (contracts/admissibility audits, proof checks)
	•	dust doc (spec-linked docs)
	•	dust clean (remove target/)

⸻

Defaults that fit your spec philosophy
	•	dust check is the “truth gate”
	•	dust dir is the canonical meaning artifact
	•	dust build is a backend detail downstream of DIR
	•	dust run is convenience only; never changes semantics

