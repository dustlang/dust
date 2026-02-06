# dust

`dust` is the compiler and toolchain entrypoint for the **Dust Programming Language (DPL)**.

This repository contains:
- the DPL specification (`spec/`)
- reference examples (`examples/`)
- the `dust` compiler implementation (Rust)

---

## Status

- **DPL specification:** v0.1 (**frozen**)
- **Compiler:** v0.1 (**executable milestone reached for K-regime subset**)

### What works today (v0.1 executable subset)

The compiler can produce **native executables** (ELF / Mach-O / PE) for a constrained, spec-aligned subset:

- **Entry point:** `K main { ... }`
- **Supported statements (in `main`):** ordered `emit "<string>"` effects
- **Output:** native executable that runs on the host OS/architecture

This subset exists to prove end-to-end correctness:

`.ds` → parse/check → DIR → object → link → executable → run

### What is planned next

- **Q-regime (linear):** backend + conformance tests
- **Φ-regime (admissibility):** backend + witness/constraint tooling
- Expanded K-regime statements (still deterministic and auditable)

---

## Quickstart

### Build and run an example

```bash
cargo run -p dust -- build examples/K/k_hello_world.ds
./target/dust/k_hello_world