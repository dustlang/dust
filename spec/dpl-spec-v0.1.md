# Dust Programming Language (DPL)
## Language Specification v0.1

© 2026 Dust LLC  
Licensed under the Dust Open Source License.

---

## Status of This Document

This document defines **version 0.1** of the **Dust Programming Language (DPL)**.

- **Status:** Frozen
- **Audience:** Language implementers, compiler authors, systems engineers
- **Normativity:** This specification is **normative**

If any other document, implementation, example, or commentary conflicts with this specification, **this specification takes precedence**.

---

## Scope of v0.1

DPL v0.1 defines:

- The lexical and grammatical structure of `.ds` programs
- The three-regime execution model:
  - **K** (Classical / Deterministic)
  - **Q** (Quantum / Linear)
  - **Φ** (Phase / Admissibility)
- Types, effects, constraints, and admissibility
- Binding and contract semantics between regimes
- The **Dust Intermediate Representation (DIR)** as the canonical semantic form
- Conformance requirements for compilers and tools

DPL v0.1 does **not** require:

- Native code generation
- Runtime guarantees beyond semantic validation
- Performance guarantees

---

## Design Principles

DPL is designed to:

- Treat **meaning as primary** and execution as secondary
- Make **effects explicit and auditable**
- Enforce **linearity and irreversibility** where required
- Allow **constraint satisfaction without execution**
- Support classical, quantum, and phase computation in one language
- Enable formal verification and governance

Design rationale is expanded in Appendix A.

---

## File Organization

The DPL v0.1 specification is organized into individual files.

### Front Matter
- `dpl-spec-v0.1.md`

### Core Sections
1. `01-introduction.md` — Overview and motivation
2. `02-lexical-structure.md` — Tokens, identifiers, comments
3. `03-grammar.md` — Formal grammar
4. `04-regime-model.md` — K, Q, and Φ regimes
5. `05-type-system.md` — Types and shapes
6. `06-effects.md` — Effects and irreversibility
7. `07-constraints.md` — Constraints and admissibility
8. `08-processes.md` — Processes and execution
9. `09-time-concurrency.md` — Time and concurrency
10. `10-binding-contracts.md` — Binding and contracts
11. `11-errors.md` — Error and failure model
12. `12-dir.md` — Dust Intermediate Representation
13. `13-conformance.md` — Conformance requirements
14. `14-versioning.md` — Language evolution rules
15. `15-security.md` — Security and safety considerations
16. `16-glossary.md` — Terminology

### Appendices (Non-Normative)
- `appendix-a-rationale.md`
- `appendix-b-deferred-features.md`
- `appendix-c-changelog.md`

---

## Notation Conventions

- **Keywords** appear in lowercase monospace (`proc`, `forge`, `bind`)
- **Regimes** are written as `K`, `Q`, `Φ`
- Code examples use the `.ds` extension
- “MUST”, “SHALL”, “MAY”, and “MUST NOT” are used in the RFC 2119 sense

---

## Reference Implementation

The reference compiler for DPL v0.1 is **`dust`**, hosted at:

https://github.com/dustlang/dust

The compiler is expected to:

- Parse `.ds` files
- Validate them against this specification
- Emit canonical DIR

---
