<!-- spec/README.md -->

# DPL Specification

This directory contains the **normative** specification for the **Dust Programming Language (DPL)**.

- **Spec version:** v0.1 (frozen)
- **Source file extension:** `.ds`
- **Compiler/toolchain entrypoint:** `dust`

> **Normative rule:** If anything outside `spec/` conflicts with the spec, the spec wins.

---

## How to read v0.1

The v0.1 spec is split into **one section per file** so each chapter can evolve independently in future versions.

Start here:

1. `dpl-spec-v0.1.md` — master table of contents + front matter  
2. Then read sections in order: `01-*.md`, `02-*.md`, …  
3. Appendices are non-normative but authoritative design context.

---

## File map (v0.1)

### Front matter
- `dpl-spec-v0.1.md`

### Core sections
- `01-introduction.md`
- `02-lexical-structure.md`
- `03-grammar.md`
- `04-regime-model.md`
- `05-type-system.md`
- `06-effects.md`
- `07-constraints.md`
- `08-processes.md`
- `09-time-concurrency.md`
- `10-binding-contracts.md`
- `11-errors.md`
- `12-dir.md`
- `13-conformance.md`
- `14-versioning.md`
- `15-security.md`
- `16-glossary.md`

### Appendices
- `appendix-a-rationale.md`
- `appendix-b-deferred-features.md`
- `appendix-c-changelog.md`

---

## Versioning policy

- v0.1 is **locked**.
- Any semantic change requires a new spec version.
- Additive clarifications should be made via v0.2+ per `14-versioning.md`.

---

© 2026 Dust LLC
