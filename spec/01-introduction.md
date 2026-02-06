# DPL Specification

This directory contains the **normative specification** for the **Dust Programming Language (DPL)**.

- **Specification version:** v0.1 (frozen)
- **Source file extension:** `.ds`
- **Compiler and toolchain entrypoint:** `dust`

If any document, implementation, example, or commentary conflicts with the contents of this directory, **the specification in this directory takes precedence**.

---

## How to Read the Specification

The DPL v0.1 specification is intentionally split into **one file per section**.

This structure allows:
- independent evolution of sections,
- precise versioning,
- clear semantic authority.

Recommended reading order:

1. `dpl-spec-v0.1.md` — master document and table of contents  
2. Section files in numerical order (`01-*` through `16-*`)  
3. Appendices (design rationale and history)

---

## Specification Structure (v0.1)

### Front Matter
- `dpl-spec-v0.1.md`

### Core Sections
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

### Appendices (Non-Normative)
- `appendix-a-rationale.md`
- `appendix-b-deferred-features.md`
- `appendix-c-changelog.md`

---

## Versioning Policy

- DPL v0.1 is **locked**.
- Any semantic change requires a new version.
- Clarifications without semantic change MAY appear in patch revisions.
- Future features MUST NOT retroactively alter v0.1 meaning.

---

## Authority

This specification defines what it means for a program, compiler, or tool to be **DPL v0.1 conformant**.

All conformance claims MUST be evaluated against these documents.

---

© 2026 Dust LLC