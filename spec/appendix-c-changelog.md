# Appendix C. Change Log (Expanded)

## C.1 Purpose of This Appendix

This appendix records the **authoritative change history** for the Dust Programming Language (DPL) specification.

It exists to:
- provide historical traceability,
- document intentional design decisions,
- prevent silent semantic drift.

This appendix is **non-normative** but authoritative for version history.

---

## C.2 Version v0.1

### Status
- **Version:** v0.1
- **Status:** Frozen
- **Release:** Initial public specification

---

### C.2.1 Scope of v0.1

DPL v0.1 establishes the **foundational semantic model** of the language, including:

- lexical structure and grammar
- three-regime model (K, Q, Φ)
- explicit effects and irreversibility
- constraint-based admissibility
- linearity enforcement
- binding and contract semantics
- deterministic error and failure model
- Dust Intermediate Representation (DIR)
- conformance requirements
- versioning and security considerations

---

### C.2.2 Major Design Decisions Introduced

The following decisions were introduced and locked in v0.1:

- Separation of computation into three explicit regimes
- Prohibition of implicit effects
- Prohibition of implicit concurrency
- Elimination of undefined behavior
- Deterministic, canonical intermediate representation
- Explicit semantic failure distinct from errors
- Constraint-first admissibility model
- Explicit binding contracts for cross-regime interaction

These decisions are foundational and MUST NOT be altered without a major version increment.

---

### C.2.3 Explicit Exclusions

The following were explicitly excluded from v0.1:

- conditionals and loops
- concurrency and parallelism
- advanced type system features
- execution runtime specification
- optimization semantics
- dynamic binding or module systems

These exclusions are documented in Appendix B.

---

### C.2.4 Editorial Notes

- The specification is split into one file per section to enable controlled evolution.
- Terminology is defined normatively in Section 16 (Glossary).
- All examples and reference materials associated with v0.1 are considered normative for conformance testing.

---

## C.3 Future Versions

Future versions of DPL may introduce:

- additional control flow constructs
- explicit concurrency models
- richer type systems
- standardized DIR serialization
- execution backends

All such changes MUST follow the versioning rules defined in Section 14.

---

## C.4 Amendment Policy

Once a version is frozen:

- no semantic changes are permitted,
- only clarifications and editorial fixes MAY be applied,
- any semantic change requires a new version.

DPL v0.1 is frozen under this policy.

---

End of Appendix C.