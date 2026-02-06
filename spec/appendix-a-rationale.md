# Appendix A. Rationale and Design Notes

## A.1 Purpose of This Appendix

This appendix provides **non-normative design rationale** for DPL v0.1.

It explains *why* certain design decisions were made, without altering or extending the normative rules defined in Sections 1–16.

Nothing in this appendix may override or reinterpret the core specification.

---

## A.2 Meaning Before Execution

Most programming languages treat execution as primary and meaning as implicit.

DPL inverts this relationship.

- **Meaning** (admissibility, constraints, intent) is established first.
- **Execution** is permitted only when meaning is valid.

This design is critical for domains where:
- incorrect execution is more dangerous than non-execution,
- correctness must be established before action,
- semantic auditability is required.

---

## A.3 Why Three Regimes

The separation into **K**, **Q**, and **Φ** regimes is deliberate and structural.

- **K-regime** isolates classical execution and effects.
- **Q-regime** enforces irreversibility and linearity.
- **Φ-regime** removes execution entirely and focuses on admissibility.

Attempting to unify these into a single semantic model leads to ambiguity, hidden effects, or unverifiable behavior.

Explicit regimes make illegal states unrepresentable.

---

## A.4 Explicit Effects

Hidden effects are a major source of bugs, security vulnerabilities, and audit failures.

By requiring effects to be:
- explicit,
- ordered,
- irreversible,

DPL makes every external interaction visible and inspectable.

This enables:
- deterministic replay,
- auditing,
- formal reasoning about system behavior.

---

## A.5 No Implicit Concurrency

Concurrency introduces nondeterminism, timing dependencies, and complex failure modes.

DPL v0.1 forbids concurrency to:
- preserve determinism,
- simplify semantic reasoning,
- ensure effect order is unambiguous.

Concurrency is deferred to future versions where it can be introduced explicitly and safely.

---

## A.6 Constraints Instead of Conditionals

Traditional conditionals (`if`, `else`) intertwine control flow and semantics.

DPL separates them:
- **constraints** define what is allowed,
- **process execution** happens only if constraints are satisfied.

This separation enables:
- earlier failure detection,
- symbolic reasoning,
- admissibility checks without execution.

---

## A.7 Why No Undefined Behavior

Undefined behavior transfers responsibility from the language to the programmer.

DPL explicitly rejects this model.

In DPL:
- anything not explicitly permitted is invalid,
- all invalid constructs are errors,
- all valid constructs have defined meaning.

This is essential for safety-critical and high-assurance systems.

---

## A.8 DIR as a First-Class Artifact

DIR exists to decouple:
- parsing,
- semantic validation,
- execution,
- analysis.

By making DIR canonical and deterministic, DPL enables:
- multiple backends,
- independent verification,
- long-term archival of meaning.

DIR is intentionally not optimized or executable.

---

## A.9 Why v0.1 Is Conservative

DPL v0.1 prioritizes:
- correctness over convenience,
- explicitness over brevity,
- semantic clarity over expressiveness.

Many features commonly found in programming languages are intentionally deferred to avoid premature complexity.

---

## A.10 Looking Forward

Future versions of DPL may introduce:
- controlled concurrency,
- richer type systems,
- temporal logic,
- execution backends.

All such features must preserve the foundational guarantees established in v0.1.

---

End of Appendix A.