# Appendix B. Deferred Features

## B.1 Purpose of This Appendix

This appendix enumerates features that are **intentionally excluded** from DPL v0.1.

The absence of these features is deliberate.  
They are deferred to preserve semantic clarity, determinism, and auditability in the foundational version of the language.

Nothing in this appendix is normative.

---

## B.2 Control Flow Constructs

The following control flow constructs are deferred:

- conditional branching (`if`, `else`)
- pattern matching
- loops (`for`, `while`)
- recursion

**Rationale:**  
These constructs intertwine execution with semantics. DPL v0.1 prioritizes linear, explicit reasoning. Control flow will be reintroduced only when it can be made explicit and auditable.

---

## B.3 Concurrency and Parallelism

Deferred features include:

- threads
- asynchronous execution
- futures and promises
- parallel blocks
- shared mutable state

**Rationale:**  
Concurrency introduces nondeterminism and timing-dependent behavior. DPL v0.1 establishes a deterministic semantic baseline first.

---

## B.4 Advanced Type System Features

The following type system features are deferred:

- parametric (generic) types
- sum and union types
- option and result types
- dependent types
- type inference beyond local expressions

**Rationale:**  
The v0.1 type system is intentionally minimal to ensure unambiguous DIR emission and regime enforcement.

---

## B.5 String Semantics and Text Processing

Deferred string-related features include:

- first-class string types
- string concatenation
- string interpolation
- encoding-aware text operations

**Rationale:**  
Strings in v0.1 are treated as opaque payloads for effects and diagnostics only.

---

## B.6 Effect Extensions

The following effect-related features are deferred:

- reversible effects
- transactional effects
- speculative effects
- effect handlers
- effect polymorphism

**Rationale:**  
DPL v0.1 enforces strict irreversibility and ordering to simplify reasoning.

---

## B.7 Runtime and Execution Model

Deferred runtime features include:

- a standardized runtime environment
- memory management semantics
- garbage collection
- execution scheduling policies

**Rationale:**  
DPL v0.1 is focused on semantic correctness, not execution strategy.

---

## B.8 Module and Package Systems

Deferred structural features include:

- imports and exports
- module hierarchies
- package management
- versioned dependencies

**Rationale:**  
Forges provide sufficient structural organization for v0.1.

---

## B.9 Tooling and Debugging

Deferred tooling features include:

- interactive debuggers
- step-through execution
- runtime introspection
- performance profiling

**Rationale:**  
DIR-based analysis is the primary tooling model for v0.1.

---

## B.10 Summary

All deferred features are candidates for future versions.  
Their absence in v0.1 is a **design decision**, not a limitation.

Future inclusion requires:
- explicit semantics,
- preservation of determinism,
- compatibility with the regime model.

---

End of Appendix B.