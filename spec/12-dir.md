# 12. Dust Intermediate Representation (DIR)

## 12.1 Overview

The **Dust Intermediate Representation (DIR)** is the **canonical semantic form** of a DPL program.

DIR is not an optimization target and not an execution format.  
It is a **lossless, deterministic representation of meaning**.

Any DPL compiler or toolchain component that claims conformance MUST be able to emit DIR for valid programs.

If two DPL programs are semantically equivalent under this specification, their emitted DIR MUST be equivalent modulo stable ordering.

---

## 12.2 Purpose of DIR

DIR exists to:

- separate **meaning** from **execution**
- provide a stable interface for tooling
- enable formal verification and auditing
- support multi-backend compilation
- allow regime-aware analysis

DIR is the single source of truth downstream of parsing and semantic validation.

---

## 12.3 DIR Emission Requirements

A DPL implementation MUST emit DIR if and only if:

- the program is lexically valid,
- the program is syntactically valid,
- the program is type-correct,
- all regime rules are satisfied.

Programs that are valid but fail semantically MAY emit DIR annotated with failure conditions.

Programs containing errors MUST NOT emit DIR.

---

## 12.4 Determinism Guarantees

DIR emission MUST be deterministic.

Specifically:
- identical source input MUST produce identical DIR,
- emission MUST NOT depend on timestamps, memory layout, or hash iteration order,
- ordering of elements MUST be stable and specified.

Determinism is a conformance requirement.

---

## 12.5 High-Level Structure

DIR represents the following entities explicitly:

- forges
- shapes and fields
- processes and regimes
- parameters and return types
- constraints and witnesses
- effects and their ordering
- bindings and contracts
- failure annotations (if any)

Every entity MUST be uniquely identifiable.

---

## 12.6 Regime Preservation

DIR MUST preserve regime information exactly.

- Each process is tagged with its regime.
- Regime boundaries are explicit.
- No cross-regime lowering may erase regime distinctions.

Any backend that consumes DIR MUST respect regime semantics.

---

## 12.7 Effects in DIR

Effects are first-class DIR elements.

DIR MUST record for each effect:
- effect kind (`emit`, `observe`, `seal`)
- effect payload
- source order
- enclosing process

Effects MUST NOT be reordered or eliminated during DIR emission.

---

## 12.8 Constraints and Witnesses in DIR

DIR MUST represent:

- all constraint predicates,
- their association with processes,
- witness construction relationships,
- admissibility outcomes where known.

Constraint logic MUST remain inspectable in DIR.

---

## 12.9 Bindings and Contracts in DIR

DIR MUST include:

- binding direction,
- source and target processes,
- all contract clauses verbatim,
- admissibility status of bindings if known.

No contract information may be weakened or inferred away.

---

## 12.10 Serialization Format

The concrete serialization format of DIR (e.g., JSON, binary, text) is **implementation-defined** in v0.1.

However, any chosen format MUST:

- be machine-readable,
- preserve all semantic information,
- support stable ordering.

Future versions may standardize a canonical wire format.

---

## 12.11 Non-Goals of v0.1

DPL v0.1 does not define:

- DIR optimization passes,
- execution semantics for DIR,
- incremental DIR updates.

DIR is a semantic artifact only.

---

End of Section 12.