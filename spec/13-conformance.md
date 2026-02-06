# 13. Conformance Requirements

## 13.1 Overview

This section defines what it means for a program, compiler, or tool to be **conformant** with the Dust Programming Language (DPL) v0.1 specification.

Conformance is **binary**: an implementation either conforms to v0.1 or it does not.

Partial or informal conformance claims are not permitted.

---

## 13.2 Program Conformance

A DPL program is **v0.1 conformant** if and only if:

- it is lexically valid according to Section 2,
- it is syntactically valid according to Section 3,
- it is type-correct according to Section 5,
- it obeys all regime rules defined in Section 4,
- all constraints and bindings are admissible under Sections 7 and 10.

A conformant program MAY still fail semantically (e.g., due to unsatisfied constraints), but it MUST NOT violate any language rules.

---

## 13.3 Compiler Conformance

A compiler claiming DPL v0.1 conformance MUST:

1. Parse `.ds` source files according to the grammar.
2. Enforce lexical, syntactic, type, and regime rules.
3. Detect and report all errors deterministically.
4. Detect semantic failures where possible.
5. Emit DIR for valid programs as defined in Section 12.
6. Preserve determinism in DIR emission.

A conformant compiler MUST NOT:
- accept invalid programs,
- silently ignore errors or failures,
- emit non-deterministic DIR.

---

## 13.4 Tooling Conformance

Tools that consume DPL or DIR (e.g., analyzers, verifiers, visualizers) are conformant if they:

- do not reinterpret or weaken semantics,
- preserve regime distinctions,
- respect effect ordering and irreversibility,
- respect constraints and contracts.

Tools MUST treat DIR as authoritative.

---

## 13.5 Diagnostic Requirements

A conformant implementation MUST provide diagnostics that include:

- error or failure category,
- human-readable explanation,
- precise source location or reference,
- violated rule or constraint when applicable.

Diagnostics MUST be stable and reproducible across runs.

---

## 13.6 Version Identification

A conformant implementation MUST:

- identify itself as supporting **DPL v0.1**,
- reject programs requiring newer language features,
- not claim forward compatibility without explicit support.

---

## 13.7 Undefined Behavior

DPL v0.1 defines **no undefined behavior**.

Any construct not explicitly permitted by the specification is invalid and MUST result in an error.

---

## 13.8 Conformance Testing

Conformance MAY be demonstrated via:

- static test suites,
- DIR equivalence checks,
- reference example validation.

Reference examples provided with the specification are normative for v0.1.

---

## 13.9 Non-Goals of v0.1

DPL v0.1 does not define:

- performance benchmarks,
- optimization requirements,
- backend-specific behavior.

Conformance is semantic, not operational.

---

End of Section 13.