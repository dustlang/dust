# 15. Security & Safety Considerations

## 15.1 Overview

Security and safety in DPL arise primarily from **explicit semantics**, not from implicit runtime mechanisms.

DPL v0.1 is designed to:
- minimize implicit behavior,
- make effects auditable,
- prevent accidental misuse of irreversible operations,
- support analysis and verification.

This section outlines the security and safety properties guaranteed by the language, as well as its limitations.

---

## 15.2 Effect Containment

Effects are the primary security boundary in DPL.

- All effects MUST be explicit.
- Effects MUST occur in source order.
- Effects MUST be representable in DIR.

Because effects cannot be hidden or reordered, DPL programs are inherently auditable.

In v0.1, only K-regime processes may perform effects.

---

## 15.3 Regime Isolation

The three regimes provide structural isolation:

- **K-regime** isolates external interaction.
- **Q-regime** enforces linearity and prevents duplication.
- **Φ-regime** forbids execution and effects entirely.

Cross-regime interaction is only possible via explicit bindings governed by contracts.

This isolation reduces the risk of unintended side effects or information leaks.

---

## 15.4 Constraint-Based Safety

Constraints allow developers to express safety conditions declaratively.

Examples include:
- value bounds,
- resource limits,
- physical-time constraints.

Programs that violate safety constraints fail semantically and MUST NOT execute.

This enables fail-safe behavior by construction.

---

## 15.5 Binding Contracts as Security Gates

Binding contracts act as explicit security gates.

Contracts may restrict:
- witness transfer,
- effect permission,
- latency or resource usage,
- linearity preservation.

If a contract cannot be satisfied, the binding fails and execution is prevented.

---

## 15.6 Absence of Undefined Behavior

DPL v0.1 defines **no undefined behavior**.

Any construct not explicitly permitted by the specification is invalid and MUST result in an error.

This eliminates entire classes of security vulnerabilities common in languages with undefined behavior.

---

## 15.7 Toolchain Trust Model

The DPL specification assumes:

- the compiler correctly implements the specification,
- DIR faithfully represents program semantics,
- downstream tools respect DIR semantics.

DPL does not attempt to protect against a malicious compiler.

---

## 15.8 Limitations

DPL v0.1 does not address:

- sandboxing of emitted code,
- runtime memory safety guarantees,
- side-channel resistance,
- adversarial resource exhaustion.

These concerns may be addressed by future versions or external systems.

---

## 15.9 Non-Goals of v0.1

DPL v0.1 does not define:

- a security policy language,
- access control primitives,
- cryptographic guarantees.

Security emerges from explicit semantics rather than policy enforcement.

---

End of Section 15.