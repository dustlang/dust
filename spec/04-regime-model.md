# 4. Regime Model

## 4.1 Overview

DPL is structured around a **three-regime computational model**.  
Every process operates entirely within **exactly one regime**.

The regimes are:

- **K** — Classical / Deterministic
- **Q** — Quantum / Linear
- **Φ** — Phase / Admissibility

Each regime defines:
- permitted operations,
- information flow rules,
- effect legality,
- failure semantics.

A DPL implementation MUST enforce regime rules structurally and MUST reject programs that violate them.

---

## 4.2 Regime Declaration

Every process MUST declare its regime explicitly:

```ds
proc K::compute() { ... }
proc Q::transform(x: QState) -> QState linear { ... }
proc Φ::admissible() { ... }
```

The declared regime applies to:
- the entire process body,
- all statements within,
- all semantic checks for that process.

A process MUST NOT change regimes internally.

---

## 4.3 K-Regime (Classical / Deterministic)

### 4.3.1 Purpose

The **K-regime** is used for:
- deterministic computation,
- orchestration and control,
- interaction with the external world,
- coordination of Q and Φ processes.

---

### 4.3.2 Semantics

In K-regime:

- Values MAY be copied and reused.
- Control flow is deterministic and sequential.
- Effects are permitted but MUST be explicit.
- Execution proceeds in source order.

---

### 4.3.3 Effects

K-regime processes MAY perform:

- `emit`
- `observe`
- `seal`

Effects:
- MUST occur in program order,
- MUST respect constraints and bindings,
- MUST be representable in DIR.

---

### 4.3.4 Failure

K-regime failure occurs due to:
- unsatisfied constraints,
- illegal effects,
- binding contract violations.

Failures are semantic and MUST be reported deterministically.

---

## 4.4 Q-Regime (Quantum / Linear)

### 4.4.1 Purpose

The **Q-regime** models computation where:
- information is non-duplicable,
- consumption is irreversible,
- linear usage is enforced.

---

### 4.4.2 Linearity Rules

In Q-regime:

- Values MUST be used exactly once.
- Values MUST NOT be copied.
- Values MUST NOT be discarded without consumption.
- Aliasing is forbidden.

These rules are structural and MUST be enforced by the compiler.

---

### 4.4.3 Qualifiers

All Q-regime processes MUST be declared `linear` in v0.1:

```ds
proc Q::step(x: QState) -> QState linear { ... }
```

---

### 4.4.4 Effects

In v0.1:

- Q-regime processes MUST NOT perform effects.
- Observable outcomes MUST occur via binding into K-regime.

---

### 4.4.5 Failure

Q-regime failure occurs if:
- a value is used more than once,
- a value is not consumed,
- an illegal effect is attempted.

Such failures MUST be detected statically where possible.

---

## 4.5 Φ-Regime (Phase / Admissibility)

### 4.5.1 Purpose

The **Φ-regime** expresses:
- constraints,
- admissibility conditions,
- semantic validity.

Φ-regime processes do not execute.

---

### 4.5.2 Non-Execution

In Φ-regime:

- No effects are permitted.
- No external interaction occurs.
- Evaluation determines admissibility only.

---

### 4.5.3 Constraints and Witnesses

Φ-regime processes MAY:
- declare constraints using `constrain`,
- construct witnesses using `prove`.

If constraints are unsatisfiable, the process fails semantically.

---

### 4.5.4 Determinism

Φ-regime evaluation MUST be deterministic with respect to:
- input values,
- constraint definitions.

---

## 4.6 Cross-Regime Interaction

Direct interaction between regimes is forbidden.

All cross-regime interaction MUST occur via **bindings**:

```ds
bind Φ::validate -> K::execute contract { ... }
```

Bindings define allowable information flow and are governed by contracts.

---

## 4.7 Enforcement

A DPL implementation MUST:
- identify the regime of every process,
- apply the correct rule set,
- reject programs that violate regime boundaries,
- preserve regime information in DIR.

---

End of Section 4.