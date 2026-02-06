# 7. Constraints & Admissibility

## 7.1 Overview

Constraints are the mechanism by which DPL expresses **semantic admissibility**.

A constraint does not cause execution.  
It restricts the set of configurations under which a process is considered **valid**.

Constraints are central to the Φ-regime and are also permitted in K-regime where they govern effect legality and execution eligibility.

A DPL implementation MUST treat constraints as **semantic requirements**, not as runtime conditionals.

---

## 7.2 The `constrain` Statement

The `constrain` statement introduces a predicate that MUST hold for the enclosing process to be admissible.

```ds
constrain x > 0;
constrain temperature <= 5000;
```

A `constrain` statement:
- MUST evaluate to a boolean predicate,
- MUST be side-effect free,
- MUST NOT perform execution or observation.

If a constraint is unsatisfiable, the process fails semantically.

---

## 7.3 Constraint Scope

Constraints apply to the **entire process** in which they appear.

- The textual position of a `constrain` statement does not limit its scope.
- Constraints MAY refer to:
  - process parameters,
  - locally bound values,
  - derived expressions.

All constraints in a process are considered **conjunctive** (logical AND).

---

## 7.4 Admissibility

A process is **admissible** if and only if:
- all of its constraints can be simultaneously satisfied,
- all regime rules are satisfied,
- all type rules are satisfied.

Admissibility is a semantic property, not a runtime state.

A process that is not admissible MUST NOT be executed.

---

## 7.5 Constraint Evaluation by Regime

### 7.5.1 K-Regime Constraints

In **K-regime**:

- Constraints gate execution and effects.
- Effects MUST NOT occur unless all constraints are satisfied.
- Constraint failure is a semantic failure and MUST be detected before execution.

---

### 7.5.2 Q-Regime Constraints

In **Q-regime**:

- Constraints restrict linear admissibility.
- Constraint predicates MUST be evaluable without violating linearity.
- Constraint failure invalidates the process.

---

### 7.5.3 Φ-Regime Constraints

In **Φ-regime**:

- Constraints are the primary mechanism of computation.
- A Φ-regime process may contain zero or more constraints.
- If constraints are satisfiable, the process is admissible.
- If constraints are unsatisfiable, the process fails.

Φ-regime processes do not execute; they **decide admissibility**.

---

## 7.6 Witness Construction (`prove`)

The `prove` statement constructs a **witness** to admissibility.

```ds
prove w from candidate;
```

A witness:
- is a named value derived from admissible inputs,
- represents evidence that constraints are satisfied,
- MAY be transferred across regime boundaries via bindings.

The existence of a witness implies admissibility.

---

## 7.7 Witness Semantics

- A witness MUST be derived only from values admissible under all constraints.
- A witness MUST NOT be constructed if constraints are unsatisfied.
- A witness MAY be used as a gating value for bindings or effects.

Witnesses have no meaning outside the admissibility context in which they are constructed.

---

## 7.8 Constraint Failure

Constraint failure occurs when:
- no assignment of values satisfies all constraints,
- a required predicate evaluates to false under all interpretations.

Constraint failure:
- is deterministic,
- is semantic (not exceptional),
- MUST prevent successful compilation or execution.

A DPL implementation SHOULD report:
- which constraints failed,
- their source locations.

---

## 7.9 Constraints and DIR

All constraints MUST be represented explicitly in DIR.

DIR MUST preserve:
- constraint predicates,
- their association with processes,
- witness construction relationships.

No constraint may be elided or weakened during lowering.

---

## 7.10 Non-Goals of v0.1

DPL v0.1 does not define:
- probabilistic constraints,
- soft or weighted constraints,
- constraint optimization or search strategies.

These may be introduced in future versions.

---

End of Section 7.