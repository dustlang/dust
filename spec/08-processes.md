# 8. Processes & Execution

## 8.1 Overview

A **process** is the fundamental unit of behavior in DPL.  
Processes encapsulate computation, admissibility evaluation, or orchestration, depending on their declared regime.

Every process:
- belongs to exactly one regime (`K`, `Q`, or `Φ`),
- has a well-defined interface (parameters and optional return type),
- obeys regime-specific rules for execution, effects, and failure.

A DPL implementation MUST treat processes as **closed semantic units** whose behavior is fully determined by their declaration and inputs.

---

## 8.2 Process Declaration

Processes are declared using the `proc` keyword:

```ds
proc K::compute(x: i32) -> i32 {
    return x + 1;
}
```

A process declaration consists of:
- a fully qualified process path,
- a parameter list,
- optional `uses` clauses,
- an optional return type,
- optional qualifiers,
- a process body.

---

## 8.3 Process Invocation

A process is invoked via a call expression:

```ds
let y = compute(5);
```

Invocation semantics depend on the regime of the **callee**.

A DPL implementation MUST:
- resolve the callee unambiguously,
- type-check all arguments,
- enforce all regime, constraint, and binding rules.

---

## 8.4 Execution Semantics by Regime

### 8.4.1 K-Regime Execution

In **K-regime**:

- Processes execute sequentially.
- Statements are evaluated in source order.
- Effects occur when encountered, subject to constraints.

Execution MUST halt on semantic failure.

---

### 8.4.2 Q-Regime Execution

In **Q-regime**:

- Processes represent **linear transformations**, not conventional execution.
- Each input value MUST be consumed exactly once.
- Control flow is structurally restricted.

A Q-regime process:
- MUST consume all linear inputs,
- MUST produce outputs consistent with declared types,
- MUST NOT perform effects.

---

### 8.4.3 Φ-Regime Evaluation

In **Φ-regime**:

- Processes do not execute.
- Processes are evaluated for **admissibility only**.

A Φ-regime process:
- evaluates constraints,
- may construct witnesses,
- determines whether a configuration is admissible.

No side effects or state changes occur.

---

## 8.5 Process Body and Control Flow

### 8.5.1 Statements

Process bodies consist of the following statements:

- `let`
- `constrain`
- `prove`
- effect statements
- `return`

Control flow in v0.1 is strictly linear.  
There are no conditionals, loops, or branching constructs.

---

### 8.5.2 Return Semantics

If a process declares a return type:
- every execution path MUST reach a `return` statement,
- the returned value MUST match the declared type.

If no return type is declared:
- the process returns no value,
- `return` MUST NOT include an expression.

---

## 8.6 Uses Clauses

A process MAY declare required external resources using `uses` clauses:

```ds
proc K::log() uses Console(device="stdout") {
    emit "HELLO";
}
```

Uses clauses:
- declare dependencies,
- do not themselves cause effects,
- may constrain effect legality.

---

## 8.7 Failure and Termination

A process may fail due to:
- unsatisfied constraints,
- illegal effects,
- type or regime violations,
- binding contract violations.

Failure:
- is deterministic,
- is semantic (not exceptional),
- MUST terminate process evaluation.

Partial execution with skipped effects is forbidden.

---

## 8.8 Process Identity

Processes are identified by their fully qualified path.

In v0.1:
- processes are not first-class values,
- processes cannot be passed as parameters,
- processes cannot be dynamically created.

---

## 8.9 Processes and DIR

Each process MUST be represented explicitly in DIR, including:
- regime,
- parameters and types,
- constraints and witnesses,
- effects,
- return semantics.

DIR MUST preserve enough information to:
- re-evaluate admissibility,
- replay effect ordering,
- enforce binding contracts.

---

## 8.10 Non-Goals of v0.1

DPL v0.1 does not define:
- branching or looping control flow,
- recursion,
- dynamic process creation.

These are reserved for future versions.

---

End of Section 8.