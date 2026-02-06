# 9. Time & Concurrency

## 9.1 Overview

DPL v0.1 adopts a **conservative and explicit model of time**.  
Time is treated as an ordering relation, not as a continuously flowing quantity.

Concurrency is intentionally restricted in v0.1 to preserve:
- determinism,
- auditability,
- semantic clarity across regimes.

A DPL implementation MUST NOT introduce implicit concurrency or reordering.

---

## 9.2 Logical Time

### 9.2.1 Definition

**Logical time** in DPL is defined by **program order**.

Within a process:
- statements occur in the order they appear,
- effects occur in the order they appear,
- constraints apply globally to the process, independent of textual position.

Logical time is the only notion of time required for correctness in v0.1.

---

### 9.2.2 Ordering Guarantees

For any single process invocation:

- Statements are evaluated sequentially.
- Effects are performed strictly in source order.
- No statement may observe the result of a future statement.

Reordering statements or effects is forbidden.

---

## 9.3 Physical Time

### 9.3.1 Conceptual Role

**Physical time** refers to real-world temporal constraints such as:
- deadlines,
- latency bounds,
- duration limits.

In v0.1, physical time is **not executable** but **may be referenced declaratively**.

---

### 9.3.2 Physical Time Constraints

Physical time constraints MAY appear only:
- in binding contracts,
- as declarative limits (e.g., latency bounds).

Example:

```ds
bind Φ::validate -> K::execute contract {
    latency_us < 5000;
}

Such constraints:
	•	do not introduce concurrency,
	•	do not affect logical ordering,
	•	are treated as admissibility requirements.

⸻

9.4 Concurrency Model (v0.1)

9.4.1 Absence of Concurrency

DPL v0.1 defines no concurrency constructs.

Specifically:
	•	there are no threads,
	•	no async or await,
	•	no parallel blocks,
	•	no shared mutable state.

Every process is evaluated as a single, sequential semantic unit.

⸻

9.4.2 Rationale

This restriction ensures that:
	•	effect ordering is unambiguous,
	•	audit logs are deterministic,
	•	semantic failures are reproducible,
	•	regime boundaries are not violated by timing artifacts.

Concurrency is a future extension and is explicitly out of scope for v0.1.

⸻

9.5 Time and Regimes

9.5.1 K-Regime

In K-regime:
	•	logical time defines execution order,
	•	physical time constraints MAY gate admissibility,
	•	effects are sequenced deterministically.

⸻

9.5.2 Q-Regime

In Q-regime:
	•	time is implicit in linear consumption,
	•	no physical-time references are permitted inside process bodies,
	•	ordering is structural, not temporal.

⸻

9.5.3 Φ-Regime

In Φ-regime:
	•	time has no operational meaning,
	•	admissibility is timeless,
	•	constraints are evaluated without reference to execution order.

⸻

9.6 Time-Related Failure

A process or binding MAY fail if:
	•	a declared physical-time constraint cannot be satisfied,
	•	a required ordering cannot be preserved.

Such failures are semantic failures and MUST be reported deterministically.

⸻

9.7 Representation in DIR

DIR MUST preserve:
	•	the logical ordering of statements and effects,
	•	any declared physical-time constraints,
	•	the absence of concurrency semantics.

DIR MUST NOT introduce timestamps, clocks, or scheduling artifacts.

⸻

9.8 Future Extensions (Non-Normative)

Future versions of DPL may introduce:
	•	explicit concurrency constructs,
	•	time-aware effects,
	•	temporal logic constraints,
	•	schedulable process graphs.

Any such extensions MUST preserve the explicitness and auditability guarantees established in v0.1.

⸻
