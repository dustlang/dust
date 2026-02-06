<!-- spec/06-effects.md -->

# 6. Effects & Irreversibility

## 6.1 Overview

In DPL, **effects are explicit, ordered, and irreversible** interactions with the external world or with semantic state that cannot be undone.

Effects are not implicit consequences of computation.  
They are **declared operations** that appear syntactically in a process body and are governed by regime-specific rules.

A DPL implementation MUST:
- identify all effects explicitly,
- enforce effect legality by regime,
- preserve effect ordering,
- reject programs that attempt to hide or reorder effects.

---

## 6.2 Effect Kinds

DPL v0.1 defines three effect kinds:

- `emit` — produce an observable output
- `observe` — consume or observe an external or irreversible value
- `seal` — finalize or commit a value or state

Each effect kind represents a **one-way semantic transition**.

```ds
emit "AUDIT:BEGIN";
observe sensor_value;
seal record;


⸻

6.3 Effect Properties

All effects share the following properties:
	•	Explicit — effects MUST appear syntactically
	•	Ordered — effects occur in program order
	•	Irreversible — effects cannot be undone, replayed, or skipped
	•	Auditable — effects MUST be representable in DIR

There is no concept of a “pure” effect.

⸻

6.4 Effect Ordering

Effects are ordered strictly by source order within a process body.

emit "A";
emit "B";

The above MUST occur in the order "A" then "B".

Reordering, parallelizing, or eliminating effects is forbidden unless explicitly permitted by future language extensions.

⸻

6.5 Effects by Regime

6.5.1 K-Regime Effects

In K-regime processes:
	•	emit, observe, and seal are permitted.
	•	Effects MUST respect declared constraints and bindings.
	•	Effects MAY depend on computed values.

K-regime is the only regime that may directly interact with the external world in v0.1.

⸻

6.5.2 Q-Regime Effects

In Q-regime processes:
	•	Direct effects (emit, observe, seal) are forbidden.
	•	Any attempt to perform an effect MUST result in a static error.

This restriction ensures that linear and irreversible computation is not violated by uncontrolled observation.

Observable outcomes from Q-regime computation MUST occur via:
	•	binding into a K-regime process.

⸻

6.5.3 Φ-Regime Effects

In Φ-regime processes:
	•	All effects are forbidden.
	•	Φ-regime is non-executing and purely semantic.

Any effect statement appearing in a Φ-regime process is a static error.

⸻

6.6 Irreversibility

Once an effect is performed:
	•	it MUST be considered irreversible,
	•	its consequences MUST be assumed to have occurred,
	•	no semantic rule may “undo” it.

Irreversibility is enforced structurally:
	•	Q-regime linearity prevents reuse,
	•	Φ-regime disallows effects entirely,
	•	K-regime requires explicit ordering.

⸻

6.7 Effect Failure

An effect MAY fail due to:
	•	violated constraints,
	•	binding contract violations,
	•	invalid execution context.

Effect failure is a semantic failure, not an exception.

A DPL implementation MUST:
	•	detect effect failures deterministically,
	•	report them with source location,
	•	prevent successful compilation or execution if failure is unavoidable.

⸻

6.8 Effects and DIR

All effects MUST be represented explicitly in the Dust Intermediate Representation (DIR).

DIR MUST preserve:
	•	effect kind,
	•	effect order,
	•	effect payload.

No effect may be elided or transformed away during lowering.

⸻

6.9 Non-Goals of v0.1

DPL v0.1 does not define:
	•	effect rollback,
	•	transactional semantics,
	•	speculative or reversible effects,
	•	implicit effect inference.

These may be explored in future versions.

⸻
