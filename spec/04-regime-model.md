# 4. Regime Model

## 4.1 Overview

DPL is structured around a **three-regime computational model**.  
Every executable or evaluative unit in DPL operates entirely within **exactly one regime**.

The regimes are:

- **K** — Classical / Deterministic
- **Q** — Quantum / Linear
- **Φ** — Phase / Admissibility

Regimes are not interchangeable. Each regime defines:
- what operations are permitted,
- how information may flow,
- what constitutes failure,
- how effects and irreversibility are treated.

A DPL implementation MUST enforce regime rules structurally and MUST reject programs that violate regime semantics.

---

## 4.2 Regime Declaration

Every process MUST declare its regime explicitly as part of its path:

```ds
proc K::compute() { ... }
proc Q::transform() linear { ... }
proc Φ::admissible_state() { ... }

The declared regime applies to:
	•	the process body,
	•	all statements within that body,
	•	all semantic checks performed on that process.

A process MUST NOT change regimes internally.

⸻

4.3 K-Regime (Classical / Deterministic)

4.3.1 Purpose

The K-regime is used for:
	•	deterministic computation,
	•	control flow and orchestration,
	•	interaction with the external world,
	•	coordination of Q and Φ processes.

It is the default “spine” of a DPL program.

⸻

4.3.2 Semantics

In K-regime:
	•	Values MAY be copied and reused.
	•	Control flow is deterministic.
	•	Effects are permitted, but MUST be explicit.
	•	Execution proceeds sequentially in program order unless otherwise specified.

K-regime is not linear: duplication and reuse of values are allowed.

⸻

4.3.3 Effects

K-regime processes MAY contain:
	•	emit
	•	observe
	•	seal

Effects:
	•	MUST occur in program order,
	•	MUST respect any declared constraints,
	•	MUST be representable in DIR.

Effect semantics are defined in Section 6.

⸻

4.3.4 Failure

A K-regime process MAY fail due to:
	•	violated constraints,
	•	invalid effect ordering,
	•	binding contract violations.

Such failures are semantic failures and MUST be reported by the compiler or toolchain.

⸻

4.4 Q-Regime (Quantum / Linear)

4.4.1 Purpose

The Q-regime models computation where:
	•	information is non-duplicable,
	•	consumption is irreversible,
	•	linear usage must be enforced.

This regime is inspired by quantum computation and linear logic, but is not restricted to physical quantum hardware.

⸻

4.4.2 Linearity

In Q-regime:
	•	Values MUST be used exactly once.
	•	Values MUST NOT be copied.
	•	Values MUST NOT be discarded without explicit consumption.
	•	Aliasing is forbidden.

These rules are structural and MUST be enforced by the compiler.

⸻

4.4.3 Qualifiers

All Q-regime processes MUST be declared linear in v0.1:

proc Q::step(x: QState) -> QState linear { ... }

Future versions MAY relax or refine this requirement, but v0.1 implementations MUST enforce it.

⸻

4.4.4 Effects

In v0.1:
	•	Q-regime processes MUST NOT directly perform external effects.
	•	Observable effects MUST occur via binding to K-regime processes.

This separation ensures linearity is not violated by uncontrolled observation.

⸻

4.4.5 Failure

A Q-regime process MUST fail if:
	•	a value is used more than once,
	•	a value is not consumed,
	•	an illegal effect is attempted.

Such failures are semantic and MUST be detected statically where possible.

⸻

4.5 Φ-Regime (Phase / Admissibility)

4.5.1 Purpose

The Φ-regime is used to express:
	•	constraints,
	•	admissibility conditions,
	•	semantic validity,
	•	existence of witnesses.

Φ-regime computation does not represent execution in the classical sense.

⸻

4.5.2 Non-Execution

In Φ-regime:
	•	Statements do not cause external effects.
	•	No interaction with the external world is permitted.
	•	Evaluation determines whether a configuration is admissible.

A Φ-regime process answers the question:

“Is this configuration semantically allowed?”

⸻

4.5.3 Constraints and Witnesses

Φ-regime processes may:
	•	declare constraints using constrain,
	•	construct witnesses using prove.

If constraints are unsatisfiable, the process fails.

Constraint semantics are defined in Section 7.

⸻

4.5.4 Determinism

Although Φ-regime processes do not “execute”, their evaluation MUST be deterministic with respect to:
	•	input values,
	•	constraint definitions,
	•	admissibility rules.

⸻

4.6 Cross-Regime Interaction

Direct interaction between regimes is forbidden.

All cross-regime interaction MUST occur via bindings, governed by explicit contracts:

bind Φ::admissible -> K::execute contract { ... }

Bindings:
	•	define allowable information flow,
	•	enforce constraints on that flow,
	•	prevent regime rule violations.

Binding semantics are defined in Section 10.

⸻

4.7 Regime Enforcement

A DPL implementation MUST:
	•	statically identify the regime of every process,
	•	apply the correct rule set to each process,
	•	reject programs that violate regime boundaries,
	•	preserve regime information in DIR.

Regime violations MUST be reported as semantic errors.

⸻
