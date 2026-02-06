# 10. Binding & Contracts

## 10.1 Overview

**Bindings** are the sole mechanism by which processes interact across regime boundaries in DPL.

A binding:
- connects a **source process** to a **target process**,
- declares a **contract** that governs the interaction,
- enforces admissibility, ordering, and regime safety.

Direct invocation across regimes without a binding is forbidden.

A DPL implementation MUST enforce all bindings and contracts statically where possible and semantically at evaluation time otherwise.

---

## 10.2 Binding Declaration

Bindings are declared using the `bind` keyword:

```ds
bind Φ::validate -> K::execute contract {
    witness_transfer == true;
    effects_allowed == true;
}

10.2.1 Directionality

Bindings are directional:

bind A -> B contract { ... }

This means:
	•	A MAY feed into B,
	•	B MUST NOT feed into A unless a separate binding exists.

Directionality is semantic and MUST be preserved in DIR.

⸻

10.3 Process References

A binding references processes using either:
	•	a fully qualified path (K::run), or
	•	an unqualified identifier resolved within the same forge.

Bindings MUST resolve to exactly one source process and one target process.

Unresolvable or ambiguous references are static errors.

⸻

10.4 Contracts

A contract is a set of declarative clauses that constrain a binding.

contract {
    latency_us < 5000;
    witness_transfer == true;
}

Contracts:
	•	do not execute,
	•	do not produce effects,
	•	define admissibility requirements for the binding.

All contract clauses are conjunctive.

⸻

10.5 Contract Clauses

10.5.1 Clause Structure

Each clause has the form:

identifier operator value ;

Where:
	•	identifier names a contract attribute,
	•	operator is a comparison operator,
	•	value is a literal or identifier.

⸻

10.5.2 Common Contract Attributes (v0.1)

The following attributes are recognized in v0.1:

Attribute	Meaning
witness_transfer	Whether a Φ witness may cross the binding
effects_allowed	Whether the target may perform effects
latency_us	Upper bound on acceptable latency
linearity_enforced	Whether linearity must be preserved

An implementation MAY accept additional attributes but MUST NOT reinterpret these.

⸻

10.6 Binding Semantics by Regime

10.6.1 Φ → K Binding

A Φ → K binding:
	•	transfers admissibility information,
	•	MAY transfer witnesses if allowed by contract,
	•	gates K-regime execution on Φ-regime admissibility.

If the Φ process is not admissible, the K process MUST NOT execute.

⸻

10.6.2 Q → K Binding

A Q → K binding:
	•	transfers the result of a linear computation,
	•	MUST respect linearity,
	•	MAY allow effects only in the K process.

Measurement or observation MUST occur in K-regime.

⸻

10.6.3 K → Q Binding

A K → Q binding:
	•	injects classical values into a linear context,
	•	MUST ensure no duplication of linear resources occurs.

Such bindings are subject to strict admissibility checks.

⸻

10.7 Contract Enforcement

A binding is admissible if and only if:
	•	all contract clauses are satisfied,
	•	both processes are individually admissible,
	•	regime rules are not violated.

If a contract clause cannot be satisfied, the binding fails semantically.

Bindings MUST NOT partially succeed.

⸻

10.8 Failure Modes

Binding-related failures include:
	•	unsatisfied contract clauses,
	•	invalid witness transfer,
	•	illegal effect permissions,
	•	regime boundary violations.

These failures are semantic and MUST be reported deterministically.

⸻

10.9 Representation in DIR

DIR MUST represent:
	•	binding direction,
	•	source and target process identities,
	•	all contract clauses verbatim.

DIR MUST NOT weaken or omit contract information.

⸻

10.10 Non-Goals of v0.1

DPL v0.1 does not define:
	•	dynamic binding creation,
	•	conditional bindings,
	•	binding polymorphism.

These may be introduced in future versions.

⸻

