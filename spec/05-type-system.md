# 5. Type System

## 5.1 Overview

The DPL v0.1 type system provides **explicit, static structure** for values manipulated by processes in all regimes.

The type system is intentionally conservative. Its primary goals are:

- to make data structure explicit,
- to support regime-specific semantic enforcement,
- to enable deterministic DIR emission,
- to prevent implicit or ambiguous interpretation of values.

The v0.1 type system is **nominal, structural, and static**.

---

## 5.2 Type Categories

DPL v0.1 defines the following categories of types:

- **Primitive types**
- **Named types (Shapes)**
- **Process-local value types**

No implicit type conversion is permitted.

---

## 5.3 Primitive Types

The following primitive types are defined in DPL v0.1:

| Type  | Description                          |
|------|--------------------------------------|
| `i32` | 32-bit signed integer                |
| `i64` | 64-bit signed integer                |
| `f32` | 32-bit floating-point number         |
| `f64` | 64-bit floating-point number         |
| `bool` | Boolean value (`true` or `false`)   |

### 5.3.1 Literal Typing

- Integer literals are initially untyped and MUST be resolved to a concrete integer type by context.
- Boolean literals are of type `bool`.
- String literals are not first-class typed values in v0.1 and are treated as opaque payloads where permitted.

Type mismatch MUST result in a static error.

---

## 5.4 Shapes (Structured Types)

### 5.4.1 Shape Declaration

Shapes define named structured types:

```ds
shape Measurement {
    value: f64;
    valid: bool;
}

A shape:
	•	defines a nominal type,
	•	consists of named fields with explicit types,
	•	has no methods or behavior.

Field names within a shape MUST be unique.

⸻

5.4.2 Shape Usage

Shape types may be used:
	•	as process parameters,
	•	as return types,
	•	within expressions via field access.

Example:

let m = Measurement { value: 3.14, valid: true };
let v = m.value;


⸻

5.5 Type References

A type reference in v0.1 is:

type_ref ::= identifier

The identifier MUST resolve to:
	•	a primitive type, or
	•	a shape defined within the current forge or imported scope.

Unresolved type identifiers are static errors.

⸻

5.6 Type Checking Rules

5.6.1 Let Bindings

For a let statement:

let x = expression;

The type of x is the inferred type of expression.

Rebinding the same identifier within the same scope is forbidden.

⸻

5.6.2 Expressions

Expressions are type-checked recursively.
	•	Binary arithmetic operators (+, -, *, /) require numeric operands.
	•	Comparison operators require operands of compatible types and produce bool.
	•	Logical operators (&&, ||) require bool operands.

Type mismatch MUST be reported as a static error.

⸻

5.6.3 Field Access

Field access:

expr.field

is valid only if:
	•	expr has a shape type,
	•	field is a declared field of that shape.

Otherwise, a static error MUST be raised.

⸻

5.6.4 Function and Process Calls

A call expression:

callee(arg1, arg2)

is type-checked by:
	•	resolving the callee to a process,
	•	matching argument types against parameter types,
	•	ensuring all regime and linearity rules are satisfied.

Return type mismatches MUST result in static errors.

⸻

5.7 Return Types

If a process declares a return type:

proc K::compute() -> i32 { ... }

	•	every execution path MUST return a value of that type.

If no return type is declared:
	•	the process returns no value,
	•	a return statement MUST NOT include an expression.

⸻

5.8 Regime Interaction and Types

Type correctness alone is insufficient for validity.

In addition to type checking:
	•	K-regime allows unrestricted reuse of typed values.
	•	Q-regime enforces linear usage of typed values.
	•	Φ-regime treats typed values as symbolic carriers for admissibility checking.

A program MUST satisfy both type rules and regime rules to be valid.

⸻

5.9 Type Errors

Type errors include, but are not limited to:
	•	undefined type names,
	•	mismatched operand types,
	•	invalid field access,
	•	incorrect argument or return types.

Type errors MUST be detected statically and MUST prevent successful compilation.

⸻

5.10 Future Extensions (Non-Normative)

Future versions of DPL may introduce:
	•	parametric (generic) types,
	•	sum and option types,
	•	explicit string types,
	•	dependent typing elements.

Such features are intentionally excluded from v0.1.

⸻
