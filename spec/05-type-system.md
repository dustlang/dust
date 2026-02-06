# 5. Type System

## 5.1 Overview

The DPL v0.1 type system provides **explicit, static structure** for values used across all regimes.

Its goals are to:
- make data structure explicit,
- support regime-specific semantic enforcement,
- enable deterministic DIR emission,
- prevent implicit or ambiguous interpretation of values.

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

| Type  | Description                    |
|------:|--------------------------------|
| `i32` | 32-bit signed integer          |
| `i64` | 64-bit signed integer          |
| `f32` | 32-bit floating-point number   |
| `f64` | 64-bit floating-point number   |
| `bool`| Boolean value                  |

### 5.3.1 Literal Typing

- Integer literals are initially untyped and MUST be resolved by context.
- Boolean literals have type `bool`.
- String literals are opaque payloads where permitted.

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
```

A shape:
- defines a nominal type,
- consists of named fields with explicit types,
- has no behavior.

Field names MUST be unique.

---

### 5.4.2 Shape Usage

Shape types may be used:
- as process parameters,
- as return types,
- in expressions via field access.

```ds
let m = Measurement { value: 3.14, valid: true };
let v = m.value;
```

---

## 5.5 Type References

A type reference is an identifier that MUST resolve to:
- a primitive type, or
- a shape defined in scope.

Unresolved type identifiers are static errors.

---

## 5.6 Type Checking Rules

### 5.6.1 Let Bindings

For:

```ds
let x = expression;
```

The type of `x` is the inferred type of `expression`.

Rebinding within the same scope is forbidden.

---

### 5.6.2 Expressions

- Arithmetic operators require numeric operands.
- Comparison operators produce `bool`.
- Logical operators require `bool` operands.

All operand types MUST be compatible.

---

### 5.6.3 Field Access

```ds
expr.field
```

Valid only if:
- `expr` has a shape type,
- `field` exists in that shape.

---

### 5.6.4 Process Calls

A call expression is valid only if:
- argument types match parameter types,
- regime and linearity rules are satisfied.

Return type mismatches are static errors.

---

## 5.7 Return Types

If a process declares a return type:

```ds
proc K::compute() -> i32 { ... }
```

All execution paths MUST return a value of that type.

If no return type is declared:
- the process returns no value,
- `return` MUST NOT include an expression.

---

## 5.8 Regime Interaction and Types

Type correctness alone is insufficient.

In addition:
- **K-regime** allows unrestricted reuse of values.
- **Q-regime** enforces linear usage.
- **Φ-regime** treats values symbolically for admissibility.

Programs MUST satisfy both type and regime rules.

---

## 5.9 Type Errors

Type errors include:
- undefined types,
- incompatible operands,
- invalid field access,
- argument or return mismatches.

Type errors MUST be detected statically.

---

## 5.10 Future Extensions

Future versions may introduce:
- generic types,
- sum and option types,
- explicit string types,
- dependent typing elements.

These are intentionally excluded from v0.1.

---

End of Section 5.