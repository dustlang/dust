# 11. Error & Failure Model

## 11.1 Overview

DPL distinguishes sharply between **errors** and **failures**.

- **Errors** are violations of the language rules.
- **Failures** are semantic outcomes of admissible programs.

This distinction is fundamental. Errors indicate that a program is **invalid**. Failures indicate that a valid program **cannot proceed** under given conditions.

A DPL implementation MUST report errors and failures deterministically and unambiguously.

---

## 11.2 Error Categories

Errors are classified as follows:

### 11.2.1 Lexical Errors

Lexical errors occur during tokenization, including:
- invalid characters,
- unterminated comments,
- unterminated string literals,
- invalid escape sequences.

Lexical errors MUST prevent parsing.

---

### 11.2.2 Syntax Errors

Syntax errors occur when token sequences do not conform to the grammar.

Examples include:
- missing delimiters,
- malformed declarations,
- invalid statement structure.

Syntax errors MUST prevent semantic analysis.

---

### 11.2.3 Type Errors

Type errors occur when:
- types are undefined,
- operand types are incompatible,
- field access is invalid,
- argument or return types mismatch.

Type errors MUST be detected statically and MUST prevent successful compilation.

---

### 11.2.4 Regime Errors

Regime errors occur when a program violates regime rules, including:
- illegal effects in Q or Φ regimes,
- linearity violations in Q-regime,
- forbidden cross-regime interaction without binding.

Regime errors MUST be reported as static semantic errors.

---

### 11.2.5 Binding Errors

Binding errors occur when:
- bindings reference unresolved processes,
- contract clauses are malformed,
- bindings violate regime boundaries.

Binding errors MUST prevent successful compilation.

---

## 11.3 Failures

Failures occur in **valid programs** whose semantic conditions cannot be satisfied.

Failures include:
- unsatisfied constraints,
- inadmissible bindings,
- effect preconditions not met,
- physical-time constraints violated.

Failures are deterministic outcomes of program semantics.

---

## 11.4 Static vs Semantic Detection

A DPL implementation MUST:

- detect errors statically where possible,
- detect failures semantically when static detection is not possible.

A program that is known to fail MUST NOT be executed.

---

## 11.5 Failure Propagation

When a failure occurs:

- execution or evaluation MUST halt,
- no further effects may occur,
- partial success is forbidden.

Failure propagates outward through process invocation and binding.

---

## 11.6 Diagnostics and Reporting

For both errors and failures, implementations MUST report:

- the category (error or failure),
- a clear description,
- the source location (file, line, column or span),
- the violated rule or constraint when applicable.

Diagnostics MUST be stable and reproducible.

---

## 11.7 Errors and DIR

Programs containing errors MUST NOT produce DIR.

Programs that are valid but fail semantically MAY produce DIR annotated with failure conditions.

DIR MUST preserve enough information to explain failures.

---

## 11.8 Non-Goals of v0.1

DPL v0.1 does not define:
- recovery from errors,
- exception handling,
- resumable or partial execution.

These are reserved for future versions.

---

End of Section 11.