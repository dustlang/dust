# 16. Glossary

This glossary defines **normative terms** used throughout the DPL v0.1 specification.  
Capitalized terms have the meanings defined here.

---

## Admissibility

The property of a process or binding being **semantically valid** under all declared constraints, regime rules, and contracts.  
A process that is not admissible MUST NOT execute.

---

## Binding

A declarative connection between two processes that permits controlled information flow across regime boundaries, governed by a contract.

---

## Constraint

A declarative predicate introduced with `constrain` that restricts admissible configurations.  
Constraints do not execute and have no side effects.

---

## Contract

A set of declarative clauses associated with a binding that governs admissibility, permissions, and limits of cross-process interaction.

---

## DIR (Dust Intermediate Representation)

The canonical, deterministic semantic representation of a DPL program emitted by a conformant compiler.

---

## Effect

An explicit, irreversible interaction with the external world or semantic state, introduced using `emit`, `observe`, or `seal`.

---

## Failure

A deterministic semantic outcome in which a valid program cannot proceed due to unsatisfied constraints, inadmissible bindings, or violated preconditions.

---

## Forge

A semantic namespace that contains shapes, processes, and bindings.

---

## K-Regime

The **Classical / Deterministic** regime used for orchestration, control flow, and external interaction.

---

## Linearity

The property requiring a value to be used exactly once, enforced structurally in Q-regime processes.

---

## Process

The fundamental unit of behavior in DPL, declared with a regime, parameters, optional return type, and a body.

---

## Q-Regime

The **Quantum / Linear** regime used for non-duplicable, irreversible computation with enforced linearity.

---

## Regime

A semantic execution domain (`K`, `Q`, or `Φ`) that defines permitted operations, effects, and failure modes.

---

## Semantic Failure

See **Failure**.

---

## Shape

A named, structured type consisting of explicitly typed fields.

---

## Toolchain

The set of tools (including the `dust` compiler) that parse, validate, and process DPL programs.

---

## Type System

The static system that assigns meaning to values and expressions, ensuring structural correctness across regimes.

---

## Witness

A value constructed using `prove` that serves as evidence of admissibility.

---

## Φ-Regime

The **Phase / Admissibility** regime used to express constraints and semantic validity without execution.

---

End of Section 16.