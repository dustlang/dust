# 1. Introduction

## 1.1 Purpose

The Dust Programming Language (DPL) is a systems-level language designed to express **meaning, admissibility, and execution** as first-class concepts. Unlike conventional languages that treat execution as primary and meaning as implicit, DPL makes semantic intent explicit and verifiable.

DPL is intended for domains where:
- correctness is more important than convenience,
- effects must be explicitly controlled,
- irreversibility must be reasoned about,
- multiple computational paradigms must coexist.

These include, but are not limited to, physics, advanced engineering, cryptography, governance systems, and safety-critical computation.

---

## 1.2 Core Philosophy

DPL is built on three foundational principles:

1. **Meaning precedes execution**  
   A DPL program describes what is admissible before describing what is executed. Execution is permitted only within admissible semantic space.

2. **Effects are explicit**  
   All observable interactions with the external world are declared, ordered, and constrained. No hidden effects are allowed.

3. **Irreversibility is enforced by structure**  
   Where information is consumed, destroyed, or observed, the language structure prevents illegal reuse or rollback.

These principles are enforced not by convention, but by the language itself.

---

## 1.3 The Three-Regime Model

DPL unifies three computational regimes within a single language:

- **K-regime (Classical / Deterministic)**  
  Used for conventional computation, orchestration, control flow, and interaction with the environment.

- **Q-regime (Quantum / Linear)**  
  Used for linear, non-duplicable computation where consumption and irreversibility are intrinsic.

- **Φ-regime (Phase / Admissibility)**  
  Used to express constraints, admissibility conditions, and semantic validity without performing execution.

Each regime has its own rules, guarantees, and failure modes. Cross-regime interaction is explicit and governed by binding contracts.

---

## 1.4 Programs, Forges, and Processes

A DPL program is composed of **forges**, which serve as semantic namespaces and organizational units.

Within a forge, a programmer may define:

- **Shapes** — structured data definitions
- **Processes** — executable or admissibility-evaluated units
- **Bindings** — contracts governing interactions between processes

Processes are always declared with an explicit regime (`K`, `Q`, or `Φ`), and their behavior is constrained by that regime’s semantics.

---

## 1.5 Admissibility and Failure

In DPL, failure is not exceptional; it is informative.

A program may fail because:
- a constraint is unsatisfiable,
- an effect violates ordering or irreversibility rules,
- a binding contract is not met,
- a linear resource is misused.

Such failures are semantic failures, not runtime accidents. The language and compiler are required to detect and report them as early as possible.

---

## 1.6 The Dust Toolchain

The reference toolchain for DPL is the **`dust`** compiler.

At minimum, `dust` is responsible for:
- parsing `.ds` source files,
- validating them against the DPL specification,
- producing the **Dust Intermediate Representation (DIR)**.

DIR is the canonical semantic form of a DPL program. All execution, code generation, or analysis is downstream of DIR.

---

## 1.7 Scope and Non-Goals of v0.1

DPL v0.1 establishes the semantic foundation of the language. It is intentionally conservative.

v0.1 does define:
- syntax and grammar,
- regime semantics,
- effects, constraints, and admissibility,
- binding and contract structure,
- DIR as a canonical representation.

v0.1 does not require:
- native code generation,
- performance guarantees,
- a runtime environment beyond semantic validation.

These may be introduced in future versions.

---

## 1.8 Reading the Specification

This specification is divided into numbered sections, each in its own file. Sections should be read in order.

Terms defined in later sections are capitalized when first used and are fully defined in the Glossary.

Normative language such as “MUST”, “SHALL”, and “MUST NOT” is used intentionally and should be interpreted strictly.