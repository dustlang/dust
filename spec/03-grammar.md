# 3. Grammar

This section defines the **formal grammar** of DPL v0.1. The grammar is presented in an extended BNF-like notation. It specifies **syntax only**; semantic constraints are defined in later sections.

Unless otherwise stated:
- Terminals appear in `monospace`.
- Non-terminals appear in *italics*.
- `?` denotes optionality.
- `*` denotes zero or more repetitions.
- `+` denotes one or more repetitions.
- Alternatives are separated by `|`.

---

## 3.1 Program Structure

```ebnf
program        ::= forge+

A DPL program consists of one or more forges.

⸻

3.2 Forges

forge          ::= "forge" identifier "{" forge_item* "}"

A forge is a semantic namespace containing shapes, processes, and bindings.

⸻

3.3 Forge Items

forge_item     ::= shape_decl
                 | proc_decl
                 | bind_decl


⸻

3.4 Shapes

shape_decl     ::= "shape" identifier "{" field_decl* "}"

field_decl     ::= identifier ":" type_ref ";"

Shapes define structured data layouts. Field ordering is not semantically significant.

⸻

3.5 Processes

3.5.1 Process Declaration

proc_decl      ::= "proc" proc_path
                   "(" param_list? ")"
                   uses_clause*
                   return_type?
                   proc_qualifier*
                   block


⸻

3.5.2 Process Path and Regime

proc_path      ::= regime "::" identifier

regime         ::= "K" | "Q" | "Φ"

Every process MUST declare exactly one regime.

⸻

3.5.3 Parameters

param_list     ::= param ("," param)*

param          ::= identifier ":" type_ref


⸻

3.5.4 Uses Clauses

uses_clause    ::= "uses" identifier "(" named_arg_list? ")"

named_arg_list ::= named_arg ("," named_arg)*

named_arg      ::= identifier "=" literal

A process MAY declare zero or more uses clauses.

⸻

3.5.5 Return Type

return_type    ::= "->" type_ref

If omitted, the process returns no value.

⸻

3.5.6 Process Qualifiers

proc_qualifier ::= "linear"

Additional qualifiers MAY be introduced in future versions.

⸻

3.6 Bindings and Contracts

3.6.1 Binding Declaration

bind_decl      ::= "bind" proc_ref "->" proc_ref contract_block


⸻

3.6.2 Process Reference

proc_ref       ::= proc_path
                 | identifier

Unqualified identifiers are resolved according to forge scope rules.

⸻

3.6.3 Contract Block

contract_block ::= "contract" "{" contract_clause* "}"

contract_clause ::= identifier contract_op contract_value ";"

contract_op    ::= "==" | "<" | "<=" | ">" | ">="

contract_value ::= identifier | literal

Contracts constrain cross-process bindings. Semantic interpretation is defined in Section 10.

⸻

3.7 Blocks and Statements

block          ::= "{" statement* "}"


⸻

3.7.1 Statements

statement      ::= let_stmt
                 | constrain_stmt
                 | prove_stmt
                 | effect_stmt
                 | return_stmt


⸻

3.7.2 Let Statement

let_stmt       ::= "let" identifier "=" expression ";"


⸻

3.7.3 Constrain Statement

constrain_stmt ::= "constrain" expression ";"


⸻

3.7.4 Prove Statement

prove_stmt     ::= "prove" identifier "from" expression ";"


⸻

3.7.5 Effect Statements

effect_stmt    ::= effect_kind expression ";"

effect_kind    ::= "observe"
                 | "emit"
                 | "seal"

Effect legality depends on the enclosing regime.

⸻

3.7.6 Return Statement

return_stmt    ::= "return" expression ";"


⸻

3.8 Expressions

expression     ::= logical_or

logical_or     ::= logical_and ("||" logical_and)*

logical_and    ::= equality ("&&" equality)*

equality       ::= comparison (("==" ) comparison)*

comparison     ::= additive (("<" | "<=" | ">" | ">=") additive)*

additive       ::= multiplicative (("+" | "-") multiplicative)*

multiplicative ::= postfix (("*" | "/") postfix)*

postfix        ::= primary postfix_op*

postfix_op     ::= "." identifier
                 | "(" argument_list? ")"
                 | struct_literal

primary        ::= literal
                 | identifier
                 | "(" expression ")"


⸻

3.8.1 Argument List

argument_list  ::= expression ("," expression)*


⸻

3.8.2 Struct Literals

struct_literal ::= "{" field_init_list? "}"

field_init_list ::= field_init ("," field_init)*

field_init     ::= identifier ":" expression

Struct literal syntax is resolved contextually by the type system.

⸻

3.9 Types

type_ref       ::= identifier

Primitive and composite types are defined semantically in Section 5.

⸻

3.10 Grammar Notes
	•	This grammar is intentionally conservative.
	•	Ambiguities are resolved by precedence rules encoded above.
	•	Future versions may extend this grammar, but v0.1 programs MUST conform exactly.

⸻
