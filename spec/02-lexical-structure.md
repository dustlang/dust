<!-- spec/02-lexical-structure.md -->

# 2. Lexical Structure

## 2.1 Source Files and Encoding

A DPL source file MUST be a UTF-8 encoded text file.

- The default file extension for DPL source programs is **`.ds`**.
- A DPL implementation MUST accept UTF-8 input and MUST preserve byte-accurate spans for diagnostics.

Whitespace consists of:
- space (`U+0020`)
- horizontal tab (`U+0009`)
- carriage return (`U+000D`)
- line feed (`U+000A`)

Whitespace is semantically insignificant except where it separates tokens.

---

## 2.2 Comments

Comments are non-semantic and are treated as whitespace.

DPL supports:

### 2.2.1 Line Comments

A line comment begins with `//` and continues to the end of the line.

```ds
// this is a comment
let x = 1;

2.2.2 Block Comments

A block comment begins with /* and ends at the first subsequent */.

/*
  this is a block comment
*/
proc K::f() { return 0; }

Block comments MUST NOT nest.

If the end delimiter */ is not encountered before end-of-file, the implementation MUST report a lexical error.

⸻

2.3 Tokens

A DPL implementation MUST tokenize the source into a sequence of tokens. Tokens belong to the following categories:
	•	identifiers
	•	keywords
	•	literals
	•	operators and punctuation
	•	delimiters

Tokens MUST be recognized using a maximal-munch (longest match) rule.

⸻

2.4 Identifiers

Identifiers name forges, shapes, fields, processes, parameters, bindings, witnesses, and local variables.

2.4.1 Identifier Character Set

An identifier MUST begin with:
	•	ASCII letter (A–Z, a–z) or underscore (_)

Subsequent characters MAY be:
	•	ASCII letter
	•	ASCII digit (0–9)
	•	underscore (_)

Examples of valid identifiers:

forge Examples { }
shape QState { id: i64; }
proc K::hello_world() { return 0; }
let job_id = 1001;

Examples of invalid identifiers (lexically invalid or reserved):
	•	123abc (begins with digit)
	•	let (keyword)
	•	proc (keyword)

⸻

2.5 Keywords

The following keywords are reserved in DPL v0.1 and MUST NOT be used as identifiers:
	•	forge
	•	shape
	•	proc
	•	bind
	•	contract
	•	uses
	•	let
	•	constrain
	•	prove
	•	from
	•	observe
	•	emit
	•	seal
	•	return
	•	linear

The regime symbols are reserved and MUST be recognized as regime markers:
	•	K
	•	Q
	•	Φ

⸻

2.6 Literals

2.6.1 Integer Literals

Integer literals are base-10 sequences of digits:

0
7
404
1001
5000

An implementation MUST reject an integer literal that cannot be represented in the implementation’s selected integer literal range.

The semantic interpretation of integer literals with respect to specific integer types (e.g. i32, i64) is defined in Section 5.

2.6.2 Boolean Literals

Boolean literals are:
	•	true
	•	false

2.6.3 String Literals

String literals are delimited by double quotes (").

"Hello, World"
"AUDIT:BEGIN"
"Φ-VALUE"

A string literal MAY contain the following escape sequences:
	•	\" (double quote)
	•	\\ (backslash)
	•	\n (line feed)
	•	\r (carriage return)
	•	\t (horizontal tab)

If a string literal is not terminated before end-of-file, the implementation MUST report a lexical error.

v0.1 Note: Strings are admitted for tooling, diagnostics, and explicit emit payloads.
Their deeper semantic role is defined by later sections.

⸻

2.7 Operators and Punctuation

The following operators and punctuation tokens are defined in DPL v0.1:

2.7.1 Structural Tokens
	•	:: (regime qualification and path separation)
	•	-> (binding arrow)
	•	: (type ascription / field delimiter)
	•	= (assignment)
	•	. (field access)

2.7.2 Delimiters
	•	{ } (blocks and forge bodies)
	•	( ) (parameter lists and call arguments)
	•	[ ] (reserved for future use; may be used in later versions)
	•	, (separator)
	•	; (statement terminator)

2.7.3 Comparison Operators
	•	==
	•	<
	•	<=
	•	>
	•	>=

2.7.4 Boolean Operators
	•	&&
	•	||

2.7.5 Arithmetic Operators
	•	+
	•	-
	•	*
	•	/

⸻

2.8 Newlines and Statement Termination

Statements are terminated by semicolons (;) in DPL v0.1.

Newlines are not statement terminators and do not affect parsing except as whitespace.

⸻

2.9 Errors

Lexical errors MUST be reported deterministically and MUST include:
	•	the byte span of the invalid region (start and end indices)
	•	an error category (e.g., unterminated string, invalid escape, unexpected character)

A lexical error MUST prevent successful parsing of the program.

⸻
