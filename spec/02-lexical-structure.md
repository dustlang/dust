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

### 2.2.1 Line Comments

A line comment begins with `//` and continues to the end of the line.

```ds
// this is a comment
let x = 1;
```

### 2.2.2 Block Comments

A block comment begins with `/*` and ends at the first subsequent `*/`.

```ds
/*
  this is a block comment
*/
proc K::f() { return 0; }
```

Block comments MUST NOT nest.

If the end delimiter `*/` is not encountered before end-of-file, the implementation MUST report a lexical error.

---

## 2.3 Tokens

A DPL implementation MUST tokenize the source into a sequence of tokens. Tokens belong to the following categories:

- identifiers
- keywords
- literals
- operators and punctuation
- delimiters

Tokens MUST be recognized using a maximal-munch (longest match) rule.

---

## 2.4 Identifiers

Identifiers name forges, shapes, fields, processes, parameters, bindings, witnesses, and local variables.

### 2.4.1 Identifier Character Set

An identifier MUST begin with:
- ASCII letter (`A`–`Z`, `a`–`z`) or underscore (`_`)

Subsequent characters MAY be:
- ASCII letter
- ASCII digit (`0`–`9`)
- underscore (`_`)

Examples of valid identifiers:

```ds
forge Examples { }
shape QState { id: i64; }
proc K::hello_world() { return 0; }
let job_id = 1001;
```

Examples of invalid identifiers:

- `123abc`
- `let`
- `proc`

---

## 2.5 Keywords

The following keywords are reserved in DPL v0.1 and MUST NOT be used as identifiers:

- `forge`
- `shape`
- `proc`
- `bind`
- `contract`
- `uses`
- `let`
- `constrain`
- `prove`
- `from`
- `observe`
- `emit`
- `seal`
- `return`
- `linear`

The following regime markers are reserved:

- `K`
- `Q`
- `Φ`

---

## 2.6 Literals

### 2.6.1 Integer Literals

Integer literals are base-10 sequences of digits:

```ds
0
7
404
1001
5000
```

An implementation MUST reject an integer literal that cannot be represented in the selected integer range.

### 2.6.2 Boolean Literals

Boolean literals are:

- `true`
- `false`

### 2.6.3 String Literals

String literals are delimited by double quotes (`"`).

```ds
"Hello, World"
"AUDIT:BEGIN"
"Φ-VALUE"
```

Supported escape sequences:

- `\"`
- `\\`
- `\n`
- `\r`
- `\t`

An unterminated string literal is a lexical error.

---

## 2.7 Operators and Punctuation

### 2.7.1 Structural Tokens

- `::`
- `->`
- `:`
- `=`
- `.`

### 2.7.2 Delimiters

- `{` `}`
- `(` `)`
- `[` `]`
- `,`
- `;`

### 2.7.3 Comparison Operators

- `==`
- `<`
- `<=`
- `>`
- `>=`

### 2.7.4 Boolean Operators

- `&&`
- `||`

### 2.7.5 Arithmetic Operators

- `+`
- `-`
- `*`
- `/`

---

## 2.8 Newlines and Statement Termination

Statements are terminated by semicolons (`;`).

Newlines are treated as whitespace and have no syntactic significance.

---

## 2.9 Lexical Errors

Lexical errors include, but are not limited to:

- invalid characters,
- unterminated comments,
- unterminated string literals,
- invalid escape sequences.

A lexical error MUST:
- be reported deterministically,
- include the source span of the error,
- prevent successful parsing.