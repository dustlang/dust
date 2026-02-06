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