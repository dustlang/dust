# Frontend: Lexer and Parser

## AST Layer

Defined in `crates/dust_frontend/src/ast.rs`.

Core concepts:

- byte spans (`Span`, `Spanned<T>`)
- forges and items (`Shape`, `Proc`, `Bind`, `Const`)
- statement and expression trees
- regimes (`K`, `Q`, `Phi`)

## Lexer

Defined in `crates/dust_frontend/src/lexer.rs`.

Token classes include:

- identifiers and keywords
- integer/float/char/string/bool literals
- punctuation/operators (`::`, `->`, `==`, `<=`, `>=`, `&&`, `||`, `..`, shifts, etc.)

Comment support:

- line: `// ...`
- block: `/* ... */` (non-nesting)

Error kinds:

- unexpected char
- unterminated string/block comment
- invalid escape
- integer overflow

## Parser

Defined in `crates/dust_frontend/src/parser.rs`.

Notable behavior:

- supports `forge` blocks and top-level shorthand procs (`K main { ... }`)
- parses `proc`, `const`, `shape`
- parses control flow (`if`, `for`, `while`, `break`, `continue`)
- parses unary/binary expressions, calls, field/index access, arrays
- returns `ParseError { message, span }`

Operational note:

- binary parsing is simple iterative chaining and does not implement full precedence stratification for all operators.
