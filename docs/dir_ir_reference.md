# DIR IR Reference

Source: `crates/dust_dir/src/lib.rs`

## Top-level

```text
DirProgram {
  forges: Vec<DirForge>,
  types: Vec<DirTypeDef>
}
```

## Forge and Procedure Model

- `DirForge`: named container of shapes, procs, binds
- `DirProc`:
  - `regime`
  - `name`
  - parameters, uses clauses, return type, qualifiers
  - `body: Vec<DirStmt>`
  - `locals: Vec<DirLocal>`

## Statement Variants

`DirStmt` includes:

- basic: `Let`, `Assign`, `Constrain`, `Prove`, `Effect`, `Return`
- control flow: `If`, `While`, `For`, `Break`, `Continue`
- calls: `Call`
- memory: `Alloc`, `Dealloc`, `Load`, `Store`
- access: `Index`, `Field`

## Serialization

All DIR types derive serde Serialize/Deserialize and are used for:

- `dust dir` output files
- internal codegen input contracts

## Practical Note

Current lowering and codegen do not yet use every `DirStmt` variant uniformly across all build modes.
