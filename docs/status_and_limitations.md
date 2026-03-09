# Status and Limitations

## Spec and Implementation State

- The normative language specification under `dust/spec` is currently organized around v0.1 documents.
- Repository narratives and helper modules include v0.2-oriented work and expansion paths.

## Current Effective Constraints

- many default compile paths still rely on emit/call subset extraction
- host executable expression/statement lowering is still partial for full-feature CLI tools
- full coverage of all parser/AST constructs in codegen is not yet end-to-end
- Q and Phi are not first-class executable codegen targets in standard paths

## Recent Progress

- host executable link path now prefers `dustlink` with explicit `lld` fallback order
- bootstrap guard for building `dustlink` avoids recursive self-link
- kernel workflows are documented around `obj` + `dustlink` with `kernel-link` as compatibility mode
- host runtime shim grew linker-oriented intrinsics used by Dust-built `dustlink`
- host runtime shim shared-symbol ingestion now includes ELF, PE, COFF, and Mach-O metadata paths
- host runtime shim accepts both x86_64 and arm64 machine/cpu IDs in COFF/Mach-O object-format probe and symbol-ingest paths
- linker-script application now uses block-aware statement splitting and additional `SECTIONS`/`ENTRY(symbol)` semantics used by dustlink parity work
- linker-script runtime semantics now include sysroot-aware `SEARCH_DIR(=...)` handling and `INPUT` support for `-L` search-path tokens / `-l` needed-library token capture
- linker-script runtime semantics now include expression evaluation (`ORIGIN/LENGTH/ADDR/LOADADDR/SIZEOF/ALIGN` + arithmetic), `ASSERT(...)` checks, and `PHDRS`/`VERSION` compatibility blocks with structural validation
- compatibility CLI controls in host linker runtime now carry explicit state for hash-style, thread count, eh-frame-header, diagnostics toggles, print-gc toggles, and icf mode
- host linker CLI compatibility coverage now includes script/export families (`--version-script`, `--dynamic-list`, `--trace-symbol`) plus broader no-op compatibility flags (`--print-map`, `--start-lib`, `--end-lib`, `--emit-relocs`, `--strip-all`)
- host linker CLI compatibility coverage now also includes major `lld-link` slash-option families (`/OUT`, `/ENTRY`, `/MACHINE`, `/LIBPATH`, `/DEFAULTLIB`, `/MAP`, `/DLL`, `/SUBSYSTEM`, `/OPT`, `/WX`) plus common slash metadata compatibility paths
- host linker target modeling now preserves architecture-specific targets (`x86_64` vs `aarch64`) instead of collapsing all aliases into x86_64-only families
- host linker relocation ingest/application now branches on per-object machine type, enabling machine-aware relocation validation and patch rules
- host linker relocation baseline now includes AArch64 IDs (`R_AARCH64_NONE`, `R_AARCH64_ABS64`, `R_AARCH64_ABS32`, `R_AARCH64_PREL32`)
- host linker relocation AArch64 coverage now includes instruction-form patching and validation for:
  - `CALL26` / `JUMP26`
  - `CONDBR19` / `TSTBR14`
  - `LD_PREL_LO19`, `ADR_PREL_LO21`, `ADR_PREL_PG_HI21(_NC)`
  - `ADD_ABS_LO12_NC` and `LDST*_ABS_LO12_NC` (including `LDST128`)
  - MOVW `UABS` / `SABS` / `PREL` families
  - starter TLS instruction/data relocations for `TLSGD` / `TLSLD` / `TLSDESC` plus `TLS_DTPMOD` / `TLS_DTPREL` / `TLS_TPREL`
- host linker runtime now computes AArch64 TLS data relocation values (`TLS_DTPMOD`, `TLS_DTPREL`, `TLS_TPREL`) for non-shared links using a deterministic synthesized TLS layout derived from object TLS sections
- host linker runtime now partially differentiates shared-link AArch64 TLS data relocations: `TLS_DTPREL` resolves from TLS layout metadata, shared-link `TLS_TPREL` is rejected as invalid, and shared-link `TLS_DTPMOD` remains deferred
- Dust linker relocation apply path now also supports AArch64 TLSLE/TLSLD low12 offset instruction relocations (`ADD`/`LDST64`/`LDST128`) in non-shared links by reusing host-runtime TLS offset helpers
- Dust linker relocation apply path now permits `R_AARCH64_TLSDESC_CALL` as a validated `BLR` preserve relocation and routes remaining `TLSGD`/`TLSLD`/`TLSDESC` descriptor-sequence instruction relocs through the host synthetic-slot reloc-value helper (staged semantics)
- host runtime now includes AArch64 TLS synthetic descriptor/GOT planning state plus helper ABI (`reserve/count/slot-address/reloc-value`) and materializes a synthetic TLS descriptor/GOT-like slot region in ELF load images for reserved descriptor-sequence slots
- host ELF writer now emits minimal synthetic dynamic relocation metadata (`DT_SYMTAB`, `DT_SYMENT`, `DT_RELA*`, synthetic `.rela.dyn` records) for reserved AArch64 TLS descriptor-sequence slots
- initial TLS descriptor-sequence relaxation scope is limited to deterministic synthetic-slot reuse/coalescing (including TLSLD module-slot coalescing); full instruction-sequence rewrite relaxations remain pending
- host linker output writers now emit architecture-correct machine/cpu identifiers for ELF/PE/Mach-O outputs
- host linker PE output now applies `/NOENTRY`, `/DYNAMICBASE`, `/NXCOMPAT`, and `/LARGEADDRESSAWARE` to emitted header fields (entrypoint/characteristics)
- host linker compatibility handling now includes broader soft-compatibility ld/lld/lld-link families (`--warn-*`, `--time-trace*`, `--lto-*`, `/GUARD:*`, `/TIMESTAMP:*`, `/MERGE:*`, `/SECTION:*`)
- host linker dynamic-policy coverage now includes `--no-allow-shlib-undefined` and split-form parsing for `--dynamic-linker` / `--soname`
- host linker `-z` coverage now includes `defs`/`undefs` unresolved-policy toggles and accepted compatibility tokens `text`/`notext`/`origin`
- additional `lld-link` no-value compatibility forms are accepted (`/NOENTRY`, `/DYNAMICBASE`, `/NXCOMPAT`, `/LARGEADDRESSAWARE`)
- ELF write path no longer primes output with ident-only stubs during staged emission; header/finalize stages both execute complete ELF writer flow with section-index validation on staged section calls
- PE/Mach-O host writers now emit sectionized images from alloc chunks instead of a single synthetic text payload section
- linker-script semantics are stricter and broader: unknown directive heads are rejected, direct symbol assignments are supported, and expression evaluation now covers unary/mul/div/mod/shift/bitwise operators
- host linker CLI compatibility no-op paths now emit diagnostics (and can hard-fail under `--fatal-warnings`) instead of silently consuming all compatibility flags
- host linker target alias coverage expanded for musl triples, Windows GNU triples, and `*-none[-elf]` bare-metal aliases used by Dust-built tooling
- host runtime shim now consumes previously parse-only compatibility state for:
  - `--hash-style` (ELF hash-tag emission)
  - `--dependency-file` (depfile output)
  - `--emit-relocs` (map-row relocation reporting)
  - `--print-gc-sections` (GC drop diagnostics)
- host object ingestion now uses refined machine-aware COFF and Mach-O relocation-kind mapping during relocation record ingestion
- host shared-object symbol ingest now enforces target/ABI/file-kind checks before symbol ingestion (`ELF ET_DYN` + target machine, Windows PE DLL/COFF machine, Mach-O dylib target CPU)
- host shared-object symbol ingest now filters ELF hidden/internal dynsyms and Mach-O private extern/debug symbols from shared export fallback ingestion
- host needed-library recording now prefers embedded shared-library names where available (`DT_SONAME`, PE export DLL name, Mach-O `LC_ID_DYLIB` install name) over filename-only normalization
- full AArch64 ELF TLS descriptor / TLS instruction-family semantics are not complete yet (current coverage includes ingest/validation, bitfield patching for non-TLS instruction forms, and host-backed TLS data reloc math, but not exhaustive TLSDESC/GOT/TLS relaxation behavior parity)
- AArch64 TLS instruction-family coverage is still partial: descriptor-sequence instruction relocs now patch against synthetic slot addresses, but full TLSDESC/TLSGD/TLSLD runtime semantics, symbol-aware dynamic relocation binding, and instruction rewrite relaxations remain incomplete
- AArch64 TLS descriptor-sequence parity is no longer blocked on planning-only state: synthetic slot-region materialization and minimal `.rela.dyn` metadata emission are wired, but full TLSDESC/GOT runtime semantics and relaxations are still pending
- shared-object handling remains primarily symbol-ingest oriented rather than full dynamic-linker semantic parity (though unknown shared-object formats now fail instead of silently succeeding)
- shared-object handling remains primarily symbol-ingest oriented rather than full dynamic-linker semantic parity (now with stricter target/ABI/file-kind rejection before ingest)
- shared-object handling remains primarily symbol-ingest oriented rather than full dynamic-linker semantic parity (now with stricter target/ABI/file-kind checks and tighter exported-symbol filtering)

## Important Mismatches to Track

- DIR model includes richer statements than default lowering/codegen paths fully consume
- `type_system.rs` provides a larger type-checking framework than the minimal checks used in `parse_and_check`
- `v02_codegen.rs` contains additional routines but is not wired through `lib.rs`

## Operational Guidance

Treat current compiler behavior as a staged implementation:

1. verify syntax and minimal semantics with `check`
2. inspect lowered form via `dir`
3. use `obj` + `dustlink` for multi-object kernel workflows
4. validate unsupported constructs early using targeted fixtures
