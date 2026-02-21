# Dust - DPL Compiler and Toolchain

`dust` is the compiler and toolchain for the **Dust Programming Language (DPL)**.

This repository contains:
- the DPL specification (`spec/`)
- reference examples (`examples/`)
- the `dust` compiler implementation (Rust)
- `dust-runtime` - OS services for bare-metal kernels

---

## Status

- **DPL Specification:** v0.2 (**active development**)
- **Compiler:** v0.2 (**bare-metal and extended feature support**)

### Linker Policy

- Host executable builds prefer `dustlink` as the linker frontend.
- To bootstrap `dustlink` itself, the compiler bypasses `dustlink` and falls back to `lld` paths (`-fuse-ld=lld`, `rust-lld`, `ld.lld`) before the driver default linker.
- `dust kernel-link` remains available but is deprecated; use `dust obj` + `dustlink`.
- Host linker runtime support used by Dust-built tools now includes:
  - cross-format shared-symbol ingestion (ELF/PE/COFF/Mach-O paths)
  - COFF/Mach-O object-format probe and symbol-ingest acceptance for `arm64` machine/cpu IDs in addition to `x86_64`
  - block-aware linker-script statement splitting
  - section output-address parsing inside `SECTIONS { ... }`
  - script expression evaluation (`ORIGIN/LENGTH/ADDR/LOADADDR/SIZEOF/ALIGN` + `+/-`) and `ASSERT(...)` checks
  - `PHDRS` / `VERSION` script compatibility blocks with block-shape validation
  - `SECTIONS ... AT(<expr>)` load-address capture
  - required-symbol registration for `ENTRY(symbol)` script form
  - sysroot-aware `SEARCH_DIR(=...)` linker-script resolution and script `INPUT` handling for `-L` / `-l` tokens
  - compatibility-flag state wiring for hash-style/threading/diagnostic/icf controls
  - broader CLI compatibility handling for script/export-related ld/lld flags (`--version-script`, `--dynamic-list`, `--trace-symbol`, `--print-map`, `--start-lib`, `--end-lib`)
  - `lld-link` slash-option compatibility handling (`/OUT`, `/ENTRY`, `/MACHINE`, `/LIBPATH`, `/DEFAULTLIB`, `/MAP`, `/DLL`, `/SUBSYSTEM`, `/OPT`, `/WX`)
  - additional target alias parsing for `aarch64`/`arm64` triples (mapped to platform linker target families)
  - complete ELF writer execution in staged header/finalize flow (no ident-only prewrite)
  - sectionized PE/Mach-O host writer emission from real alloc chunks

### What Works Today (v0.2)

The compiler supports multiple output modes:

#### 1. Native Executables (Host)
```bash
dust build hello.ds
./target/dust/hello
```

#### 2. Object Files (for linking)
```bash
dust obj hello.ds -o hello.o
```

#### 3. Cross-Compilation
```bash
dust obj hello.ds --target x86_64-unknown-linux-gnu
```

#### 4. Bare-Metal Kernels (no libc)
```bash
dust obj hello.ds --bare-metal -o kernel.bin
```

---

## DPL v0.2 Specification

### Supported Features

#### K-Regime (Classical Computing)
- **Entry Point:** `K main { ... }`
- **Variable Declarations:** `let x = 5;`
- **Control Flow:**
  - `if condition { ... } else { ... }`
  - `while condition { ... }`
  - `for i in 0..10 { ... }`
- **Effects:** `emit "string"` for output
- **Arithmetic:** `+`, `-`, `*`, `/`, `%`
- **Comparisons:** `==`, `!=`, `<`, `<=`, `>`, `>=`

#### Q-Regime (Quantum Simulation)
- Returns `ERR_DOMAIN_NOT_AVAILABLE` (100) - stub implementation

#### Φ-Regime (Future Computing)
- Returns `ERR_DOMAIN_NOT_AVAILABLE` (100) - stub implementation

### Data Types
- `UInt8`, `UInt16`, `UInt32`, `UInt64`
- `Int8`, `Int16`, `Int32`, `Int64`
- `Float32`, `Float64`
- `Bool`
- `Char`

### Code Generation
- Cranelift-based JIT/AOT compilation
- ELF (Linux), PE (Windows), Mach-O (macOS) executables
- Flat binary for bare-metal
- Object files (.o) for linking with dustlink

---

## Supported Targets

| Target | Description |
|--------|-------------|
| (host) | Native (default) |
| x86_64-unknown-linux-gnu | Linux |
| x86_64-pc-windows-gnu | Windows |
| x86_64-apple-darwin | macOS |
| x86_64-unknown-none | Bare-metal (x86-64) |

---

## Quickstart

### Build and run an example
```bash
cargo run -p dust -- build examples/K/k_hello_world.ds
./target/dust/k_hello_world
```

Expected output:
```text
Hello, Dust
```

### Build a bare-metal kernel
```bash
cargo run -p dust -- obj kernel.ds --bare-metal -o kernel.bin
```

### Build an object file
```bash
cargo run -p dust -- obj mymodule.ds -o mymodule.o
```

### Build + link a multi-object bare-metal kernel
```bash
cargo run -p dust -- obj xdv-kernel/sector/xdv_kernel/src xdv-runtime/src xdv-xdvfs/src --target x86_64-pc-none-elf --out-dir target/dust/kernel_objs
dustlink --oformat=binary --image-base 0x100000 -Ttext 0x100000 -e _dust_kernel_start -o target/dust/xdv-kernel.bin target/dust/kernel_objs/*.o
```

---

## Dust Runtime (dust-runtime/)

OS services for bare-metal DPL kernels written entirely in DPL:

- **Console I/O** - VGA text mode output
- **Memory Management** - Physical page allocation, heap management
- **Process Management** - Task scheduling and context switching
- **Interrupt Handling** - IDT and IRQ management
- **I/O Ports** - Direct hardware I/O for x86

```dust
K main {
    Runtime::K::init();
    emit "Kernel started";
}
```

### Runtime API

```dust
// Console
Console::K::init()
Console::K::putchar(ch: UInt8)
Console::K::puts(s: UInt64)
Console::K::clear()
Console::K::set_cursor(row: UInt32, col: UInt32)

// Memory
Memory::K::init()
Memory::K::alloc(size: UInt32) -> UInt64
Memory::K::free(ptr: UInt64) -> UInt32
Memory::K::copy(dest: UInt64, src: UInt64, size: UInt32) -> UInt32

// Process
Process::K::init()
Process::K::create_task(entry: UInt64, stack: UInt64) -> UInt32
Process::K::schedule()
Process::K::yield()

// Interrupts
Interrupts::K::init()
Interrupts::K::enable()
Interrupts::K::disable()
Interrupts::K::register_handler(vector: UInt8, handler: UInt64)

// I/O Ports
IOPort::K::inb(port: UInt16) -> UInt8
IOPort::K::outb(port: UInt16, value: UInt8)
IOPort::K::inw(port: UInt16) -> UInt16
IOPort::K::outw(port: UInt16, value: UInt16)
```

---

## XDV OS Integration

Dust compiles to bare-metal binaries that can be used with xdv-os:

```
xdv-os/
├── boot_sector.asm    # 512-byte boot sector
├── kernel.asm         # Assembly kernel entry
├── kernel.ds          # DPL kernel
└── kernel.bin         # Compiled kernel
```

Build the bootable image:
```bash
cd xdv-os/src
./build.sh  # Linux/Mac
build.bat   # Windows
```

---

## Architecture

```
.ds source → dust parser → dust semantics → DIR IR
                                      ↓
                         dust_codegen (Cranelift)
                                      ↓
              ┌─────────────┬──────────────┴──────────────┐
              ↓             ↓                           ↓
        Executable    Object File (.o)           Bare-Metal (.bin)
        (ELF/PE/Mach)  (for linking)            (flat binary)
```

---

## CI Verification

GitHub Actions verifies:
- Workspace build
- Unit tests
- Example compilation
- Executable execution
- Object file generation
- Bare-metal kernel compilation

---

## License

Dust Open Source License  
© 2026 Dust LLC
