// File: host_runtime_shim_tls_synth.rs - This file is part of the DPL Toolchain
// Copyright (c) 2026 Dust LLC, and Contributors
// Description:
//   Test module for TLS synthetic relocation functionality in the host runtime shim.
//   Tests AArch64 and x86_64 TLS models including:
//     - LE (Local Exec)
//     - IE (Initial Exec via GOT)
//     - GD/LD (Global/Local Dynamic)
//     - TLSDESC (TLS Descriptor)

include!("../src/host_runtime_shim.rs");
