#![allow(clippy::missing_safety_doc)]
#![allow(clippy::too_many_arguments)]

use std::collections::{HashMap, HashSet};
use std::ffi::{CStr, CString};
use std::fs::{self, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::os::raw::c_char;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, OnceLock};

const ERR_OK: u32 = 0;
const ERR_FILE_NOT_FOUND: u32 = 1;
const ERR_INVALID_FORMAT: u32 = 2;
const ERR_INVALID_SECTION: u32 = 3;
const ERR_UNDEFINED_SYMBOL: u32 = 4;
const ERR_MULTIPLE_DEFINITION: u32 = 5;
const ERR_INVALID_RELOCATION: u32 = 6;
const ERR_INVALID_ADDRESS: u32 = 8;
const ERR_INVALID_ENTRY: u32 = 9;
const ERR_WRITE_FAILED: u32 = 10;
const ERR_INVALID_IMAGE: u32 = 11;
const ERR_UNSUPPORTED_FLAG: u32 = 12;
const ERR_MISSING_FLAG_VALUE: u32 = 13;
const ERR_UNSUPPORTED_TARGET: u32 = 14;
const ERR_EMPTY_INPUT: u32 = 15;
const ERR_CONFLICTING_OPTIONS: u32 = 16;
const ERR_NOT_IMPLEMENTED_YET: u32 = 17;

const DEFAULT_OUTPUT_HANDLE: u64 = 1;
const DEFAULT_OUTPUT_NAME: &str = "a.out";
const SHN_UNDEF: u16 = 0;
const SHN_ABS: u16 = 65521;
const SHF_ALLOC: u64 = 0x2;
const SHF_WRITE: u64 = 0x1;
const SHF_EXECINSTR: u64 = 0x4;
const SHT_NOBITS: u32 = 8;
const SHT_PROGBITS: u32 = 1;
const SHT_DYNAMIC: u32 = 6;
const SHT_DYNSYM: u32 = 11;
const PT_LOAD: u32 = 1;
const PT_DYNAMIC: u32 = 2;
const PT_INTERP: u32 = 3;
const EM_X86_64: u16 = 62;
const EM_AARCH64: u16 = 183;
const ET_EXEC: u16 = 2;
const ET_DYN: u16 = 3;
const EV_CURRENT: u32 = 1;
const DT_NULL: u64 = 0;
const DT_NEEDED: u64 = 1;
const DT_HASH: u64 = 4;
const DT_STRTAB: u64 = 5;
const DT_STRSZ: u64 = 10;
const DT_SONAME: u64 = 14;
const DT_RPATH: u64 = 15;
const DT_RUNPATH: u64 = 29;
const DT_FLAGS: u64 = 30;
const DT_GNU_HASH: u64 = 0x6fff_fef5;
const DF_BIND_NOW: u64 = 0x8;

const FORMAT_ELF64: u32 = 1;
const FORMAT_BINARY: u32 = 2;
const FORMAT_MBR: u32 = 3;
const FORMAT_PE64: u32 = 4;
const FORMAT_MACHO64: u32 = 5;

const TARGET_NONE: u32 = 0;
const TARGET_X86_64_NONE: u32 = 0;
const TARGET_X86_64_LINUX: u32 = 1;
const TARGET_X86_64_WINDOWS: u32 = 2;
const TARGET_X86_64_MACOS: u32 = 3;
const TARGET_AARCH64_LINUX: u32 = 4;
const TARGET_AARCH64_WINDOWS: u32 = 5;
const TARGET_AARCH64_MACOS: u32 = 6;

const BUILD_ID_MODE_NONE: u32 = 0;
const BUILD_ID_MODE_FAST: u32 = 1;
const BUILD_ID_MODE_MD5: u32 = 2;
const BUILD_ID_MODE_SHA1: u32 = 3;
const BUILD_ID_MODE_UUID: u32 = 4;
const BUILD_ID_MODE_HEX: u32 = 5;

const HASH_STYLE_NONE: u32 = 0;
const HASH_STYLE_SYSV: u32 = 1;
const HASH_STYLE_GNU: u32 = 2;
const HASH_STYLE_BOTH: u32 = 3;

const ICF_MODE_NONE: u32 = 0;
const ICF_MODE_SAFE: u32 = 1;
const ICF_MODE_ALL: u32 = 2;

const OBJECT_FORMAT_UNKNOWN: u32 = 0;
const OBJECT_FORMAT_ELF64: u32 = 1;
const OBJECT_FORMAT_COFF64: u32 = 2;
const OBJECT_FORMAT_MACHO64: u32 = 3;

const COFF_MACHINE_X86_64: u16 = 0x8664;
const COFF_MACHINE_X86: u16 = 0x14c;
const COFF_MACHINE_AARCH64: u16 = 0xaa64;
const MACH_CPU_X86_64: u32 = 0x0100_0007;
const MACH_CPU_ARM64: u32 = 0x0100_000c;

const R_X86_64_NONE: u32 = 0;
const R_X86_64_64: u32 = 1;
const R_X86_64_PC32: u32 = 2;
const R_X86_64_32: u32 = 10;
const R_X86_64_32S: u32 = 11;
const R_X86_64_GOTPCREL: u32 = 9;
const R_AARCH64_NONE: u32 = 0;
const R_AARCH64_ABS64: u32 = 257;
const R_AARCH64_ABS32: u32 = 258;
const R_AARCH64_PREL32: u32 = 261;

const STORAGE_CLASS_EXTERNAL: u8 = 2;
const STORAGE_CLASS_STATIC: u8 = 3;
const STORAGE_CLASS_FILE: u8 = 103;
const STORAGE_CLASS_WEAK_EXTERNAL: u8 = 105;

#[derive(Clone, Default)]
struct ObjectSection {
    index: u32,
    section_type: u32,
    flags: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    align: u64,
    entsize: u64,
    data: Vec<u8>,
}

#[derive(Clone, Default)]
struct ObjectSymbol {
    name_hash: u64,
    bind: u8,
    sym_type: u8,
    shndx: u16,
    value: u64,
    size: u64,
    strtab_section: u32,
}

#[derive(Clone, Default)]
struct ObjectRelocation {
    section: u32,
    offset: u64,
    reloc_type: u32,
    symbol: u32,
    addend: u64,
}

#[derive(Clone, Default)]
struct ObjectRecord {
    path: u64,
    file_size: u64,
    elf_type: u16,
    machine: u16,
    sections: Vec<ObjectSection>,
    symbols: Vec<ObjectSymbol>,
    relocations: Vec<ObjectRelocation>,
}

#[derive(Clone, Default)]
struct GlobalSymbol {
    object_index: u32,
    symbol_index: u32,
    bind: u8,
    defined: u32,
    address: u64,
}

#[derive(Clone, Default)]
struct ArchiveMember {
    name: String,
    data_offset: usize,
    data_size: usize,
}

#[derive(Default)]
struct LinkerState {
    last_error: u64,
    output_path: u64,
    output_format: u32,
    target: u32,
    entry: u64,
    image_base: u64,
    arch: u32,
    os: u32,
    search_paths: Vec<u64>,
    archives: Vec<u64>,
    inputs: Vec<u64>,
    current_object: Option<ObjectRecord>,
    objects: Vec<ObjectRecord>,
    globals: HashMap<u64, GlobalSymbol>,
    required_symbols: Vec<u64>,
    output_sections: Vec<u64>,
    image_size: u32,
    active_patch_object: Option<u32>,
    strip_debug: bool,
    gc_sections: bool,
    allow_multiple_definition: bool,
    build_id_mode: u32,
    build_id_bytes: Vec<u8>,
    z_relro: bool,
    z_now: bool,
    z_execstack: bool,
    memory_origin: u64,
    memory_length: u64,
    pending_elf_entry: u64,
    pending_elf_image_base: u64,
    extracted_members: Vec<PathBuf>,
    loaded_archive_members: HashSet<String>,
    archive_progress: u32,
    group_archive_starts: Vec<u32>,
    link_shared: bool,
    link_pie: bool,
    link_static: bool,
    link_as_needed: bool,
    whole_archive: bool,
    link_new_dtags: bool,
    link_copy_dt_needed_entries: bool,
    no_undefined: bool,
    sysroot_path: u64,
    rpaths: Vec<u64>,
    rpath_links: Vec<u64>,
    dynamic_linker_path: u64,
    soname: u64,
    needed_shared_libs: Vec<u64>,
    hash_style: u32,
    thread_count: u32,
    emit_eh_frame_hdr: bool,
    fatal_warnings: bool,
    color_diagnostics: bool,
    print_gc_sections: bool,
    icf_mode: u32,
    pe_no_entry: bool,
    pe_dynamic_base: bool,
    pe_nx_compat: bool,
    pe_large_address_aware: bool,
    dependency_file: u64,
    emit_relocs: bool,
}

static STRINGS: OnceLock<Mutex<Vec<CString>>> = OnceLock::new();
static ARGS: OnceLock<Mutex<Vec<CString>>> = OnceLock::new();
static LINKER: OnceLock<Mutex<LinkerState>> = OnceLock::new();

fn strings() -> &'static Mutex<Vec<CString>> {
    STRINGS.get_or_init(|| Mutex::new(Vec::new()))
}

fn args() -> &'static Mutex<Vec<CString>> {
    ARGS.get_or_init(|| Mutex::new(Vec::new()))
}

fn linker() -> &'static Mutex<LinkerState> {
    LINKER.get_or_init(|| {
        let mut state = LinkerState::default();
        reset_linker_state_defaults(&mut state);
        Mutex::new(state)
    })
}

fn intern_string<S: AsRef<str>>(value: S) -> u64 {
    let mut sanitized = value.as_ref().replace('\0', "");
    if sanitized.is_empty() {
        sanitized.push('_');
    }
    let cstr = match CString::new(sanitized) {
        Ok(v) => v,
        Err(_) => CString::new("_").unwrap_or_else(|_| unsafe { CString::from_vec_unchecked(vec![b'_', 0]) }),
    };
    let ptr = cstr.as_ptr() as u64;
    let mut pool = strings().lock().expect("strings mutex poisoned");
    pool.push(cstr);
    ptr
}

fn read_c_string(handle: u64) -> Option<String> {
    if handle < 0x10000 {
        return None;
    }
    let ptr = handle as *const c_char;
    if ptr.is_null() {
        return None;
    }
    let text = unsafe { CStr::from_ptr(ptr) }.to_string_lossy().to_string();
    Some(text)
}

fn as_string_or_number(handle: u64) -> String {
    read_c_string(handle).unwrap_or_else(|| handle.to_string())
}

fn to_path(handle: u64) -> Option<PathBuf> {
    if let Some(p) = read_c_string(handle).map(PathBuf::from) {
        return Some(p);
    }
    if handle == DEFAULT_OUTPUT_HANDLE {
        return Some(PathBuf::from(DEFAULT_OUTPUT_NAME));
    }
    if handle == 0 {
        return None;
    }
    Some(PathBuf::from(handle.to_string()))
}

fn push_unique_search_path(paths: &mut Vec<String>, path: String) {
    let trimmed = path.trim().to_string();
    if trimmed.is_empty() {
        return;
    }
    if !paths.iter().any(|existing| existing == &trimmed) {
        paths.push(trimmed);
    }
}

fn target_is_windows(target: u32) -> bool {
    target == TARGET_X86_64_WINDOWS || target == TARGET_AARCH64_WINDOWS
}

fn target_is_macos(target: u32) -> bool {
    target == TARGET_X86_64_MACOS || target == TARGET_AARCH64_MACOS
}

fn target_is_linux(target: u32) -> bool {
    target == TARGET_X86_64_LINUX || target == TARGET_AARCH64_LINUX || target == TARGET_NONE
}

fn target_is_aarch64(target: u32) -> bool {
    target == TARGET_AARCH64_LINUX || target == TARGET_AARCH64_WINDOWS || target == TARGET_AARCH64_MACOS
}

fn target_elf_machine(target: u32) -> u16 {
    if target_is_aarch64(target) {
        EM_AARCH64
    } else {
        EM_X86_64
    }
}

fn target_pe_machine(target: u32) -> u16 {
    if target == TARGET_AARCH64_WINDOWS {
        COFF_MACHINE_AARCH64
    } else {
        COFF_MACHINE_X86_64
    }
}

fn target_macho_cpu_type(target: u32) -> (u32, u32) {
    if target == TARGET_AARCH64_MACOS {
        (MACH_CPU_ARM64, 0)
    } else {
        (MACH_CPU_X86_64, 3)
    }
}

fn linker_default_search_paths(target: u32, sysroot: Option<&Path>) -> Vec<String> {
    let suffixes: &[&str] = if target_is_windows(target) {
        &["lib", "lib64", "usr/lib", "usr/lib64"]
    } else if target_is_macos(target) {
        &["usr/lib", "usr/local/lib", "lib"]
    } else {
        &["lib64", "usr/lib64", "lib", "usr/lib", "usr/local/lib"]
    };

    let mut out = Vec::new();
    for suffix in suffixes {
        if let Some(root) = sysroot {
            let mut joined = PathBuf::from(root);
            for component in suffix.split('/') {
                if !component.is_empty() {
                    joined.push(component);
                }
            }
            push_unique_search_path(&mut out, joined.to_string_lossy().to_string());
        } else {
            let full = format!("/{}", suffix);
            push_unique_search_path(&mut out, full);
        }
    }

    if target_is_windows(target) && sysroot.is_none() {
        push_unique_search_path(&mut out, "C:\\Windows\\System32".to_string());
        push_unique_search_path(&mut out, "C:\\Windows".to_string());
        push_unique_search_path(&mut out, ".".to_string());
    }

    out
}

fn refresh_args() {
    let mut cache = args().lock().expect("args mutex poisoned");
    if !cache.is_empty() {
        return;
    }
    let mut out = Vec::new();
    for arg in std::env::args() {
        let value = if arg.contains('\0') { arg.replace('\0', "") } else { arg };
        let c = CString::new(value).unwrap_or_else(|_| CString::new("_").expect("cstr fallback"));
        out.push(c);
    }
    *cache = out;
}

fn set_last_error(state: &mut LinkerState, code: u32) -> u32 {
    state.last_error = code as u64;
    code
}

fn align_up(value: u64, alignment: u64) -> u64 {
    if alignment <= 1 {
        return value;
    }
    let rem = value % alignment;
    if rem == 0 {
        value
    } else {
        value.saturating_add(alignment - rem)
    }
}

fn read_path_bytes_range(path: &Path, offset: u64, size: u64) -> Option<Vec<u8>> {
    let end = offset.checked_add(size)?;
    let meta = fs::metadata(path).ok()?;
    if end > meta.len() {
        return None;
    }
    let mut f = OpenOptions::new().read(true).open(path).ok()?;
    f.seek(SeekFrom::Start(offset)).ok()?;
    let mut buf = vec![0u8; size as usize];
    f.read_exact(&mut buf).ok()?;
    Some(buf)
}

fn parse_ascii_decimal(bytes: &[u8]) -> Option<usize> {
    let text = std::str::from_utf8(bytes).ok()?.trim();
    if text.is_empty() {
        return None;
    }
    text.parse::<usize>().ok()
}

fn parse_archive_members(path: &Path) -> Option<Vec<ArchiveMember>> {
    let raw = fs::read(path).ok()?;
    if raw.len() < 8 || &raw[0..8] != b"!<arch>\n" {
        return None;
    }

    let mut out = Vec::new();
    let mut pos: usize = 8;
    while pos + 60 <= raw.len() {
        let hdr = &raw[pos..pos + 60];
        if hdr[58] != b'`' || hdr[59] != b'\n' {
            break;
        }
        let mut name = String::from_utf8_lossy(&hdr[0..16]).trim().to_string();
        let size = match parse_ascii_decimal(&hdr[48..58]) {
            Some(v) => v,
            None => break,
        };
        let member_data_start = pos + 60;
        if member_data_start + size > raw.len() {
            break;
        }

        let mut payload_offset = member_data_start;
        let mut payload_size = size;
        if let Some(rest) = name.strip_prefix("#1/") {
            if let Ok(name_len) = rest.trim().parse::<usize>() {
                if name_len <= payload_size {
                    let name_bytes = &raw[payload_offset..payload_offset + name_len];
                    name = String::from_utf8_lossy(name_bytes)
                        .trim_end_matches('\0')
                        .to_string();
                    payload_offset += name_len;
                    payload_size -= name_len;
                }
            }
        } else {
            name = name.trim_end_matches('/').trim().to_string();
        }

        let is_special = name.is_empty() || name == "/" || name == "//" || name.starts_with('/');
        if !is_special {
            out.push(ArchiveMember {
                name,
                data_offset: payload_offset,
                data_size: payload_size,
            });
        }

        pos = member_data_start + size;
        if pos % 2 == 1 {
            pos += 1;
        }
    }

    Some(out)
}

fn archive_member_at(path: &Path, logical_index: usize) -> Option<ArchiveMember> {
    let members = parse_archive_members(path)?;
    members.get(logical_index).cloned()
}

fn archive_member_key(path: &Path, index: u32) -> String {
    format!("{}#{}", path.to_string_lossy(), index)
}

fn unresolved_required_symbol_hashes(state: &LinkerState) -> HashSet<u64> {
    let mut needed = HashSet::new();
    for object in &state.objects {
        for symbol in &object.symbols {
            if symbol.shndx == SHN_UNDEF && symbol.bind != 0 && symbol.name_hash != 0 {
                let defined = state
                    .globals
                    .get(&symbol.name_hash)
                    .map(|g| g.defined == 1)
                    .unwrap_or(false);
                if !defined {
                    needed.insert(symbol.name_hash);
                }
            }
        }
    }
    for hash in &state.required_symbols {
        if *hash == 0 {
            continue;
        }
        let defined = state
            .globals
            .get(hash)
            .map(|g| g.defined == 1)
            .unwrap_or(false);
        if !defined {
            needed.insert(*hash);
        }
    }
    needed
}

fn elf_member_defined_hashes(payload: &[u8]) -> HashSet<u64> {
    let mut out = HashSet::new();
    if payload.len() < 64 {
        return out;
    }
    if &payload[0..4] != b"\x7fELF" || payload[4] != 2 || payload[5] != 1 {
        return out;
    }
    let shoff = match read_u64_le_at(payload, 40) {
        Some(v) => v as usize,
        None => return out,
    };
    let shentsize = match read_u16_le_at(payload, 58) {
        Some(v) => v as usize,
        None => return out,
    };
    let shnum = match read_u16_le_at(payload, 60) {
        Some(v) => v as usize,
        None => return out,
    };
    if shoff == 0 || shentsize < 64 || shnum == 0 {
        return out;
    }
    let sh_table_size = match shentsize.checked_mul(shnum) {
        Some(v) => v,
        None => return out,
    };
    let sh_end = match shoff.checked_add(sh_table_size) {
        Some(v) => v,
        None => return out,
    };
    if sh_end > payload.len() {
        return out;
    }

    for sec_idx in 0..shnum {
        let sh_base = shoff + (sec_idx * shentsize);
        let sh_type = match read_u32_le_at(payload, sh_base + 4) {
            Some(v) => v,
            None => continue,
        };
        if sh_type != 2 {
            continue;
        }
        let sym_off = match read_u64_le_at(payload, sh_base + 24) {
            Some(v) => v as usize,
            None => continue,
        };
        let sym_size = match read_u64_le_at(payload, sh_base + 32) {
            Some(v) => v as usize,
            None => continue,
        };
        let strtab_index = match read_u32_le_at(payload, sh_base + 40) {
            Some(v) => v as usize,
            None => continue,
        };
        let sym_entsize = match read_u64_le_at(payload, sh_base + 56) {
            Some(v) => v as usize,
            None => continue,
        };
        if sym_entsize < 24 || sym_size < sym_entsize {
            continue;
        }
        let sym_end = match sym_off.checked_add(sym_size) {
            Some(v) => v,
            None => continue,
        };
        if sym_end > payload.len() {
            continue;
        }

        let str_sh_base = match shoff.checked_add(strtab_index.saturating_mul(shentsize)) {
            Some(v) => v,
            None => continue,
        };
        if str_sh_base + 64 > payload.len() {
            continue;
        }
        let str_off = match read_u64_le_at(payload, str_sh_base + 24) {
            Some(v) => v as usize,
            None => continue,
        };
        let str_size = match read_u64_le_at(payload, str_sh_base + 32) {
            Some(v) => v as usize,
            None => continue,
        };
        let str_end = match str_off.checked_add(str_size) {
            Some(v) => v,
            None => continue,
        };
        if str_end > payload.len() || str_size == 0 {
            continue;
        }
        let strtab = &payload[str_off..str_end];
        let count = sym_size / sym_entsize;
        for i in 0..count {
            let rec = sym_off + (i * sym_entsize);
            if rec + 24 > payload.len() {
                break;
            }
            let st_name = match read_u32_le_at(payload, rec) {
                Some(v) => v as usize,
                None => continue,
            };
            let st_info = payload[rec + 4];
            let st_shndx = match read_u16_le_at(payload, rec + 6) {
                Some(v) => v,
                None => continue,
            };
            let bind = st_info >> 4;
            if st_shndx == SHN_UNDEF || bind == 0 || st_name == 0 || st_name >= strtab.len() {
                continue;
            }
            let rel = &strtab[st_name..];
            let end = rel.iter().position(|b| *b == 0).unwrap_or(rel.len());
            if end == 0 {
                continue;
            }
            let name = String::from_utf8_lossy(&rel[..end]).to_string();
            if !name.is_empty() {
                out.insert(fnv1a64(&name));
            }
        }
    }

    out
}

fn coff_member_defined_hashes(payload: &[u8]) -> HashSet<u64> {
    let mut out = HashSet::new();
    if payload.len() < 20 {
        return out;
    }
    let machine = match read_u16_le_at(payload, 0) {
        Some(v) => v,
        None => return out,
    };
    if machine != COFF_MACHINE_X86_64 && machine != COFF_MACHINE_AARCH64 {
        return out;
    }
    let ptr_symtab = match read_u32_le_at(payload, 8) {
        Some(v) => v as usize,
        None => return out,
    };
    let symbol_count = match read_u32_le_at(payload, 12) {
        Some(v) => v as usize,
        None => return out,
    };
    if ptr_symtab == 0 || symbol_count == 0 {
        return out;
    }
    let sym_end = match ptr_symtab.checked_add(symbol_count.saturating_mul(18)) {
        Some(v) => v,
        None => return out,
    };
    if sym_end > payload.len() {
        return out;
    }
    let mut strtab: &[u8] = &[0, 0, 0, 0];
    if sym_end + 4 <= payload.len() {
        if let Some(str_size_u32) = read_u32_le_at(payload, sym_end) {
            let str_size = str_size_u32 as usize;
            let str_end = sym_end.saturating_add(str_size);
            if str_size >= 4 && str_end <= payload.len() {
                strtab = &payload[sym_end..str_end];
            }
        }
    }

    let mut raw_index = 0usize;
    while raw_index < symbol_count {
        let sym_off = ptr_symtab + (raw_index * 18);
        if sym_off + 18 > payload.len() {
            break;
        }
        let section_number = read_i16_le_at(payload, sym_off + 12).unwrap_or(0);
        let storage_class = payload[sym_off + 16];
        let aux_count = payload[sym_off + 17] as usize;
        if section_number > 0
            && (storage_class == STORAGE_CLASS_EXTERNAL
                || storage_class == STORAGE_CLASS_WEAK_EXTERNAL)
        {
            let name_raw = &payload[sym_off..sym_off + 8];
            let name = read_coff_name(name_raw, strtab);
            if !name.is_empty() {
                out.insert(fnv1a64(&name));
            }
        }
        raw_index = raw_index.saturating_add(1 + aux_count);
    }

    out
}

fn macho_member_defined_hashes(payload: &[u8]) -> HashSet<u64> {
    let mut out = HashSet::new();
    if payload.len() < 32 {
        return out;
    }
    let magic = match read_u32_le_at(payload, 0) {
        Some(v) => v,
        None => return out,
    };
    if magic != 0xfeedfacf && magic != 0xcffaedfe {
        return out;
    }
    let cpu_type = match read_u32_le_at(payload, 4) {
        Some(v) => v,
        None => return out,
    };
    if cpu_type != MACH_CPU_X86_64 && cpu_type != MACH_CPU_ARM64 {
        return out;
    }
    let ncmds = match read_u32_le_at(payload, 16) {
        Some(v) => v as usize,
        None => return out,
    };
    let sizeofcmds = match read_u32_le_at(payload, 20) {
        Some(v) => v as usize,
        None => return out,
    };
    if 32usize.saturating_add(sizeofcmds) > payload.len() {
        return out;
    }

    let mut symoff = 0usize;
    let mut nsyms = 0usize;
    let mut stroff = 0usize;
    let mut strsize = 0usize;
    let mut cursor = 32usize;
    for _ in 0..ncmds {
        if cursor + 8 > payload.len() {
            return out;
        }
        let cmd = match read_u32_le_at(payload, cursor) {
            Some(v) => v,
            None => return out,
        };
        let cmdsize = match read_u32_le_at(payload, cursor + 4) {
            Some(v) => v as usize,
            None => return out,
        };
        if cmdsize < 8 || cursor + cmdsize > payload.len() {
            return out;
        }
        if cmd == 0x2 {
            symoff = read_u32_le_at(payload, cursor + 8).unwrap_or(0) as usize;
            nsyms = read_u32_le_at(payload, cursor + 12).unwrap_or(0) as usize;
            stroff = read_u32_le_at(payload, cursor + 16).unwrap_or(0) as usize;
            strsize = read_u32_le_at(payload, cursor + 20).unwrap_or(0) as usize;
        }
        cursor = cursor.saturating_add(cmdsize);
    }

    if nsyms == 0 || strsize == 0 {
        return out;
    }
    let sym_end = match symoff.checked_add(nsyms.saturating_mul(16)) {
        Some(v) => v,
        None => return out,
    };
    let str_end = match stroff.checked_add(strsize) {
        Some(v) => v,
        None => return out,
    };
    if sym_end > payload.len() || str_end > payload.len() {
        return out;
    }
    let strtab = &payload[stroff..str_end];
    for i in 0..nsyms {
        let off = symoff + (i * 16);
        if off + 16 > payload.len() {
            break;
        }
        let strx = read_u32_le_at(payload, off).unwrap_or(0) as usize;
        let n_type = payload[off + 4];
        let n_type_kind = n_type & 0x0e;
        let is_external = (n_type & 0x01) != 0;
        if !is_external || n_type_kind == 0x00 || strx >= strtab.len() {
            continue;
        }
        let rel = &strtab[strx..];
        let end = rel.iter().position(|b| *b == 0).unwrap_or(rel.len());
        if end == 0 {
            continue;
        }
        let name = String::from_utf8_lossy(&rel[..end]).to_string();
        if !name.is_empty() {
            out.insert(fnv1a64(&name));
        }
    }
    out
}

fn archive_member_defined_symbol_hashes(payload: &[u8]) -> HashSet<u64> {
    match probe_object_format_bytes(payload) {
        OBJECT_FORMAT_ELF64 => elf_member_defined_hashes(payload),
        OBJECT_FORMAT_COFF64 => coff_member_defined_hashes(payload),
        OBJECT_FORMAT_MACHO64 => macho_member_defined_hashes(payload),
        _ => HashSet::new(),
    }
}

fn archive_member_matches_unresolved_state(state: &LinkerState, path: &Path, index: u32) -> u32 {
    let key = archive_member_key(path, index);
    if state.loaded_archive_members.contains(&key) {
        return 0;
    }
    let needed = unresolved_required_symbol_hashes(state);
    if needed.is_empty() {
        return 0;
    }

    let member = match archive_member_at(path, index as usize) {
        Some(v) => v,
        None => return 0,
    };
    let raw = match fs::read(path) {
        Ok(v) => v,
        Err(_) => return 0,
    };
    let end = member.data_offset.saturating_add(member.data_size);
    if end > raw.len() || member.data_size == 0 {
        return 0;
    }
    let payload = &raw[member.data_offset..end];
    let defined = archive_member_defined_symbol_hashes(payload);
    for hash in defined {
        if needed.contains(&hash) {
            return 1;
        }
    }
    0
}

fn build_section_runtime_address(state: &LinkerState, object_index: u32, section_index: u32) -> u64 {
    let mut cursor = if state.image_base == 0 {
        0x0010_0000
    } else {
        state.image_base
    };
    cursor = align_up(cursor, 0x1000);

    for (obj_idx, object) in state.objects.iter().enumerate() {
        for section in &object.sections {
            if section.flags & SHF_ALLOC == 0 {
                continue;
            }
            let sec_align = section.align.max(16);
            cursor = align_up(cursor, sec_align);
            if obj_idx as u32 == object_index && section.index == section_index {
                return cursor;
            }
            let sec_size = section.size.max(section.data.len() as u64);
            cursor = cursor.saturating_add(sec_size);
        }
    }

    if state.image_base == 0 {
        0x0010_0000u64
            .saturating_add((object_index as u64) * 0x0100_0000)
            .saturating_add((section_index as u64) * 0x1000)
    } else {
        state
            .image_base
            .saturating_add((object_index as u64) * 0x0100_0000)
            .saturating_add((section_index as u64) * 0x1000)
    }
}

fn symbol_runtime_address(state: &LinkerState, object_index: u32, symbol_index: u32) -> u64 {
    let object = match state.objects.get(object_index as usize) {
        Some(v) => v,
        None => return 0,
    };
    let symbol = match object.symbols.get(symbol_index as usize) {
        Some(v) => v,
        None => return 0,
    };
    if symbol.shndx == SHN_UNDEF {
        if symbol.name_hash != 0 {
            if let Some(global) = state.globals.get(&symbol.name_hash) {
                if global.defined == 1 {
                    return global.address;
                }
            }
        }
        return 0;
    }
    if symbol.shndx == SHN_ABS {
        return symbol.value;
    }
    let sec_addr = build_section_runtime_address(state, object_index, symbol.shndx as u32);
    sec_addr.saturating_add(symbol.value)
}

fn section_payload_from_object(path: u64, section_type: u32, offset: u64, size: u64) -> Option<Vec<u8>> {
    if size == 0 {
        return Some(Vec::new());
    }
    if size > (usize::MAX as u64) {
        return None;
    }
    if section_type == SHT_NOBITS {
        return Some(vec![0u8; size as usize]);
    }
    let p = to_path(path)?;
    read_path_bytes_range(&p, offset, size)
}

fn symbol_name_hash_from_strtab(
    object: &ObjectRecord,
    st_name: u64,
    strtab_section: u32,
    fallback: u64,
) -> u64 {
    if st_name == 0 {
        return 0;
    }
    let strtab = match object.sections.iter().find(|s| s.index == strtab_section) {
        Some(v) => v,
        None => return fallback,
    };
    let off = st_name as usize;
    if off >= strtab.data.len() {
        return fallback;
    }
    let rest = &strtab.data[off..];
    let end = rest.iter().position(|b| *b == 0).unwrap_or(rest.len());
    if end == 0 {
        return fallback;
    }
    let sym = String::from_utf8_lossy(&rest[..end]).to_string();
    if sym.is_empty() {
        fallback
    } else {
        fnv1a64(&sym)
    }
}

fn build_alloc_segments(state: &LinkerState) -> Vec<(u64, Vec<u8>, u32, u32)> {
    let mut referenced: HashSet<(u32, u32)> = HashSet::new();
    if state.gc_sections {
        for (obj_idx, object) in state.objects.iter().enumerate() {
            for reloc in &object.relocations {
                referenced.insert((obj_idx as u32, reloc.section));
                if let Some(symbol) = object.symbols.get(reloc.symbol as usize) {
                    if symbol.shndx != SHN_UNDEF && symbol.shndx != SHN_ABS {
                        referenced.insert((obj_idx as u32, symbol.shndx as u32));
                    }
                }
            }
        }

        for global in state.globals.values() {
            if global.defined == 1 {
                if let Some(object) = state.objects.get(global.object_index as usize) {
                    if let Some(symbol) = object.symbols.get(global.symbol_index as usize) {
                        if symbol.shndx != SHN_UNDEF && symbol.shndx != SHN_ABS {
                            referenced.insert((global.object_index, symbol.shndx as u32));
                        }
                    }
                }
            }
        }

        if state.entry != 0 {
            for (obj_idx, object) in state.objects.iter().enumerate() {
                for (sym_idx, symbol) in object.symbols.iter().enumerate() {
                    if symbol.shndx == SHN_UNDEF || symbol.shndx == SHN_ABS {
                        continue;
                    }
                    let addr = symbol_runtime_address(state, obj_idx as u32, sym_idx as u32);
                    if addr == state.entry {
                        referenced.insert((obj_idx as u32, symbol.shndx as u32));
                    }
                }
            }
        }
    }

    let mut out = Vec::new();
    for (obj_idx, object) in state.objects.iter().enumerate() {
        let first_alloc = object
            .sections
            .iter()
            .find(|section| section.flags & SHF_ALLOC != 0)
            .map(|section| section.index);
        for section in &object.sections {
            if section.flags & SHF_ALLOC == 0 {
                continue;
            }
            if state.gc_sections {
                let key = (obj_idx as u32, section.index);
                let keep =
                    referenced.contains(&key)
                        || (section.flags & SHF_EXECINSTR != 0)
                        || first_alloc == Some(section.index);
                if !keep {
                    if state.print_gc_sections {
                        eprintln!(
                            "dustlink: gc-sections dropped obj={} sec={} flags=0x{:x}",
                            obj_idx, section.index, section.flags
                        );
                    }
                    continue;
                }
            }
            let addr = build_section_runtime_address(state, obj_idx as u32, section.index);
            let mut bytes = section.data.clone();
            let target_len = section.size.max(bytes.len() as u64) as usize;
            if bytes.len() < target_len {
                bytes.resize(target_len, 0);
            }
            out.push((addr, bytes, obj_idx as u32, section.index));
        }
    }
    out.sort_by_key(|(addr, _, obj, sec)| (*addr, *obj, *sec));
    out
}

#[derive(Clone)]
struct OutputChunk {
    addr: u64,
    bytes: Vec<u8>,
    flags: u64,
}

fn build_output_chunks(state: &LinkerState) -> Vec<OutputChunk> {
    let mut out = Vec::new();
    for (addr, bytes, obj_idx, sec_idx) in build_alloc_segments(state) {
        let flags = state
            .objects
            .get(obj_idx as usize)
            .and_then(|o| o.sections.iter().find(|s| s.index == sec_idx))
            .map(|s| s.flags)
            .unwrap_or(SHF_ALLOC);
        out.push(OutputChunk { addr, bytes, flags });
    }
    out.sort_by_key(|c| c.addr);
    out
}

fn build_flat_image_bytes(state: &LinkerState) -> Vec<u8> {
    let segments = build_alloc_segments(state);
    if segments.is_empty() {
        return Vec::new();
    }
    let base = segments
        .first()
        .map(|(addr, _, _, _)| *addr)
        .unwrap_or(state.image_base);
    let mut image = Vec::new();
    for (addr, bytes, _, _) in segments {
        if addr < base {
            continue;
        }
        let rel = (addr - base) as usize;
        if image.len() < rel {
            image.resize(rel, 0);
        }
        let end = rel + bytes.len();
        if image.len() < end {
            image.resize(end, 0);
        }
        image[rel..end].copy_from_slice(&bytes);
    }
    image
}

fn write_u16_le(dst: &mut [u8], off: usize, value: u16) {
    if off + 2 <= dst.len() {
        dst[off..off + 2].copy_from_slice(&value.to_le_bytes());
    }
}

fn write_u32_le(dst: &mut [u8], off: usize, value: u32) {
    if off + 4 <= dst.len() {
        dst[off..off + 4].copy_from_slice(&value.to_le_bytes());
    }
}

fn write_u64_le(dst: &mut [u8], off: usize, value: u64) {
    if off + 8 <= dst.len() {
        dst[off..off + 8].copy_from_slice(&value.to_le_bytes());
    }
}

fn write_minimal_elf_exec(
    path: &Path,
    entry: u64,
    image_base: u64,
    image_payload: &[u8],
    build_id: &[u8],
    execstack: bool,
    et_type: u16,
    machine: u16,
    interp_path: Option<&str>,
    soname: Option<&str>,
    needed_libs: &[String],
    runpath: Option<&str>,
    use_new_dtags: bool,
    z_now: bool,
    hash_style: u32,
) -> u32 {
    let phoff = 64usize;

    let mut interp_bytes = Vec::new();
    if let Some(path) = interp_path {
        let trimmed = path.trim();
        if !trimmed.is_empty() {
            interp_bytes.extend_from_slice(trimmed.as_bytes());
            if interp_bytes.last().copied().unwrap_or(0) != 0 {
                interp_bytes.push(0);
            }
        }
    }

    let mut dynstr = vec![0u8];
    let mut needed_offsets = Vec::new();
    for needed in needed_libs {
        let name = needed.trim();
        if name.is_empty() {
            continue;
        }
        let off = dynstr.len() as u64;
        dynstr.extend_from_slice(name.as_bytes());
        dynstr.push(0);
        needed_offsets.push(off);
    }
    let mut soname_offset: Option<u64> = None;
    if let Some(raw) = soname {
        let name = raw.trim();
        if !name.is_empty() {
            let off = dynstr.len() as u64;
            dynstr.extend_from_slice(name.as_bytes());
            dynstr.push(0);
            soname_offset = Some(off);
        }
    }
    let mut runpath_offset: Option<u64> = None;
    if let Some(raw) = runpath {
        let value = raw.trim();
        if !value.is_empty() {
            let off = dynstr.len() as u64;
            dynstr.extend_from_slice(value.as_bytes());
            dynstr.push(0);
            runpath_offset = Some(off);
        }
    }
    if dynstr.is_empty() {
        dynstr.push(0);
    }

    let dynamic_enabled = et_type == ET_DYN
        || !interp_bytes.is_empty()
        || !needed_offsets.is_empty()
        || soname_offset.is_some()
        || runpath_offset.is_some()
        || z_now;

    let mut dynamic_entries: Vec<(u64, u64)> = Vec::new();
    if dynamic_enabled {
        for off in &needed_offsets {
            dynamic_entries.push((DT_NEEDED, *off));
        }
        if let Some(off) = soname_offset {
            dynamic_entries.push((DT_SONAME, off));
        }
        if let Some(off) = runpath_offset {
            let runpath_tag = if use_new_dtags { DT_RUNPATH } else { DT_RPATH };
            dynamic_entries.push((runpath_tag, off));
        }
        // DT_STRTAB patched after layout is known.
        dynamic_entries.push((DT_STRTAB, 0));
        if hash_style == HASH_STYLE_SYSV || hash_style == HASH_STYLE_BOTH {
            dynamic_entries.push((DT_HASH, 0));
        }
        if hash_style == HASH_STYLE_GNU || hash_style == HASH_STYLE_BOTH {
            dynamic_entries.push((DT_GNU_HASH, 0));
        }
        dynamic_entries.push((DT_STRSZ, dynstr.len() as u64));
        if z_now {
            dynamic_entries.push((DT_FLAGS, DF_BIND_NOW));
        }
        dynamic_entries.push((DT_NULL, 0));
    }

    let mut phnum = 1usize;
    if !interp_bytes.is_empty() {
        phnum += 1;
    }
    if !dynamic_entries.is_empty() {
        phnum += 1;
    }
    let phdr_end = phoff.saturating_add(phnum.saturating_mul(56));
    let interp_offset = align_up(phdr_end as u64, 16) as usize;
    let mut segment_offset = 0x1000usize;
    if !interp_bytes.is_empty() {
        let interp_end = interp_offset.saturating_add(interp_bytes.len());
        if interp_end > segment_offset {
            segment_offset = align_up(interp_end as u64, 0x1000) as usize;
        }
    }
    let payload = if image_payload.is_empty() {
        vec![0u8; 1]
    } else {
        image_payload.to_vec()
    };
    let payload_offset = segment_offset;
    let payload_end = payload_offset.saturating_add(payload.len());
    let mut total_size = payload_end;
    if !interp_bytes.is_empty() {
        let interp_end = interp_offset.saturating_add(interp_bytes.len());
        if interp_end > total_size {
            total_size = interp_end;
        }
    }

    let mut dynamic_offset = 0usize;
    let mut dynamic_size = 0usize;
    let mut dynstr_offset = 0usize;
    let mut dynstr_end = payload_end;
    let mut sysv_hash_offset = 0usize;
    let mut gnu_hash_offset = 0usize;
    let include_sysv_hash = hash_style == HASH_STYLE_SYSV || hash_style == HASH_STYLE_BOTH;
    let include_gnu_hash = hash_style == HASH_STYLE_GNU || hash_style == HASH_STYLE_BOTH;
    let sysv_hash_bytes: [u8; 16] = [1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let gnu_hash_bytes: [u8; 24] = [
        1, 0, 0, 0, // nbuckets
        1, 0, 0, 0, // symoffset
        1, 0, 0, 0, // bloom size
        0, 0, 0, 0, // bloom shift
        0, 0, 0, 0, // bucket[0]
        0, 0, 0, 0, // chain[0]
    ];
    if !dynamic_entries.is_empty() {
        dynamic_size = dynamic_entries.len().saturating_mul(16);
        dynamic_offset = align_up(payload_end as u64, 16) as usize;
        dynstr_offset = align_up(dynamic_offset.saturating_add(dynamic_size) as u64, 16) as usize;
        dynstr_end = dynstr_offset.saturating_add(dynstr.len());
        let mut hash_cursor = align_up(dynstr_end as u64, 8) as usize;
        if include_sysv_hash {
            sysv_hash_offset = hash_cursor;
            hash_cursor = hash_cursor.saturating_add(sysv_hash_bytes.len());
        }
        if include_gnu_hash {
            hash_cursor = align_up(hash_cursor as u64, 8) as usize;
            gnu_hash_offset = hash_cursor;
            hash_cursor = hash_cursor.saturating_add(gnu_hash_bytes.len());
        }
        if hash_cursor > dynstr_end {
            dynstr_end = hash_cursor;
        }
        if dynstr_end > total_size {
            total_size = dynstr_end;
        }
    }

    let load_end = if dynstr_end > payload_end {
        dynstr_end
    } else {
        payload_end
    };

    let mut out = vec![0u8; total_size];

    // ELF header.
    out[0] = 0x7f;
    out[1] = b'E';
    out[2] = b'L';
    out[3] = b'F';
    out[4] = 2; // 64-bit
    out[5] = 1; // little-endian
    out[6] = 1; // version
    write_u16_le(&mut out, 16, et_type);
    write_u16_le(&mut out, 18, machine);
    write_u32_le(&mut out, 20, EV_CURRENT);
    write_u64_le(
        &mut out,
        24,
        if entry == 0 { image_base.max(0x1000) } else { entry },
    );
    write_u64_le(&mut out, 32, 64); // e_phoff
    write_u64_le(&mut out, 40, 0); // e_shoff
    write_u32_le(&mut out, 48, 0); // flags
    write_u16_le(&mut out, 52, 64); // ehsize
    write_u16_le(&mut out, 54, 56); // phentsize
    write_u16_le(&mut out, 56, phnum as u16); // phnum
    write_u16_le(&mut out, 58, 0); // shentsize
    write_u16_le(&mut out, 60, 0); // shnum
    write_u16_le(&mut out, 62, 0); // shstrndx

    let mut ph_index = 0usize;
    if !interp_bytes.is_empty() {
        let interp_phoff = phoff + ph_index.saturating_mul(56);
        // PT_INTERP program header.
        write_u32_le(&mut out, interp_phoff, PT_INTERP);
        write_u32_le(&mut out, interp_phoff + 4, 0x4);
        write_u64_le(&mut out, interp_phoff + 8, interp_offset as u64);
        write_u64_le(
            &mut out,
            interp_phoff + 16,
            image_base.saturating_add(interp_offset as u64),
        );
        write_u64_le(
            &mut out,
            interp_phoff + 24,
            image_base.saturating_add(interp_offset as u64),
        );
        write_u64_le(&mut out, interp_phoff + 32, interp_bytes.len() as u64);
        write_u64_le(&mut out, interp_phoff + 40, interp_bytes.len() as u64);
        write_u64_le(&mut out, interp_phoff + 48, 1);
        out[interp_offset..interp_offset + interp_bytes.len()].copy_from_slice(&interp_bytes);
        ph_index += 1;
    }

    if !dynamic_entries.is_empty() {
        let dynamic_vaddr = image_base.saturating_add(dynamic_offset.saturating_sub(payload_offset) as u64);
        let dynstr_vaddr = image_base.saturating_add(dynstr_offset.saturating_sub(payload_offset) as u64);
        let sysv_hash_vaddr =
            image_base.saturating_add(sysv_hash_offset.saturating_sub(payload_offset) as u64);
        let gnu_hash_vaddr =
            image_base.saturating_add(gnu_hash_offset.saturating_sub(payload_offset) as u64);
        for i in 0..dynamic_entries.len() {
            if dynamic_entries[i].0 == DT_STRTAB {
                dynamic_entries[i].1 = dynstr_vaddr;
            } else if dynamic_entries[i].0 == DT_HASH {
                dynamic_entries[i].1 = sysv_hash_vaddr;
            } else if dynamic_entries[i].0 == DT_GNU_HASH {
                dynamic_entries[i].1 = gnu_hash_vaddr;
            }
        }
        let dynamic_phoff = phoff + ph_index.saturating_mul(56);
        write_u32_le(&mut out, dynamic_phoff, PT_DYNAMIC);
        write_u32_le(&mut out, dynamic_phoff + 4, 0x6);
        write_u64_le(&mut out, dynamic_phoff + 8, dynamic_offset as u64);
        write_u64_le(&mut out, dynamic_phoff + 16, dynamic_vaddr);
        write_u64_le(&mut out, dynamic_phoff + 24, dynamic_vaddr);
        write_u64_le(&mut out, dynamic_phoff + 32, dynamic_size as u64);
        write_u64_le(&mut out, dynamic_phoff + 40, dynamic_size as u64);
        write_u64_le(&mut out, dynamic_phoff + 48, 8);

        for i in 0..dynamic_entries.len() {
            let off = dynamic_offset + i.saturating_mul(16);
            let (tag, value) = dynamic_entries[i];
            write_u64_le(&mut out, off, tag);
            write_u64_le(&mut out, off + 8, value);
        }
        out[dynstr_offset..dynstr_offset + dynstr.len()].copy_from_slice(&dynstr);
        if include_sysv_hash && sysv_hash_offset != 0 {
            out[sysv_hash_offset..sysv_hash_offset + sysv_hash_bytes.len()]
                .copy_from_slice(&sysv_hash_bytes);
        }
        if include_gnu_hash && gnu_hash_offset != 0 {
            out[gnu_hash_offset..gnu_hash_offset + gnu_hash_bytes.len()]
                .copy_from_slice(&gnu_hash_bytes);
        }
        ph_index += 1;
    }

    // PT_LOAD program header.
    let load_phoff = phoff + ph_index.saturating_mul(56);
    write_u32_le(&mut out, load_phoff, PT_LOAD);
    let segment_flags = if execstack { 0x7 } else { 0x5 };
    write_u32_le(&mut out, load_phoff + 4, segment_flags);
    write_u64_le(&mut out, load_phoff + 8, payload_offset as u64);
    write_u64_le(&mut out, load_phoff + 16, image_base);
    write_u64_le(&mut out, load_phoff + 24, image_base);
    write_u64_le(&mut out, load_phoff + 32, load_end.saturating_sub(payload_offset) as u64);
    write_u64_le(&mut out, load_phoff + 40, load_end.saturating_sub(payload_offset) as u64);
    write_u64_le(&mut out, load_phoff + 48, 0x1000);

    out[payload_offset..payload_offset + payload.len()].copy_from_slice(&payload);
    append_build_id_note(&mut out, build_id);
    write_all(path, &out)
}

fn align_up_u32(value: u32, alignment: u32) -> u32 {
    if alignment <= 1 {
        return value;
    }
    let rem = value % alignment;
    if rem == 0 {
        value
    } else {
        value.saturating_add(alignment - rem)
    }
}

fn read_u16_le_at(data: &[u8], off: usize) -> Option<u16> {
    let end = off.checked_add(2)?;
    if end > data.len() {
        return None;
    }
    Some(u16::from_le_bytes([data[off], data[off + 1]]))
}

fn read_i16_le_at(data: &[u8], off: usize) -> Option<i16> {
    read_u16_le_at(data, off).map(|v| i16::from_le_bytes(v.to_le_bytes()))
}

fn read_u32_le_at(data: &[u8], off: usize) -> Option<u32> {
    let end = off.checked_add(4)?;
    if end > data.len() {
        return None;
    }
    Some(u32::from_le_bytes([
        data[off],
        data[off + 1],
        data[off + 2],
        data[off + 3],
    ]))
}

fn read_i32_le_at(data: &[u8], off: usize) -> Option<i32> {
    read_u32_le_at(data, off).map(|v| i32::from_le_bytes(v.to_le_bytes()))
}

fn read_u64_le_at(data: &[u8], off: usize) -> Option<u64> {
    let end = off.checked_add(8)?;
    if end > data.len() {
        return None;
    }
    Some(u64::from_le_bytes([
        data[off],
        data[off + 1],
        data[off + 2],
        data[off + 3],
        data[off + 4],
        data[off + 5],
        data[off + 6],
        data[off + 7],
    ]))
}

fn trim_quotes(value: &str) -> &str {
    let mut text = value.trim();
    if text.starts_with('"') && text.ends_with('"') && text.len() >= 2 {
        text = &text[1..text.len() - 1];
    }
    if text.starts_with('\'') && text.ends_with('\'') && text.len() >= 2 {
        text = &text[1..text.len() - 1];
    }
    text
}

fn parse_u64_auto(text: &str) -> Option<u64> {
    let raw = text.trim();
    if raw.is_empty() {
        return None;
    }
    if let Some(hex) = raw.strip_prefix("0x").or_else(|| raw.strip_prefix("0X")) {
        return u64::from_str_radix(hex, 16).ok();
    }
    if let Some(bin) = raw.strip_prefix("0b").or_else(|| raw.strip_prefix("0B")) {
        return u64::from_str_radix(bin, 2).ok();
    }
    if let Some(oct) = raw.strip_prefix("0o").or_else(|| raw.strip_prefix("0O")) {
        return u64::from_str_radix(oct, 8).ok();
    }
    raw.parse::<u64>().ok()
}

fn parse_u64_scaled(text: &str) -> Option<u64> {
    let raw = text.trim();
    if raw.is_empty() {
        return None;
    }
    if raw.ends_with('k') || raw.ends_with('K') {
        return parse_u64_auto(&raw[..raw.len().saturating_sub(1)]).map(|v| v.saturating_mul(1024));
    }
    if raw.ends_with('m') || raw.ends_with('M') {
        return parse_u64_auto(&raw[..raw.len().saturating_sub(1)])
            .map(|v| v.saturating_mul(1024 * 1024));
    }
    if raw.ends_with('g') || raw.ends_with('G') {
        return parse_u64_auto(&raw[..raw.len().saturating_sub(1)])
            .map(|v| v.saturating_mul(1024 * 1024 * 1024));
    }
    parse_u64_auto(raw)
}

fn basename_from_token(token: &str) -> String {
    let trimmed = token.trim();
    if trimmed.is_empty() {
        return String::new();
    }
    let p = Path::new(trimmed);
    if let Some(name) = p.file_name().and_then(|n| n.to_str()) {
        return name.to_string();
    }
    trimmed.to_string()
}

fn has_shared_suffix(name: &str) -> bool {
    let lower = name.to_ascii_lowercase();
    if lower.ends_with(".so") || lower.contains(".so.") {
        return true;
    }
    if lower.ends_with(".dylib") || lower.ends_with(".dll") {
        return true;
    }
    false
}

fn normalize_needed_library_name(requested: &str, resolved: &str) -> Option<String> {
    let req_base = basename_from_token(requested);
    let res_base = basename_from_token(resolved);

    if has_shared_suffix(&req_base) {
        return Some(req_base);
    }
    if has_shared_suffix(&res_base) {
        return Some(res_base);
    }

    let req_lower = req_base.to_ascii_lowercase();
    if req_lower.ends_with(".a") {
        let stem = req_base[..req_base.len().saturating_sub(2)].to_string();
        return Some(format!("{stem}.so"));
    }
    if req_lower.ends_with(".lib") {
        let stem = req_base[..req_base.len().saturating_sub(4)].to_string();
        return Some(format!("{stem}.dll"));
    }
    if req_base.contains('.') {
        return Some(req_base);
    }
    if req_base.is_empty() {
        return None;
    }
    Some(format!("lib{}.so", req_base))
}

fn linker_has_needed_library(state: &LinkerState, name: &str) -> bool {
    for handle in &state.needed_shared_libs {
        if let Some(existing) = read_c_string(*handle) {
            if existing == name {
                return true;
            }
        }
    }
    false
}

fn add_needed_library_if_missing(state: &mut LinkerState, name: &str) {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return;
    }
    if linker_has_needed_library(state, trimmed) {
        return;
    }
    let handle = intern_string(trimmed);
    state.needed_shared_libs.push(handle);
}

fn parse_elf_needed_libraries(raw: &[u8]) -> Vec<String> {
    let mut out = Vec::new();
    if raw.len() < 64 || &raw[0..4] != b"\x7fELF" {
        return out;
    }
    if raw[4] != 2 || raw[5] != 1 {
        return out;
    }

    let shoff = match read_u64_le_at(raw, 40) {
        Some(v) => v as usize,
        None => return out,
    };
    let shentsize = match read_u16_le_at(raw, 58) {
        Some(v) => v as usize,
        None => return out,
    };
    let shnum = match read_u16_le_at(raw, 60) {
        Some(v) => v as usize,
        None => return out,
    };
    if shoff == 0 || shentsize < 64 || shnum == 0 {
        return out;
    }
    if shoff.saturating_add(shentsize.saturating_mul(shnum)) > raw.len() {
        return out;
    }

    let mut dynamic_offset = 0usize;
    let mut dynamic_size = 0usize;
    let mut dynstr_section = 0usize;
    for idx in 0..shnum {
        let off = shoff + idx.saturating_mul(shentsize);
        let sh_type = match read_u32_le_at(raw, off + 4) {
            Some(v) => v,
            None => continue,
        };
        if sh_type == SHT_DYNAMIC {
            dynamic_offset = match read_u64_le_at(raw, off + 24) {
                Some(v) => v as usize,
                None => 0,
            };
            dynamic_size = match read_u64_le_at(raw, off + 32) {
                Some(v) => v as usize,
                None => 0,
            };
            dynstr_section = match read_u32_le_at(raw, off + 40) {
                Some(v) => v as usize,
                None => 0,
            };
            break;
        }
    }
    if dynamic_offset == 0 || dynamic_size < 16 || dynstr_section >= shnum {
        return out;
    }
    if dynamic_offset.saturating_add(dynamic_size) > raw.len() {
        return out;
    }

    let dynstr_off = shoff + dynstr_section.saturating_mul(shentsize);
    let dynstr_offset = match read_u64_le_at(raw, dynstr_off + 24) {
        Some(v) => v as usize,
        None => return out,
    };
    let dynstr_size = match read_u64_le_at(raw, dynstr_off + 32) {
        Some(v) => v as usize,
        None => return out,
    };
    if dynstr_offset == 0 || dynstr_size == 0 || dynstr_offset.saturating_add(dynstr_size) > raw.len() {
        return out;
    }

    let mut off = dynamic_offset;
    let end = dynamic_offset + dynamic_size;
    while off + 16 <= end {
        let tag = match read_u64_le_at(raw, off) {
            Some(v) => v,
            None => break,
        };
        let value = match read_u64_le_at(raw, off + 8) {
            Some(v) => v,
            None => break,
        };
        if tag == DT_NULL {
            break;
        }
        if tag == DT_NEEDED {
            if let Some(name) = dynstr_symbol_name(raw, dynstr_offset, dynstr_size, value as u32) {
                let trimmed = name.trim().to_string();
                if !trimmed.is_empty() && !out.iter().any(|existing| existing == &trimmed) {
                    out.push(trimmed);
                }
            }
        }
        off += 16;
    }

    out
}

fn append_copy_dt_needed_entries(state: &mut LinkerState, resolved: &str) {
    if !state.link_copy_dt_needed_entries {
        return;
    }
    let resolved_trimmed = resolved.trim();
    if resolved_trimmed.is_empty() {
        return;
    }
    let path = Path::new(resolved_trimmed);
    let raw = match fs::read(path) {
        Ok(v) => v,
        Err(_) => return,
    };
    if probe_object_format_bytes(&raw) != OBJECT_FORMAT_ELF64 {
        return;
    }
    for needed in parse_elf_needed_libraries(&raw) {
        add_needed_library_if_missing(state, &needed);
    }
}

fn parse_defsym_spec(text: &str) -> Option<(u64, u64)> {
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }
    let (name_raw, value_raw) = trimmed.split_once('=')?;
    let name = name_raw.trim();
    if name.is_empty() {
        return None;
    }
    let value = parse_u64_auto(value_raw.trim())?;
    Some((fnv1a64(name), value))
}

fn parse_defsym_or_symbol_rhs(state: &LinkerState, text: &str) -> Option<(u64, u64)> {
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }
    let (name_raw, value_raw) = trimmed.split_once('=')?;
    let name = name_raw.trim();
    if name.is_empty() {
        return None;
    }
    let rhs = value_raw.trim();
    let value = if let Some(parsed) = parse_u64_auto(rhs) {
        parsed
    } else {
        let hash = fnv1a64(rhs);
        state
            .globals
            .get(&hash)
            .filter(|g| g.defined == 1)
            .map(|g| g.address)?
    };
    Some((fnv1a64(name), value))
}

fn tokenize_script_args(value: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut current = String::new();
    let mut depth = 0u32;
    let mut in_string = false;
    let mut quote = '\0';
    let mut escape = false;

    for ch in value.chars() {
        if in_string {
            current.push(ch);
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == quote {
                in_string = false;
            }
            continue;
        }

        if ch == '"' || ch == '\'' {
            in_string = true;
            quote = ch;
            current.push(ch);
            continue;
        }

        if ch == '(' {
            depth = depth.saturating_add(1);
            current.push(ch);
            continue;
        }
        if ch == ')' {
            depth = depth.saturating_sub(1);
            current.push(ch);
            continue;
        }

        if (ch == ',' || ch.is_ascii_whitespace()) && depth == 0 {
            let token = trim_quotes(current.trim());
            if !token.is_empty() {
                out.push(token.to_string());
            }
            current.clear();
            continue;
        }

        current.push(ch);
    }

    let tail = trim_quotes(current.trim());
    if !tail.is_empty() {
        out.push(tail.to_string());
    }
    out
}

fn parse_target_value(raw: &str) -> Option<u32> {
    let normalized = raw.trim().to_ascii_lowercase();
    match normalized.as_str() {
        "x86_64-linux"
        | "x86_64-linux-gnu"
        | "x86_64-unknown-linux-gnu"
        | "x86_64-linux-musl"
        | "x86_64-unknown-linux-musl"
        | "x86_64-alpine-linux-musl"
        | "elf_x86_64"
        | "elf64-x86-64" => Some(TARGET_X86_64_LINUX),
        "aarch64-linux"
        | "aarch64-linux-gnu"
        | "aarch64-unknown-linux-gnu"
        | "aarch64-linux-musl"
        | "aarch64-unknown-linux-musl"
        | "elf64-littleaarch64"
        | "aarch64elf" => Some(TARGET_AARCH64_LINUX),
        "x86_64-windows-msvc"
        | "x86_64-pc-windows-msvc"
        | "x86_64-windows-gnu"
        | "x86_64-pc-windows-gnu"
        | "i386pep"
        | "pep"
        | "pe-x86-64" => Some(TARGET_X86_64_WINDOWS),
        "aarch64-windows-msvc"
        | "aarch64-pc-windows-msvc"
        | "aarch64-windows-gnu"
        | "aarch64-pc-windows-gnu"
        | "arm64pe" => Some(TARGET_AARCH64_WINDOWS),
        "x86_64-apple-darwin"
        | "x86_64-macos"
        | "mach_o_x86_64"
        | "macho-x86-64" => Some(TARGET_X86_64_MACOS),
        "aarch64-apple-darwin" | "arm64-apple-darwin" | "arm64-macos" | "macho-arm64" => {
            Some(TARGET_AARCH64_MACOS)
        }
        "x86_64-none" | "x86_64-unknown-none-elf" | "x86_64-pc-none-elf" => Some(TARGET_NONE),
        "aarch64-none" | "aarch64-unknown-none-elf" | "aarch64-pc-none-elf" => {
            Some(TARGET_AARCH64_LINUX)
        }
        _ => None,
    }
}

fn validate_named_block_statement(statement: &str, directive: &str) -> bool {
    let trimmed = statement.trim();
    let upper = trimmed.to_ascii_uppercase();
    if !upper.starts_with(directive) {
        return false;
    }
    let open = match trimmed.find('{') {
        Some(v) => v,
        None => return false,
    };
    let close = match trimmed.rfind('}') {
        Some(v) => v,
        None => return false,
    };
    close > open
}

fn parse_hash_style_value(raw: &str) -> Option<u32> {
    let normalized = raw.trim().to_ascii_lowercase();
    match normalized.as_str() {
        "none" => Some(HASH_STYLE_NONE),
        "sysv" => Some(HASH_STYLE_SYSV),
        "gnu" => Some(HASH_STYLE_GNU),
        "both" => Some(HASH_STYLE_BOTH),
        _ => None,
    }
}

fn parse_hex_bytes(raw: &str) -> Option<Vec<u8>> {
    let body = raw
        .strip_prefix("0x")
        .or_else(|| raw.strip_prefix("0X"))
        .unwrap_or(raw)
        .trim();
    if body.is_empty() {
        return Some(Vec::new());
    }
    let mut text = body.to_string();
    if text.len() % 2 != 0 {
        text.insert(0, '0');
    }
    let mut out = Vec::new();
    let bytes = text.as_bytes();
    let mut i = 0usize;
    while i + 1 < bytes.len() {
        let pair = std::str::from_utf8(&bytes[i..i + 2]).ok()?;
        let value = u8::from_str_radix(pair, 16).ok()?;
        out.push(value);
        i += 2;
    }
    Some(out)
}

fn eval_script_atom(state: &LinkerState, raw: &str) -> Option<u64> {
    let token = trim_quotes(raw.trim());
    if token.is_empty() {
        return None;
    }
    if token == "." {
        return Some(state.image_base);
    }
    if let Some(v) = parse_u64_scaled(token) {
        return Some(v);
    }
    let upper = token.to_ascii_uppercase();
    if upper.starts_with("ORIGIN(") && token.ends_with(')') {
        return Some(state.memory_origin);
    }
    if upper.starts_with("LENGTH(") && token.ends_with(')') {
        return Some(state.memory_length);
    }
    if upper.starts_with("ADDR(") && token.ends_with(')') {
        return Some(state.image_base);
    }
    if upper.starts_with("LOADADDR(") && token.ends_with(')') {
        return Some(state.pending_elf_image_base.max(state.image_base));
    }
    if upper.starts_with("SIZEOF(") && token.ends_with(')') {
        return Some(state.image_size as u64);
    }
    if upper.starts_with("ALIGN(") && token.ends_with(')') {
        let inner = &token[6..token.len().saturating_sub(1)];
        let align = eval_script_expr(state, inner)?;
        if align == 0 {
            return Some(state.image_base);
        }
        return Some(align_up(state.image_base, align));
    }
    state.globals.get(&fnv1a64(token)).map(|g| g.address)
}

fn strip_wrapping_parens(raw: &str) -> Option<&str> {
    let text = raw.trim();
    if !(text.starts_with('(') && text.ends_with(')')) {
        return None;
    }
    let mut depth = 0u32;
    for (idx, ch) in text.char_indices() {
        if ch == '(' {
            depth = depth.saturating_add(1);
        } else if ch == ')' {
            if depth == 0 {
                return None;
            }
            depth -= 1;
            if depth == 0 && idx + ch.len_utf8() < text.len() {
                return None;
            }
        }
    }
    if depth == 0 && text.len() >= 2 {
        Some(text[1..text.len() - 1].trim())
    } else {
        None
    }
}

fn find_last_top_level_char_operator(text: &str, ops: &[char]) -> Option<(usize, char)> {
    let mut depth = 0u32;
    let mut in_string = false;
    let mut quote = '\0';
    let mut escape = false;
    let mut candidate: Option<(usize, char)> = None;

    for (idx, ch) in text.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == quote {
                in_string = false;
            }
            continue;
        }
        if ch == '"' || ch == '\'' {
            in_string = true;
            quote = ch;
            continue;
        }
        if ch == '(' {
            depth = depth.saturating_add(1);
            continue;
        }
        if ch == ')' {
            if depth > 0 {
                depth -= 1;
            }
            continue;
        }
        if depth != 0 {
            continue;
        }
        if !ops.contains(&ch) {
            continue;
        }
        let lhs = text[..idx].trim();
        if lhs.is_empty() {
            continue;
        }
        candidate = Some((idx, ch));
    }
    candidate
}

fn find_last_top_level_shift_operator(text: &str) -> Option<(usize, usize, bool)> {
    let bytes = text.as_bytes();
    let mut idx = 0usize;
    let mut depth = 0u32;
    let mut in_string = false;
    let mut quote = b'\0';
    let mut escape = false;
    let mut candidate: Option<(usize, usize, bool)> = None;

    while idx < bytes.len() {
        let ch = bytes[idx];
        if in_string {
            if escape {
                escape = false;
                idx += 1;
                continue;
            }
            if ch == b'\\' {
                escape = true;
                idx += 1;
                continue;
            }
            if ch == quote {
                in_string = false;
            }
            idx += 1;
            continue;
        }
        if ch == b'"' || ch == b'\'' {
            in_string = true;
            quote = ch;
            idx += 1;
            continue;
        }
        if ch == b'(' {
            depth = depth.saturating_add(1);
            idx += 1;
            continue;
        }
        if ch == b')' {
            if depth > 0 {
                depth -= 1;
            }
            idx += 1;
            continue;
        }
        if depth == 0 && idx + 1 < bytes.len() {
            if bytes[idx] == b'<' && bytes[idx + 1] == b'<' {
                let lhs = text[..idx].trim();
                if !lhs.is_empty() {
                    candidate = Some((idx, 2, true));
                }
                idx += 2;
                continue;
            }
            if bytes[idx] == b'>' && bytes[idx + 1] == b'>' {
                let lhs = text[..idx].trim();
                if !lhs.is_empty() {
                    candidate = Some((idx, 2, false));
                }
                idx += 2;
                continue;
            }
        }
        idx += 1;
    }
    candidate
}

fn eval_script_primary(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some(inner) = strip_wrapping_parens(text) {
        return eval_script_expr(state, inner);
    }
    eval_script_atom(state, text)
}

fn eval_script_unary(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some(rest) = text.strip_prefix('+') {
        return eval_script_unary(state, rest);
    }
    if let Some(rest) = text.strip_prefix('-') {
        let rhs = eval_script_unary(state, rest)?;
        return Some(0u64.wrapping_sub(rhs));
    }
    if let Some(rest) = text.strip_prefix('~') {
        let rhs = eval_script_unary(state, rest)?;
        return Some(!rhs);
    }
    eval_script_primary(state, text)
}

fn eval_script_mul_div_mod(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some((idx, op)) = find_last_top_level_char_operator(text, &['*', '/', '%']) {
        let lhs = eval_script_mul_div_mod(state, &text[..idx])?;
        let rhs = eval_script_unary(state, &text[idx + op.len_utf8()..])?;
        return match op {
            '*' => Some(lhs.wrapping_mul(rhs)),
            '/' => {
                if rhs == 0 {
                    None
                } else {
                    Some(lhs / rhs)
                }
            }
            '%' => {
                if rhs == 0 {
                    None
                } else {
                    Some(lhs % rhs)
                }
            }
            _ => None,
        };
    }
    eval_script_unary(state, text)
}

fn eval_script_add_sub(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some((idx, op)) = find_last_top_level_char_operator(text, &['+', '-']) {
        let lhs = eval_script_add_sub(state, &text[..idx])?;
        let rhs = eval_script_mul_div_mod(state, &text[idx + op.len_utf8()..])?;
        return if op == '+' {
            Some(lhs.wrapping_add(rhs))
        } else {
            Some(lhs.wrapping_sub(rhs))
        };
    }
    eval_script_mul_div_mod(state, text)
}

fn eval_script_shift(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some((idx, len, is_left)) = find_last_top_level_shift_operator(text) {
        let lhs = eval_script_shift(state, &text[..idx])?;
        let rhs = eval_script_add_sub(state, &text[idx + len..])?;
        let shift = (rhs & 63) as u32;
        return if is_left {
            Some(lhs.wrapping_shl(shift))
        } else {
            Some(lhs.wrapping_shr(shift))
        };
    }
    eval_script_add_sub(state, text)
}

fn eval_script_bit_and(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some((idx, _op)) = find_last_top_level_char_operator(text, &['&']) {
        let lhs = eval_script_bit_and(state, &text[..idx])?;
        let rhs = eval_script_shift(state, &text[idx + 1..])?;
        return Some(lhs & rhs);
    }
    eval_script_shift(state, text)
}

fn eval_script_bit_xor(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some((idx, _op)) = find_last_top_level_char_operator(text, &['^']) {
        let lhs = eval_script_bit_xor(state, &text[..idx])?;
        let rhs = eval_script_bit_and(state, &text[idx + 1..])?;
        return Some(lhs ^ rhs);
    }
    eval_script_bit_and(state, text)
}

fn eval_script_bit_or(state: &LinkerState, raw: &str) -> Option<u64> {
    let text = raw.trim();
    if text.is_empty() {
        return None;
    }
    if let Some((idx, _op)) = find_last_top_level_char_operator(text, &['|']) {
        let lhs = eval_script_bit_or(state, &text[..idx])?;
        let rhs = eval_script_bit_xor(state, &text[idx + 1..])?;
        return Some(lhs | rhs);
    }
    eval_script_bit_xor(state, text)
}

fn eval_script_expr(state: &LinkerState, raw: &str) -> Option<u64> {
    eval_script_bit_or(state, raw)
}

fn parse_memory_keyword_value(state: &LinkerState, statement: &str, keyword: &str) -> Option<u64> {
    let upper = statement.to_ascii_uppercase();
    let key_upper = keyword.to_ascii_uppercase();
    let pos = upper.find(&key_upper)?;
    let mut tail = statement[pos + keyword.len()..].trim_start();
    if tail.starts_with('=') {
        tail = tail[1..].trim_start();
    }
    let mut depth = 0u32;
    let mut end = tail.len();
    for (idx, ch) in tail.char_indices() {
        if ch == '(' {
            depth = depth.saturating_add(1);
            continue;
        }
        if ch == ')' {
            if depth == 0 {
                end = idx;
                break;
            }
            depth -= 1;
            continue;
        }
        if depth == 0 && (ch == ',' || ch == '}' || ch == ';') {
            end = idx;
            break;
        }
    }
    let token = tail[..end].trim();
    eval_script_expr(state, token)
}

fn parse_sections_location_assignment(state: &LinkerState, statement: &str) -> Option<u64> {
    let t = statement.trim();
    if t.is_empty() {
        return None;
    }
    let bytes = t.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if bytes[idx] == b'.' {
            let mut cursor = idx + 1;
            while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
                cursor += 1;
            }
            if cursor < bytes.len() && bytes[cursor] == b'=' {
                let mut rhs = t[cursor + 1..].trim();
                rhs = rhs.split('{').next().unwrap_or(rhs).trim();
                rhs = rhs.split('}').next().unwrap_or(rhs).trim();
                rhs = rhs.split(',').next().unwrap_or(rhs).trim();
                return eval_script_expr(state, rhs);
            }
        }
        idx += 1;
    }
    None
}

fn parse_sections_output_address(state: &LinkerState, statement: &str) -> Option<u64> {
    let t = statement.trim();
    if t.is_empty() {
        return None;
    }
    let bytes = t.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if bytes[idx] == b'.' {
            let mut cursor = idx + 1;
            while cursor < bytes.len() {
                let ch = bytes[cursor];
                if ch.is_ascii_alphanumeric() || ch == b'_' || ch == b'.' {
                    cursor += 1;
                } else {
                    break;
                }
            }
            while cursor < bytes.len() && bytes[cursor].is_ascii_whitespace() {
                cursor += 1;
            }
            if cursor < bytes.len() && bytes[cursor] == b'=' {
                idx += 1;
                continue;
            }
            if cursor < bytes.len() && bytes[cursor] == b':' {
                idx += 1;
                continue;
            }

            let tail = t[cursor..].trim_start();
            let before_colon = tail.split(':').next().unwrap_or("").trim();
            if !before_colon.is_empty() {
                if let Some(value) = eval_script_expr(state, before_colon) {
                    return Some(value);
                }
            }
        }
        idx += 1;
    }
    None
}

fn parse_sections_load_address(state: &LinkerState, statement: &str) -> Option<u64> {
    let upper = statement.to_ascii_uppercase();
    let pos = upper.find("AT(")?;
    let raw = &statement[pos + 3..];
    let mut depth = 1u32;
    let mut end = None;
    for (idx, ch) in raw.char_indices() {
        if ch == '(' {
            depth = depth.saturating_add(1);
        } else if ch == ')' {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                end = Some(idx);
                break;
            }
        }
    }
    let close = end?;
    let expr = raw[..close].trim();
    eval_script_expr(state, expr)
}

fn split_assert_args(raw: &str) -> (String, String) {
    let mut paren_depth = 0u32;
    for (idx, ch) in raw.char_indices() {
        if ch == '(' {
            paren_depth = paren_depth.saturating_add(1);
            continue;
        }
        if ch == ')' {
            if paren_depth > 0 {
                paren_depth -= 1;
            }
            continue;
        }
        if ch == ',' && paren_depth == 0 {
            let lhs = raw[..idx].trim().to_string();
            let rhs = trim_quotes(raw[idx + 1..].trim()).to_string();
            return (lhs, rhs);
        }
    }
    (raw.trim().to_string(), String::new())
}

fn eval_script_condition(state: &LinkerState, raw: &str) -> Option<bool> {
    let expr = raw.trim();
    if expr.is_empty() {
        return None;
    }
    for op in ["==", "!=", ">=", "<=", ">", "<"] {
        if let Some(pos) = expr.find(op) {
            let lhs = eval_script_expr(state, expr[..pos].trim())?;
            let rhs = eval_script_expr(state, expr[pos + op.len()..].trim())?;
            return Some(match op {
                "==" => lhs == rhs,
                "!=" => lhs != rhs,
                ">=" => lhs >= rhs,
                "<=" => lhs <= rhs,
                ">" => lhs > rhs,
                "<" => lhs < rhs,
                _ => false,
            });
        }
    }
    eval_script_expr(state, expr).map(|v| v != 0)
}

fn parse_sections_region_assignment(statement: &str) -> Option<String> {
    let t = statement.trim();
    if t.is_empty() {
        return None;
    }
    let pos = t.rfind('>')?;
    let tail = t[pos + 1..].trim_start();
    if tail.is_empty() {
        return None;
    }
    let mut end = tail.len();
    for (idx, ch) in tail.char_indices() {
        if !(ch.is_ascii_alphanumeric() || ch == '_' || ch == '.') {
            end = idx;
            break;
        }
    }
    if end == 0 {
        return None;
    }
    let region = tail[..end].trim();
    if region.is_empty() {
        None
    } else {
        Some(region.to_string())
    }
}

fn script_head_identifier(statement: &str) -> Option<String> {
    let text = statement.trim_start();
    if text.is_empty() {
        return None;
    }
    let mut chars = text.char_indices();
    let (_, first) = chars.next()?;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    let mut end = first.len_utf8();
    for (idx, ch) in chars {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            end = idx + ch.len_utf8();
        } else {
            break;
        }
    }
    Some(text[..end].to_string())
}

fn is_supported_script_directive(head: &str) -> bool {
    matches!(
        head,
        "ENTRY"
            | "OUTPUT"
            | "OUTPUT_FORMAT"
            | "OUTPUT_ARCH"
            | "TARGET"
            | "SEARCH_DIR"
            | "EXTERN"
            | "PROVIDE"
            | "PROVIDE_HIDDEN"
            | "INPUT"
            | "GROUP"
            | "AS_NEEDED"
            | "NO_AS_NEEDED"
            | "INCLUDE"
            | "ASSERT"
            | "MEMORY"
            | "SECTIONS"
            | "PHDRS"
            | "VERSION"
    )
}

fn parse_script_symbol_assignment(statement: &str) -> Option<(String, String)> {
    let text = statement.trim();
    if text.is_empty() {
        return None;
    }
    let mut depth_paren = 0u32;
    let mut depth_brace = 0u32;
    let mut in_string = false;
    let mut quote = '\0';
    let mut escape = false;
    let mut eq_index: Option<usize> = None;
    for (idx, ch) in text.char_indices() {
        if in_string {
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == quote {
                in_string = false;
            }
            continue;
        }
        if ch == '"' || ch == '\'' {
            in_string = true;
            quote = ch;
            continue;
        }
        if ch == '(' {
            depth_paren = depth_paren.saturating_add(1);
            continue;
        }
        if ch == ')' {
            if depth_paren > 0 {
                depth_paren -= 1;
            }
            continue;
        }
        if ch == '{' {
            depth_brace = depth_brace.saturating_add(1);
            continue;
        }
        if ch == '}' {
            if depth_brace > 0 {
                depth_brace -= 1;
            }
            continue;
        }
        if depth_paren == 0 && depth_brace == 0 && ch == '=' {
            eq_index = Some(idx);
            break;
        }
    }
    let idx = eq_index?;
    let lhs = text[..idx].trim();
    let rhs = text[idx + 1..].trim();
    if lhs.is_empty() || rhs.is_empty() || lhs == "." {
        return None;
    }
    let mut lhs_chars = lhs.chars();
    let first = lhs_chars.next()?;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    if !lhs_chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_') {
        return None;
    }
    Some((lhs.to_string(), rhs.to_string()))
}

fn split_script_statements(cleaned: &str) -> Vec<String> {
    let mut statements: Vec<String> = Vec::new();
    let mut current = String::new();
    let mut brace_depth = 0u32;
    let mut paren_depth = 0u32;
    let mut in_string = false;
    let mut quote = '\0';
    let mut escape = false;

    for ch in cleaned.chars() {
        if in_string {
            current.push(ch);
            if escape {
                escape = false;
                continue;
            }
            if ch == '\\' {
                escape = true;
                continue;
            }
            if ch == quote {
                in_string = false;
            }
            continue;
        }

        if ch == '"' || ch == '\'' {
            in_string = true;
            quote = ch;
            current.push(ch);
            continue;
        }

        if ch == '{' {
            brace_depth = brace_depth.saturating_add(1);
            current.push(ch);
            continue;
        }
        if ch == '}' {
            brace_depth = brace_depth.saturating_sub(1);
            current.push(ch);
            continue;
        }
        if ch == '(' {
            paren_depth = paren_depth.saturating_add(1);
            current.push(ch);
            continue;
        }
        if ch == ')' {
            paren_depth = paren_depth.saturating_sub(1);
            current.push(ch);
            continue;
        }

        if ch == ';' {
            if brace_depth == 0 && paren_depth == 0 {
                let stmt = current.trim();
                if !stmt.is_empty() {
                    statements.push(stmt.to_string());
                }
                current.clear();
            } else {
                current.push(ch);
            }
            continue;
        }

        if ch == '\n' && brace_depth == 0 && paren_depth == 0 {
            let stmt = current.trim();
            if !stmt.is_empty() {
                statements.push(stmt.to_string());
            }
            current.clear();
            continue;
        }

        current.push(ch);
    }

    let tail = current.trim();
    if !tail.is_empty() {
        statements.push(tail.to_string());
    }
    statements
}

fn parse_build_id_mode_and_bytes(raw: &str) -> Option<(u32, Vec<u8>)> {
    let normalized = raw.trim().to_ascii_lowercase();
    if normalized.is_empty() || normalized == "fast" {
        return Some((BUILD_ID_MODE_FAST, Vec::new()));
    }
    if normalized == "none" {
        return Some((BUILD_ID_MODE_NONE, Vec::new()));
    }
    if normalized == "md5" {
        return Some((BUILD_ID_MODE_MD5, Vec::new()));
    }
    if normalized == "sha1" {
        return Some((BUILD_ID_MODE_SHA1, Vec::new()));
    }
    if normalized == "uuid" {
        return Some((BUILD_ID_MODE_UUID, Vec::new()));
    }
    let bytes = parse_hex_bytes(raw)?;
    Some((BUILD_ID_MODE_HEX, bytes))
}

fn fnv1a64_seeded(text: &[u8], seed: u64) -> u64 {
    const PRIME: u64 = 0x100000001b3;
    let mut hash = seed;
    for b in text {
        hash ^= *b as u64;
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}

fn make_pseudo_digest(payload: &[u8], size: usize, salt: u64) -> Vec<u8> {
    let mut out = Vec::with_capacity(size);
    let mut seed = 0xcbf29ce484222325u64 ^ salt;
    while out.len() < size {
        let hash = fnv1a64_seeded(payload, seed).to_le_bytes();
        out.extend_from_slice(&hash);
        seed = seed.wrapping_mul(0x100000001b3).wrapping_add(0x9e3779b97f4a7c15);
    }
    out.truncate(size);
    out
}

fn build_id_payload(state: &LinkerState, image_payload: &[u8]) -> Vec<u8> {
    match state.build_id_mode {
        BUILD_ID_MODE_NONE => Vec::new(),
        BUILD_ID_MODE_FAST => make_pseudo_digest(image_payload, 8, 0x11),
        BUILD_ID_MODE_MD5 => make_pseudo_digest(image_payload, 16, 0x22),
        BUILD_ID_MODE_SHA1 => make_pseudo_digest(image_payload, 20, 0x33),
        BUILD_ID_MODE_UUID => {
            let mut uuid = make_pseudo_digest(image_payload, 16, 0x44);
            if uuid.len() == 16 {
                uuid[6] = (uuid[6] & 0x0f) | 0x40;
                uuid[8] = (uuid[8] & 0x3f) | 0x80;
            }
            uuid
        }
        BUILD_ID_MODE_HEX => state.build_id_bytes.clone(),
        _ => Vec::new(),
    }
}

fn append_build_id_note(out: &mut Vec<u8>, build_id: &[u8]) {
    if build_id.is_empty() {
        return;
    }
    let namesz = 4u32;
    let descsz = build_id.len().min(u32::MAX as usize) as u32;
    let note_type = 3u32;
    out.extend_from_slice(&namesz.to_le_bytes());
    out.extend_from_slice(&descsz.to_le_bytes());
    out.extend_from_slice(&note_type.to_le_bytes());
    out.extend_from_slice(b"GNU\0");
    while out.len() % 4 != 0 {
        out.push(0);
    }
    out.extend_from_slice(build_id);
    while out.len() % 4 != 0 {
        out.push(0);
    }
}

fn resolve_script_path(base_dir: &Path, raw: &str) -> PathBuf {
    let token = trim_quotes(raw).trim();
    let p = PathBuf::from(token);
    if p.is_absolute() {
        p
    } else {
        base_dir.join(p)
    }
}

fn resolve_search_dir_path(state: &LinkerState, base_dir: &Path, raw: &str) -> PathBuf {
    let token = trim_quotes(raw).trim();
    if let Some(rest) = token.strip_prefix('=') {
        let trimmed = rest.trim();
        if state.sysroot_path != 0 {
            if let Some(sysroot) = to_path(state.sysroot_path) {
                let suffix = trimmed.trim_start_matches('/');
                if suffix.is_empty() {
                    return sysroot;
                }
                return sysroot.join(suffix);
            }
        }
        if trimmed.is_empty() {
            return PathBuf::from(".");
        }
        return PathBuf::from(trimmed);
    }
    resolve_script_path(base_dir, token)
}

fn apply_script_input_token(state: &mut LinkerState, script_dir: &Path, token: &str) -> u32 {
    let trimmed = trim_quotes(token).trim();
    if trimmed.is_empty() {
        return ERR_OK;
    }

    if let Some(path_token) = trimmed.strip_prefix("-L") {
        if !path_token.trim().is_empty() {
            let path = resolve_search_dir_path(state, script_dir, path_token);
            state
                .search_paths
                .push(intern_string(path.to_string_lossy().to_string()));
        }
        return ERR_OK;
    }

    if let Some(lib_name) = trimmed.strip_prefix("-l") {
        let lib = lib_name.trim();
        if lib.is_empty() {
            return ERR_INVALID_FORMAT;
        }
        if !state.link_static {
            let normalized = if lib.contains('.') {
                lib.to_string()
            } else {
                format!("lib{}.so", lib)
            };
            if !state.link_as_needed {
                add_needed_library_if_missing(state, &normalized);
            }
        }
        state.inputs.push(intern_string(trimmed));
        return ERR_OK;
    }

    let path = resolve_script_path(script_dir, trimmed);
    state
        .inputs
        .push(intern_string(path.to_string_lossy().to_string()));
    ERR_OK
}

fn apply_script_input_list(state: &mut LinkerState, script_dir: &Path, arg: &str) -> u32 {
    for token in tokenize_script_args(arg) {
        let status = apply_script_input_token(state, script_dir, &token);
        if status != ERR_OK {
            return status;
        }
    }
    ERR_OK
}

fn require_symbol_hash(state: &mut LinkerState, hash: u64) {
    if hash == 0 {
        return;
    }
    if !state.required_symbols.contains(&hash) {
        state.required_symbols.push(hash);
    }
}

fn reset_linker_state_defaults(state: &mut LinkerState) {
    *state = LinkerState::default();
    state.output_path = DEFAULT_OUTPUT_HANDLE;
    state.output_format = FORMAT_ELF64;
    state.target = TARGET_NONE;
    state.entry = 0x0010_0000;
    state.image_base = 0x0001_0000;
    state.link_as_needed = false;
    state.whole_archive = false;
    state.link_new_dtags = true;
    state.link_copy_dt_needed_entries = false;
    state.no_undefined = false;
    state.sysroot_path = 0;
    state.link_pie = false;
    state.link_shared = false;
    state.link_static = false;
    state.hash_style = HASH_STYLE_BOTH;
    state.thread_count = 0;
    state.emit_eh_frame_hdr = true;
    state.fatal_warnings = false;
    state.color_diagnostics = false;
    state.print_gc_sections = false;
    state.icf_mode = ICF_MODE_NONE;
    state.pe_no_entry = false;
    state.pe_dynamic_base = true;
    state.pe_nx_compat = true;
    state.pe_large_address_aware = true;
    state.dependency_file = 0;
    state.emit_relocs = false;
}

fn probe_object_format_bytes(raw: &[u8]) -> u32 {
    if raw.len() >= 4 && &raw[0..4] == b"\x7fELF" {
        return OBJECT_FORMAT_ELF64;
    }

    if raw.len() >= 4 {
        let magic = u32::from_le_bytes([raw[0], raw[1], raw[2], raw[3]]);
        if magic == 0xfeedfacf || magic == 0xcffaedfe {
            return OBJECT_FORMAT_MACHO64;
        }
    }

    if raw.len() >= 2 {
        let machine = u16::from_le_bytes([raw[0], raw[1]]);
        if machine == COFF_MACHINE_X86_64 || machine == COFF_MACHINE_AARCH64 {
            return OBJECT_FORMAT_COFF64;
        }
    }

    OBJECT_FORMAT_UNKNOWN
}

fn probe_object_format_path(path: &Path) -> u32 {
    let raw = match fs::read(path) {
        Ok(v) => v,
        Err(_) => return OBJECT_FORMAT_UNKNOWN,
    };
    probe_object_format_bytes(&raw)
}

fn dynstr_symbol_name(raw: &[u8], dynstr_off: usize, dynstr_size: usize, st_name: u32) -> Option<String> {
    let name_off = st_name as usize;
    if name_off >= dynstr_size {
        return None;
    }
    let start = dynstr_off.checked_add(name_off)?;
    let end_limit = dynstr_off.checked_add(dynstr_size)?;
    if start >= raw.len() || start >= end_limit {
        return None;
    }
    let mut end = start;
    while end < end_limit && end < raw.len() {
        if raw[end] == 0 {
            break;
        }
        end += 1;
    }
    if end <= start {
        return None;
    }
    Some(String::from_utf8_lossy(&raw[start..end]).to_string())
}

fn add_shared_global_symbol(state: &mut LinkerState, name: &str) {
    let trimmed = name.trim();
    if trimmed.is_empty() {
        return;
    }
    let name_hash = fnv1a64(trimmed);
    let should_insert = match state.globals.get(&name_hash) {
        Some(existing) if existing.defined == 1 && existing.bind == 1 => false,
        _ => true,
    };
    if should_insert {
        state.globals.insert(
            name_hash,
            GlobalSymbol {
                object_index: 0,
                symbol_index: 0,
                bind: 2,
                defined: 1,
                address: 0,
            },
        );
    }
}

fn read_c_string_from_bytes(raw: &[u8], start: usize) -> Option<String> {
    if start >= raw.len() {
        return None;
    }
    let mut end = start;
    while end < raw.len() {
        if raw[end] == 0 {
            break;
        }
        end += 1;
    }
    if end <= start {
        return None;
    }
    Some(String::from_utf8_lossy(&raw[start..end]).to_string())
}

fn pe_rva_to_file_offset(raw: &[u8], section_table_off: usize, section_count: usize, rva: u32) -> Option<usize> {
    for i in 0..section_count {
        let sec_off = section_table_off.checked_add(i.saturating_mul(40))?;
        if sec_off + 40 > raw.len() {
            return None;
        }
        let virtual_size = read_u32_le_at(raw, sec_off + 8)?;
        let virtual_address = read_u32_le_at(raw, sec_off + 12)?;
        let size_of_raw_data = read_u32_le_at(raw, sec_off + 16)?;
        let pointer_to_raw_data = read_u32_le_at(raw, sec_off + 20)?;
        let span = if virtual_size > size_of_raw_data {
            virtual_size
        } else {
            size_of_raw_data
        };
        if span == 0 {
            continue;
        }
        if rva >= virtual_address {
            let delta = rva - virtual_address;
            if delta < span {
                let file_off = (pointer_to_raw_data as usize).checked_add(delta as usize)?;
                if file_off < raw.len() {
                    return Some(file_off);
                }
            }
        }
    }
    None
}

fn ingest_shared_elf_symbols(state: &mut LinkerState, raw: &[u8]) -> u32 {
    if raw.len() < 64 || &raw[0..4] != b"\x7fELF" {
        return ERR_INVALID_FORMAT;
    }
    if raw.get(4).copied().unwrap_or(0) != 2 || raw.get(5).copied().unwrap_or(0) != 1 {
        return ERR_INVALID_FORMAT;
    }

    let shoff = match read_u64_le_at(raw, 40) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let shentsize = match read_u16_le_at(raw, 58) {
        Some(v) if v >= 64 => v as usize,
        _ => return ERR_INVALID_FORMAT,
    };
    let shnum = match read_u16_le_at(raw, 60) {
        Some(v) if v > 0 => v as usize,
        _ => return ERR_INVALID_FORMAT,
    };

    let mut dynsym_index: Option<usize> = None;
    for index in 0..shnum {
        let base = match shoff.checked_add(index.saturating_mul(shentsize)) {
            Some(v) => v,
            None => return ERR_INVALID_FORMAT,
        };
        let sh_type = match read_u32_le_at(raw, base + 4) {
            Some(v) => v,
            None => return ERR_INVALID_FORMAT,
        };
        if sh_type == SHT_DYNSYM {
            dynsym_index = Some(index);
            break;
        }
    }

    let dynsym_index = match dynsym_index {
        Some(v) => v,
        None => return ERR_OK,
    };
    let dynsym_base = match shoff.checked_add(dynsym_index.saturating_mul(shentsize)) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let dynsym_off = match read_u64_le_at(raw, dynsym_base + 24) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let dynsym_size = match read_u64_le_at(raw, dynsym_base + 32) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let dynsym_link = match read_u32_le_at(raw, dynsym_base + 40) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let dynsym_entsize = match read_u64_le_at(raw, dynsym_base + 56) {
        Some(v) if v >= 24 => v as usize,
        _ => return ERR_INVALID_FORMAT,
    };

    if dynsym_off.saturating_add(dynsym_size) > raw.len() {
        return ERR_INVALID_FORMAT;
    }
    if dynsym_link >= shnum {
        return ERR_INVALID_FORMAT;
    }

    let dynstr_base = match shoff.checked_add(dynsym_link.saturating_mul(shentsize)) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let dynstr_off = match read_u64_le_at(raw, dynstr_base + 24) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let dynstr_size = match read_u64_le_at(raw, dynstr_base + 32) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    if dynstr_off.saturating_add(dynstr_size) > raw.len() {
        return ERR_INVALID_FORMAT;
    }

    let symbol_count = dynsym_size / dynsym_entsize;
    for index in 0..symbol_count {
        let sym_off = dynsym_off + index.saturating_mul(dynsym_entsize);
        let st_name = match read_u32_le_at(raw, sym_off) {
            Some(v) if v != 0 => v,
            _ => continue,
        };
        let st_info = *raw.get(sym_off + 4).unwrap_or(&0);
        let st_bind = st_info >> 4;
        if st_bind != 1 && st_bind != 2 {
            continue;
        }
        let st_shndx = match read_u16_le_at(raw, sym_off + 6) {
            Some(v) => v,
            None => continue,
        };
        if st_shndx == SHN_UNDEF {
            continue;
        }
        let name = match dynstr_symbol_name(raw, dynstr_off, dynstr_size, st_name) {
            Some(v) if !v.is_empty() => v,
            _ => continue,
        };
        add_shared_global_symbol(state, &name);
    }

    ERR_OK
}

fn ingest_shared_pe_symbols(state: &mut LinkerState, raw: &[u8]) -> u32 {
    if raw.len() < 0x40 || raw.get(0).copied() != Some(b'M') || raw.get(1).copied() != Some(b'Z') {
        return ERR_INVALID_FORMAT;
    }
    let pe_offset = match read_u32_le_at(raw, 0x3c) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    if pe_offset + 24 > raw.len() {
        return ERR_INVALID_FORMAT;
    }
    if &raw[pe_offset..pe_offset + 4] != b"PE\0\0" {
        return ERR_INVALID_FORMAT;
    }

    let coff_off = pe_offset + 4;
    let section_count = match read_u16_le_at(raw, coff_off + 2) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let optional_size = match read_u16_le_at(raw, coff_off + 16) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let optional_off = coff_off + 20;
    if optional_off + optional_size > raw.len() {
        return ERR_INVALID_FORMAT;
    }

    let magic = match read_u16_le_at(raw, optional_off) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let data_dir_base = if magic == 0x20b {
        optional_off + 112
    } else if magic == 0x10b {
        optional_off + 96
    } else {
        return ERR_INVALID_FORMAT;
    };
    if data_dir_base + 8 > optional_off + optional_size {
        return ERR_OK;
    }

    let export_rva = match read_u32_le_at(raw, data_dir_base) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if export_rva == 0 {
        return ERR_OK;
    }
    let section_table_off = optional_off + optional_size;
    if section_table_off + section_count.saturating_mul(40) > raw.len() {
        return ERR_INVALID_FORMAT;
    }

    let export_dir_off = match pe_rva_to_file_offset(raw, section_table_off, section_count, export_rva) {
        Some(v) => v,
        None => return ERR_OK,
    };
    if export_dir_off + 40 > raw.len() {
        return ERR_INVALID_FORMAT;
    }

    let number_of_names = match read_u32_le_at(raw, export_dir_off + 24) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let address_of_names_rva = match read_u32_le_at(raw, export_dir_off + 32) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if number_of_names == 0 || address_of_names_rva == 0 {
        return ERR_OK;
    }

    let names_table_off = match pe_rva_to_file_offset(raw, section_table_off, section_count, address_of_names_rva) {
        Some(v) => v,
        None => return ERR_OK,
    };
    for i in 0..number_of_names {
        let entry_off = match names_table_off.checked_add(i.saturating_mul(4)) {
            Some(v) => v,
            None => return ERR_INVALID_FORMAT,
        };
        if entry_off + 4 > raw.len() {
            return ERR_INVALID_FORMAT;
        }
        let name_rva = match read_u32_le_at(raw, entry_off) {
            Some(v) if v != 0 => v,
            _ => continue,
        };
        let name_off = match pe_rva_to_file_offset(raw, section_table_off, section_count, name_rva) {
            Some(v) => v,
            None => continue,
        };
        if let Some(name) = read_c_string_from_bytes(raw, name_off) {
            add_shared_global_symbol(state, &name);
        }
    }

    ERR_OK
}

fn ingest_shared_coff_symbols(state: &mut LinkerState, raw: &[u8]) -> u32 {
    if raw.len() < 20 {
        return ERR_INVALID_FORMAT;
    }
    let machine = match read_u16_le_at(raw, 0) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if machine != COFF_MACHINE_X86_64 && machine != COFF_MACHINE_AARCH64 {
        return ERR_UNSUPPORTED_TARGET;
    }
    let ptr_symtab = match read_u32_le_at(raw, 8) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let symbol_count_raw = match read_u32_le_at(raw, 12) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    if ptr_symtab == 0 || symbol_count_raw == 0 {
        return ERR_OK;
    }

    let sym_end = match ptr_symtab.checked_add(symbol_count_raw.saturating_mul(18)) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if sym_end > raw.len() {
        return ERR_INVALID_FORMAT;
    }

    let mut strtab: Vec<u8> = vec![0, 0, 0, 0];
    if sym_end + 4 <= raw.len() {
        let str_size = read_u32_le_at(raw, sym_end).unwrap_or(0) as usize;
        if str_size >= 4 && sym_end + str_size <= raw.len() {
            strtab = raw[sym_end..sym_end + str_size].to_vec();
        }
    }

    let mut raw_index = 0usize;
    while raw_index < symbol_count_raw {
        let sym_off = ptr_symtab + (raw_index * 18);
        if sym_off + 18 > raw.len() {
            return ERR_INVALID_FORMAT;
        }
        let name_raw = &raw[sym_off..sym_off + 8];
        let name = read_coff_name(name_raw, &strtab);
        let section_number = read_i16_le_at(raw, sym_off + 12).unwrap_or(0);
        let storage_class = raw[sym_off + 16];
        let aux_count = raw[sym_off + 17] as usize;

        let is_external =
            storage_class == STORAGE_CLASS_EXTERNAL || storage_class == STORAGE_CLASS_WEAK_EXTERNAL;
        if is_external && section_number > 0 {
            add_shared_global_symbol(state, &name);
        }

        raw_index = raw_index.saturating_add(1 + aux_count);
    }

    ERR_OK
}

fn ingest_shared_macho_symbols(state: &mut LinkerState, raw: &[u8]) -> u32 {
    if raw.len() < 32 {
        return ERR_INVALID_FORMAT;
    }
    let magic = match read_u32_le_at(raw, 0) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if magic != 0xfeedfacf && magic != 0xcffaedfe {
        return ERR_INVALID_FORMAT;
    }

    let cpu_type = match read_u32_le_at(raw, 4) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if cpu_type != MACH_CPU_X86_64 && cpu_type != MACH_CPU_ARM64 {
        return ERR_UNSUPPORTED_TARGET;
    }

    let ncmds = match read_u32_le_at(raw, 16) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    let sizeofcmds = match read_u32_le_at(raw, 20) {
        Some(v) => v as usize,
        None => return ERR_INVALID_FORMAT,
    };
    if 32usize.saturating_add(sizeofcmds) > raw.len() {
        return ERR_INVALID_FORMAT;
    }

    let mut symoff: usize = 0;
    let mut nsyms: usize = 0;
    let mut stroff: usize = 0;
    let mut strsize: usize = 0;
    let mut iextdefsym: Option<usize> = None;
    let mut nextdefsym: Option<usize> = None;

    let mut cursor = 32usize;
    for _ in 0..ncmds {
        if cursor + 8 > raw.len() {
            return ERR_INVALID_FORMAT;
        }
        let cmd = match read_u32_le_at(raw, cursor) {
            Some(v) => v,
            None => return ERR_INVALID_FORMAT,
        };
        let cmdsize = match read_u32_le_at(raw, cursor + 4) {
            Some(v) => v as usize,
            None => return ERR_INVALID_FORMAT,
        };
        if cmdsize < 8 || cursor + cmdsize > raw.len() {
            return ERR_INVALID_FORMAT;
        }

        if cmd == 0x2 {
            symoff = match read_u32_le_at(raw, cursor + 8) {
                Some(v) => v as usize,
                None => return ERR_INVALID_FORMAT,
            };
            nsyms = match read_u32_le_at(raw, cursor + 12) {
                Some(v) => v as usize,
                None => return ERR_INVALID_FORMAT,
            };
            stroff = match read_u32_le_at(raw, cursor + 16) {
                Some(v) => v as usize,
                None => return ERR_INVALID_FORMAT,
            };
            strsize = match read_u32_le_at(raw, cursor + 20) {
                Some(v) => v as usize,
                None => return ERR_INVALID_FORMAT,
            };
        } else if cmd == 0xb {
            iextdefsym = read_u32_le_at(raw, cursor + 16).map(|v| v as usize);
            nextdefsym = read_u32_le_at(raw, cursor + 20).map(|v| v as usize);
        }
        cursor = cursor.saturating_add(cmdsize);
    }

    if nsyms == 0 || strsize < 1 {
        return ERR_OK;
    }
    let sym_end = match symoff.checked_add(nsyms.saturating_mul(16)) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let str_end = match stroff.checked_add(strsize) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    if sym_end > raw.len() || str_end > raw.len() {
        return ERR_INVALID_FORMAT;
    }
    let strtab = &raw[stroff..str_end];
    let start_index = iextdefsym.unwrap_or(0).min(nsyms);
    let end_index = if let Some(count) = nextdefsym {
        start_index.saturating_add(count).min(nsyms)
    } else {
        nsyms
    };

    for i in start_index..end_index {
        let off = symoff + (i * 16);
        if off + 16 > raw.len() {
            return ERR_INVALID_FORMAT;
        }
        let strx = match read_u32_le_at(raw, off) {
            Some(v) => v as usize,
            None => return ERR_INVALID_FORMAT,
        };
        let n_type = raw[off + 4];
        let n_type_kind = n_type & 0x0e;
        let is_external = (n_type & 0x01) != 0;
        if !is_external || n_type_kind == 0x00 || strx >= strtab.len() {
            continue;
        }
        let rel = &strtab[strx..];
        let end = rel.iter().position(|b| *b == 0).unwrap_or(rel.len());
        if end == 0 {
            continue;
        }
        let name = String::from_utf8_lossy(&rel[..end]).to_string();
        add_shared_global_symbol(state, &name);
        if let Some(stripped) = name.strip_prefix('_') {
            if !stripped.is_empty() {
                add_shared_global_symbol(state, stripped);
            }
        }
    }

    ERR_OK
}

fn ingest_shared_object_symbols(state: &mut LinkerState, path: &Path) -> u32 {
    let raw = match fs::read(path) {
        Ok(v) => v,
        Err(_) => return ERR_FILE_NOT_FOUND,
    };
    if raw.len() >= 2 && raw.get(0).copied() == Some(b'M') && raw.get(1).copied() == Some(b'Z') {
        return ingest_shared_pe_symbols(state, &raw);
    }
    match probe_object_format_bytes(&raw) {
        OBJECT_FORMAT_ELF64 => ingest_shared_elf_symbols(state, &raw),
        OBJECT_FORMAT_COFF64 => ingest_shared_coff_symbols(state, &raw),
        OBJECT_FORMAT_MACHO64 => ingest_shared_macho_symbols(state, &raw),
        OBJECT_FORMAT_UNKNOWN => ERR_OK,
        _ => ERR_OK,
    }
}

fn coff_characteristics_to_flags(characteristics: u32, section_name: &str) -> (u32, u64) {
    let mut sh_flags = SHF_ALLOC;
    if characteristics & 0x8000_0000 != 0 {
        sh_flags |= SHF_WRITE;
    }
    if characteristics & 0x2000_0000 != 0 {
        sh_flags |= SHF_EXECINSTR;
    }
    if characteristics & 0x4000_0000 != 0 {
        sh_flags |= SHF_ALLOC;
    }
    if characteristics & 0x020 != 0 {
        sh_flags |= SHF_EXECINSTR;
    }
    if section_name.starts_with(".text") {
        sh_flags |= SHF_EXECINSTR;
    }
    let section_type = if characteristics & 0x80 != 0 {
        SHT_NOBITS
    } else {
        SHT_PROGBITS
    };
    (section_type, sh_flags)
}

fn map_coff_relocation_type(machine: u16, reloc_type: u16) -> u32 {
    if machine == COFF_MACHINE_AARCH64 {
        return match reloc_type {
            0x0000 => R_AARCH64_NONE,
            0x0002 | 0x0003 | 0x0004 | 0x0005 | 0x0006 | 0x0007 | 0x0008 | 0x0009 => {
                R_AARCH64_PREL32
            }
            0x000d => R_AARCH64_ABS32,
            0x000e => R_AARCH64_ABS64,
            _ => R_AARCH64_PREL32,
        };
    }

    // IMAGE_REL_AMD64_* and related x86 family defaults.
    match reloc_type {
        0x0000 => R_X86_64_NONE, // ABSOLUTE
        0x0001 => R_X86_64_64,   // ADDR64
        0x0002 => R_X86_64_32,   // ADDR32
        0x0003 => R_X86_64_32,   // ADDR32NB
        0x0004 | 0x0005 | 0x0006 | 0x0007 | 0x0008 | 0x0009 => R_X86_64_PC32, // REL32*
        0x000a | 0x000b => R_X86_64_32, // SECTION / SECREL
        0x000e | 0x0010 => R_X86_64_PC32, // SREL32 / SSPAN32
        _ => R_X86_64_32,
    }
}

fn map_macho_relocation_type(machine: u16, macho_type: u32, pcrel: u32, length: u32) -> u32 {
    let is_64 = length >= 3;
    if machine == EM_AARCH64 {
        return match macho_type {
            0 => {
                if is_64 {
                    R_AARCH64_ABS64
                } else {
                    R_AARCH64_ABS32
                }
            }
            2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 => R_AARCH64_PREL32,
            _ => {
                if pcrel == 1 {
                    R_AARCH64_PREL32
                } else if is_64 {
                    R_AARCH64_ABS64
                } else {
                    R_AARCH64_ABS32
                }
            }
        };
    }

    match macho_type {
        0 => {
            if is_64 {
                R_X86_64_64
            } else {
                R_X86_64_32
            }
        }
        1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 => R_X86_64_PC32,
        _ => {
            if pcrel == 1 {
                R_X86_64_PC32
            } else if is_64 {
                R_X86_64_64
            } else {
                R_X86_64_32
            }
        }
    }
}

fn relocation_addend_width(reloc_type: u32) -> usize {
    if reloc_type == R_X86_64_64 || reloc_type == R_AARCH64_ABS64 {
        8
    } else {
        4
    }
}

fn read_coff_name(name_raw: &[u8], strtab: &[u8]) -> String {
    if name_raw.len() < 8 {
        return String::new();
    }
    if name_raw[0..4] == [0, 0, 0, 0] {
        let off = u32::from_le_bytes([name_raw[4], name_raw[5], name_raw[6], name_raw[7]]) as usize;
        if off >= 4 && off < strtab.len() {
            let rel = &strtab[off..];
            let end = rel.iter().position(|b| *b == 0).unwrap_or(rel.len());
            return String::from_utf8_lossy(&rel[..end]).to_string();
        }
        return String::new();
    }
    let end = name_raw.iter().position(|b| *b == 0).unwrap_or(8);
    String::from_utf8_lossy(&name_raw[..end]).trim().to_string()
}

fn read_addend(section: &ObjectSection, offset: u64, width: usize) -> u64 {
    let at = offset as usize;
    if at + width > section.data.len() {
        return 0;
    }
    if width == 8 {
        u64::from_le_bytes([
            section.data[at],
            section.data[at + 1],
            section.data[at + 2],
            section.data[at + 3],
            section.data[at + 4],
            section.data[at + 5],
            section.data[at + 6],
            section.data[at + 7],
        ])
    } else {
        u32::from_le_bytes([
            section.data[at],
            section.data[at + 1],
            section.data[at + 2],
            section.data[at + 3],
        ]) as u64
    }
}

fn parse_coff_object(path: u64) -> Result<ObjectRecord, u32> {
    let p = to_path(path).ok_or(ERR_FILE_NOT_FOUND)?;
    let raw = fs::read(&p).map_err(|_| ERR_FILE_NOT_FOUND)?;
    if raw.len() < 20 {
        return Err(ERR_INVALID_FORMAT);
    }

    let machine = read_u16_le_at(&raw, 0).ok_or(ERR_INVALID_FORMAT)?;
    if machine != COFF_MACHINE_X86_64 && machine != COFF_MACHINE_AARCH64 {
        return Err(ERR_UNSUPPORTED_TARGET);
    }
    let section_count = read_u16_le_at(&raw, 2).ok_or(ERR_INVALID_FORMAT)? as usize;
    let ptr_symtab = read_u32_le_at(&raw, 8).ok_or(ERR_INVALID_FORMAT)? as usize;
    let symbol_count_raw = read_u32_le_at(&raw, 12).ok_or(ERR_INVALID_FORMAT)? as usize;
    let optional_size = read_u16_le_at(&raw, 16).ok_or(ERR_INVALID_FORMAT)? as usize;

    let section_table_off = 20usize.saturating_add(optional_size);
    let section_table_end = section_table_off.saturating_add(section_count.saturating_mul(40));
    if section_table_end > raw.len() {
        return Err(ERR_INVALID_FORMAT);
    }

    let mut record = ObjectRecord {
        path,
        file_size: raw.len() as u64,
        elf_type: 1,
        machine: if machine == COFF_MACHINE_AARCH64 {
            EM_AARCH64
        } else {
            EM_X86_64
        },
        sections: Vec::new(),
        symbols: Vec::new(),
        relocations: Vec::new(),
    };
    record.sections.push(ObjectSection::default());

    let mut section_meta: Vec<(usize, usize, usize, String)> = Vec::new();
    for i in 0..section_count {
        let off = section_table_off + (i * 40);
        let name = read_coff_name(&raw[off..off + 8], &[]);
        let size_of_raw = read_u32_le_at(&raw, off + 16).ok_or(ERR_INVALID_FORMAT)? as usize;
        let ptr_raw = read_u32_le_at(&raw, off + 20).ok_or(ERR_INVALID_FORMAT)? as usize;
        let ptr_reloc = read_u32_le_at(&raw, off + 24).ok_or(ERR_INVALID_FORMAT)? as usize;
        let num_reloc = read_u16_le_at(&raw, off + 32).ok_or(ERR_INVALID_FORMAT)? as usize;
        let characteristics = read_u32_le_at(&raw, off + 36).ok_or(ERR_INVALID_FORMAT)?;

        let (section_type, flags) = coff_characteristics_to_flags(characteristics, &name);
        let data = if section_type == SHT_NOBITS {
            vec![0u8; size_of_raw]
        } else if size_of_raw == 0 {
            Vec::new()
        } else {
            let end = ptr_raw.saturating_add(size_of_raw);
            if end > raw.len() {
                return Err(ERR_INVALID_FORMAT);
            }
            raw[ptr_raw..end].to_vec()
        };

        record.sections.push(ObjectSection {
            index: (i + 1) as u32,
            section_type,
            flags,
            offset: ptr_raw as u64,
            size: size_of_raw as u64,
            link: 0,
            info: 0,
            align: 16,
            entsize: 0,
            data,
        });
        section_meta.push((ptr_reloc, num_reloc, size_of_raw, name));
    }

    let mut strtab: Vec<u8> = vec![0, 0, 0, 0];
    if ptr_symtab != 0 && symbol_count_raw > 0 {
        let sym_end = ptr_symtab.saturating_add(symbol_count_raw.saturating_mul(18));
        if sym_end > raw.len() {
            return Err(ERR_INVALID_FORMAT);
        }
        if sym_end + 4 <= raw.len() {
            let str_size = read_u32_le_at(&raw, sym_end).unwrap_or(0) as usize;
            if str_size >= 4 && sym_end + str_size <= raw.len() {
                strtab = raw[sym_end..sym_end + str_size].to_vec();
            }
        }

        let mut raw_to_canonical = vec![0u32; symbol_count_raw];
        let mut raw_index = 0usize;
        while raw_index < symbol_count_raw {
            let sym_off = ptr_symtab + (raw_index * 18);
            let sym_rec_end = sym_off + 18;
            if sym_rec_end > raw.len() {
                return Err(ERR_INVALID_FORMAT);
            }
            let name_raw = &raw[sym_off..sym_off + 8];
            let mut name = read_coff_name(name_raw, &strtab);
            if name.is_empty() {
                name = format!("coff_sym_{}", raw_index);
            }
            let name_hash = fnv1a64(&name);
            let value = read_u32_le_at(&raw, sym_off + 8).ok_or(ERR_INVALID_FORMAT)? as u64;
            let section_number = read_i16_le_at(&raw, sym_off + 12).ok_or(ERR_INVALID_FORMAT)?;
            let sym_type_raw = read_u16_le_at(&raw, sym_off + 14).ok_or(ERR_INVALID_FORMAT)?;
            let storage_class = raw[sym_off + 16];
            let aux_count = raw[sym_off + 17] as usize;

            let bind = match storage_class {
                STORAGE_CLASS_EXTERNAL => 1,
                STORAGE_CLASS_WEAK_EXTERNAL => 2,
                STORAGE_CLASS_STATIC | STORAGE_CLASS_FILE => 0,
                _ => 0,
            };
            let sym_type = if storage_class == STORAGE_CLASS_FILE {
                4
            } else if (sym_type_raw & 0x20) != 0 {
                2
            } else {
                0
            };
            let shndx = if section_number == 0 {
                SHN_UNDEF
            } else if section_number < 0 {
                SHN_ABS
            } else {
                section_number as u16
            };

            let canonical = record.symbols.len() as u32;
            record.symbols.push(ObjectSymbol {
                name_hash,
                bind,
                sym_type,
                shndx,
                value,
                size: 0,
                strtab_section: 0,
            });
            raw_to_canonical[raw_index] = canonical;
            for aux_i in 1..=aux_count {
                let mapped = raw_index + aux_i;
                if mapped < raw_to_canonical.len() {
                    raw_to_canonical[mapped] = canonical;
                }
            }
            raw_index = raw_index.saturating_add(1 + aux_count);
        }

        for (sec_idx, (ptr_reloc, num_reloc, _sec_size, _name)) in section_meta.iter().enumerate() {
            if *num_reloc == 0 {
                continue;
            }
            for rel_i in 0..*num_reloc {
                let rel_off = ptr_reloc.saturating_add(rel_i.saturating_mul(10));
                if rel_off + 10 > raw.len() {
                    return Err(ERR_INVALID_FORMAT);
                }
                let offset = read_u32_le_at(&raw, rel_off).ok_or(ERR_INVALID_FORMAT)? as u64;
                let raw_symbol = read_u32_le_at(&raw, rel_off + 4).ok_or(ERR_INVALID_FORMAT)? as usize;
                let reloc_kind_raw = read_u16_le_at(&raw, rel_off + 8).ok_or(ERR_INVALID_FORMAT)?;
                let reloc_type = map_coff_relocation_type(machine, reloc_kind_raw);
                let symbol = if raw_symbol < raw_to_canonical.len() {
                    raw_to_canonical[raw_symbol]
                } else {
                    0
                };
                let section_ref = &record.sections[sec_idx + 1];
                let width = relocation_addend_width(reloc_type);
                let addend = read_addend(section_ref, offset, width);
                record.relocations.push(ObjectRelocation {
                    section: (sec_idx + 1) as u32,
                    offset,
                    reloc_type,
                    symbol,
                    addend,
                });
            }
        }
    }

    Ok(record)
}

fn parse_macho_object(path: u64) -> Result<ObjectRecord, u32> {
    let p = to_path(path).ok_or(ERR_FILE_NOT_FOUND)?;
    let raw = fs::read(&p).map_err(|_| ERR_FILE_NOT_FOUND)?;
    if raw.len() < 32 {
        return Err(ERR_INVALID_FORMAT);
    }
    let magic = read_u32_le_at(&raw, 0).ok_or(ERR_INVALID_FORMAT)?;
    if magic != 0xfeedfacf && magic != 0xcffaedfe {
        return Err(ERR_INVALID_FORMAT);
    }

    let cpu_type = read_u32_le_at(&raw, 4).ok_or(ERR_INVALID_FORMAT)?;
    if cpu_type != MACH_CPU_X86_64 && cpu_type != MACH_CPU_ARM64 {
        return Err(ERR_UNSUPPORTED_TARGET);
    }
    let ncmds = read_u32_le_at(&raw, 16).ok_or(ERR_INVALID_FORMAT)? as usize;
    let sizeofcmds = read_u32_le_at(&raw, 20).ok_or(ERR_INVALID_FORMAT)? as usize;
    if 32usize.saturating_add(sizeofcmds) > raw.len() {
        return Err(ERR_INVALID_FORMAT);
    }

    let mut record = ObjectRecord {
        path,
        file_size: raw.len() as u64,
        elf_type: 1,
        machine: if cpu_type == MACH_CPU_ARM64 {
            EM_AARCH64
        } else {
            EM_X86_64
        },
        sections: Vec::new(),
        symbols: Vec::new(),
        relocations: Vec::new(),
    };
    record.sections.push(ObjectSection::default());

    #[derive(Clone, Copy)]
    struct MachRelocList {
        section_index: u32,
        reloff: usize,
        nreloc: usize,
    }

    let mut reloc_lists: Vec<MachRelocList> = Vec::new();
    let mut symoff: usize = 0;
    let mut nsyms: usize = 0;
    let mut stroff: usize = 0;
    let mut strsize: usize = 0;
    let mut cursor = 32usize;
    for _ in 0..ncmds {
        if cursor + 8 > raw.len() {
            return Err(ERR_INVALID_FORMAT);
        }
        let cmd = read_u32_le_at(&raw, cursor).ok_or(ERR_INVALID_FORMAT)?;
        let cmdsize = read_u32_le_at(&raw, cursor + 4).ok_or(ERR_INVALID_FORMAT)? as usize;
        if cmdsize < 8 || cursor + cmdsize > raw.len() {
            return Err(ERR_INVALID_FORMAT);
        }
        if cmd == 0x2 {
            symoff = read_u32_le_at(&raw, cursor + 8).ok_or(ERR_INVALID_FORMAT)? as usize;
            nsyms = read_u32_le_at(&raw, cursor + 12).ok_or(ERR_INVALID_FORMAT)? as usize;
            stroff = read_u32_le_at(&raw, cursor + 16).ok_or(ERR_INVALID_FORMAT)? as usize;
            strsize = read_u32_le_at(&raw, cursor + 20).ok_or(ERR_INVALID_FORMAT)? as usize;
        } else if cmd == 0x19 {
            let nsects = read_u32_le_at(&raw, cursor + 64).ok_or(ERR_INVALID_FORMAT)? as usize;
            let sec_base = cursor + 72;
            let sec_end = sec_base.saturating_add(nsects.saturating_mul(80));
            if sec_end > raw.len() {
                return Err(ERR_INVALID_FORMAT);
            }
            for sec_i in 0..nsects {
                let off = sec_base + (sec_i * 80);
                let sect_name_raw = &raw[off..off + 16];
                let sect_name_end = sect_name_raw
                    .iter()
                    .position(|b| *b == 0)
                    .unwrap_or(sect_name_raw.len());
                let sect_name = String::from_utf8_lossy(&sect_name_raw[..sect_name_end]).to_string();

                let size = read_u64_le_at(&raw, off + 40).ok_or(ERR_INVALID_FORMAT)? as usize;
                let data_offset = read_u32_le_at(&raw, off + 48).ok_or(ERR_INVALID_FORMAT)? as usize;
                let align_pow = read_u32_le_at(&raw, off + 52).ok_or(ERR_INVALID_FORMAT)?;
                let reloff = read_u32_le_at(&raw, off + 56).ok_or(ERR_INVALID_FORMAT)? as usize;
                let nreloc = read_u32_le_at(&raw, off + 60).ok_or(ERR_INVALID_FORMAT)? as usize;
                let flags_raw = read_u32_le_at(&raw, off + 64).ok_or(ERR_INVALID_FORMAT)?;
                let section_type = if (flags_raw & 0xff) == 0x1 {
                    SHT_NOBITS
                } else {
                    SHT_PROGBITS
                };
                let mut flags = SHF_ALLOC;
                if sect_name.contains("text") {
                    flags |= SHF_EXECINSTR;
                }
                if sect_name.contains("data") {
                    flags |= SHF_WRITE;
                }
                let data = if section_type == SHT_NOBITS {
                    vec![0u8; size]
                } else if size == 0 {
                    Vec::new()
                } else {
                    let end = data_offset.saturating_add(size);
                    if end > raw.len() {
                        return Err(ERR_INVALID_FORMAT);
                    }
                    raw[data_offset..end].to_vec()
                };

                let section_index = record.sections.len() as u32;
                record.sections.push(ObjectSection {
                    index: section_index,
                    section_type,
                    flags,
                    offset: data_offset as u64,
                    size: size as u64,
                    link: 0,
                    info: 0,
                    align: 1u64 << align_pow.min(12),
                    entsize: 0,
                    data,
                });

                if nreloc > 0 {
                    reloc_lists.push(MachRelocList {
                        section_index,
                        reloff,
                        nreloc,
                    });
                }
            }
        }
        cursor = cursor.saturating_add(cmdsize);
    }

    if nsyms > 0 {
        let sym_end = symoff.saturating_add(nsyms.saturating_mul(16));
        let str_end = stroff.saturating_add(strsize);
        if sym_end > raw.len() || str_end > raw.len() || strsize < 1 {
            return Err(ERR_INVALID_FORMAT);
        }
        let strtab = &raw[stroff..str_end];
        for i in 0..nsyms {
            let off = symoff + (i * 16);
            let strx = read_u32_le_at(&raw, off).ok_or(ERR_INVALID_FORMAT)? as usize;
            let n_type = raw[off + 4];
            let n_sect = raw[off + 5] as u16;
            let _n_desc = read_u16_le_at(&raw, off + 6).ok_or(ERR_INVALID_FORMAT)?;
            let n_value = read_u64_le_at(&raw, off + 8).ok_or(ERR_INVALID_FORMAT)?;

            let name = if strx < strtab.len() {
                let rel = &strtab[strx..];
                let end = rel.iter().position(|b| *b == 0).unwrap_or(rel.len());
                String::from_utf8_lossy(&rel[..end]).to_string()
            } else {
                format!("macho_sym_{}", i)
            };
            let name_hash = fnv1a64(&name);
            let bind = if (n_type & 0x01) != 0 { 1 } else { 0 };
            let n_type_kind = n_type & 0x0e;
            let shndx = if n_type_kind == 0x00 {
                SHN_UNDEF
            } else if n_type_kind == 0x02 {
                SHN_ABS
            } else {
                n_sect
            };
            record.symbols.push(ObjectSymbol {
                name_hash,
                bind,
                sym_type: 0,
                shndx,
                value: n_value,
                size: 0,
                strtab_section: 0,
            });
        }
    }

    for reloc in reloc_lists {
        for r in 0..reloc.nreloc {
            let off = reloc.reloff.saturating_add(r.saturating_mul(8));
            if off + 8 > raw.len() {
                return Err(ERR_INVALID_FORMAT);
            }
            let r_address = read_i32_le_at(&raw, off).ok_or(ERR_INVALID_FORMAT)? as i64;
            if r_address < 0 {
                continue;
            }
            let info = read_u32_le_at(&raw, off + 4).ok_or(ERR_INVALID_FORMAT)?;
            let symbolnum = info & 0x00ff_ffff;
            let pcrel = (info >> 24) & 0x1;
            let length = (info >> 25) & 0x3;
            let is_extern = (info >> 27) & 0x1;
            let macho_type = (info >> 28) & 0x0f;
            let reloc_type = map_macho_relocation_type(record.machine, macho_type, pcrel, length);
            let section_ref = match record.sections.iter().find(|s| s.index == reloc.section_index) {
                Some(v) => v,
                None => continue,
            };
            let width = relocation_addend_width(reloc_type);
            let addend = read_addend(section_ref, r_address as u64, width);
            let symbol = if is_extern == 1 && (symbolnum as usize) < record.symbols.len() {
                symbolnum as u32
            } else {
                0
            };
            record.relocations.push(ObjectRelocation {
                section: reloc.section_index,
                offset: r_address as u64,
                reloc_type,
                symbol,
                addend,
            });
        }
    }

    Ok(record)
}

fn write_fixed_name(dst: &mut [u8], off: usize, width: usize, text: &[u8]) {
    if off + width > dst.len() {
        return;
    }
    for i in 0..width {
        dst[off + i] = 0;
    }
    let len = text.len().min(width);
    dst[off..off + len].copy_from_slice(&text[..len]);
}

fn pe_section_characteristics(flags: u64) -> u32 {
    let exec = flags & SHF_EXECINSTR != 0;
    let write = flags & SHF_WRITE != 0;
    let mut out = 0x4000_0000u32;
    if exec {
        out |= 0x2000_0000;
        out |= 0x0000_0020;
    } else {
        out |= 0x0000_0040;
    }
    if write {
        out |= 0x8000_0000;
    }
    out
}

fn pe_section_name(flags: u64, index: usize) -> [u8; 8] {
    let mut out = [0u8; 8];
    let stem = if flags & SHF_EXECINSTR != 0 {
        b".text".as_slice()
    } else if flags & SHF_WRITE != 0 {
        b".data".as_slice()
    } else {
        b".rdata".as_slice()
    };
    let stem_len = stem.len().min(8);
    out[..stem_len].copy_from_slice(&stem[..stem_len]);
    if index > 0 {
        let suffix = format!("{:02}", index % 100);
        let s = suffix.as_bytes();
        let mut pos = stem_len;
        for byte in s {
            if pos >= 8 {
                break;
            }
            out[pos] = *byte;
            pos += 1;
        }
    }
    out
}

fn write_minimal_pe_exec(
    path: &Path,
    entry: u64,
    image_base: u64,
    chunks: &[OutputChunk],
    machine: u16,
    pe_no_entry: bool,
    pe_dynamic_base: bool,
    pe_nx_compat: bool,
    pe_large_address_aware: bool,
) -> u32 {
    #[derive(Clone)]
    struct PeSectionPlan {
        name: [u8; 8],
        rva: u32,
        raw_off: u32,
        virt_size: u32,
        raw_size: u32,
        characteristics: u32,
        bytes: Vec<u8>,
    }

    let mut section_chunks = chunks.to_vec();
    if section_chunks.is_empty() {
        section_chunks.push(OutputChunk {
            addr: 0,
            bytes: vec![0xC3u8],
            flags: SHF_ALLOC | SHF_EXECINSTR,
        });
    }

    let file_align = 0x200u32;
    let section_align = 0x1000u32;
    let pe_offset = 0x80usize;
    let section_count = section_chunks.len().min(u16::MAX as usize);
    let headers_unaligned = pe_offset + 4 + 20 + 0xF0 + section_count * 40;
    let headers_raw_size = align_up_u32(headers_unaligned as u32, file_align);

    let source_base = section_chunks.first().map(|c| c.addr).unwrap_or(0);
    let mut plans = Vec::<PeSectionPlan>::new();
    let mut cursor_raw = 0u32;
    let mut cursor_rva = 0x1000u32;
    let mut size_of_code = 0u32;
    let mut size_of_init_data = 0u32;
    let mut base_of_code = 0u32;
    let mut first_exec_rva = 0u32;
    let mut first_data_rva = 0u32;
    let mut max_rva_end = 0u32;

    for (idx, chunk) in section_chunks.iter().enumerate().take(section_count) {
        let rel = chunk.addr.saturating_sub(source_base);
        let rel_u32 = rel.min(u32::MAX as u64) as u32;
        let base_rva = 0x1000u32.saturating_add(rel_u32);
        let rva = align_up_u32(base_rva.max(cursor_rva), section_align);
        let raw_off = align_up_u32(cursor_raw, file_align);
        let virt_size = (chunk.bytes.len().max(1)).min(u32::MAX as usize) as u32;
        let raw_size = align_up_u32(virt_size, file_align);
        let characteristics = pe_section_characteristics(chunk.flags);
        if chunk.flags & SHF_EXECINSTR != 0 {
            if first_exec_rva == 0 {
                first_exec_rva = rva;
            }
            size_of_code = size_of_code.saturating_add(raw_size);
            if base_of_code == 0 {
                base_of_code = rva;
            }
        } else {
            if first_data_rva == 0 {
                first_data_rva = rva;
            }
            size_of_init_data = size_of_init_data.saturating_add(raw_size);
        }
        let rva_end = rva.saturating_add(align_up_u32(virt_size, section_align));
        if rva_end > max_rva_end {
            max_rva_end = rva_end;
        }
        plans.push(PeSectionPlan {
            name: pe_section_name(chunk.flags, idx),
            rva,
            raw_off,
            virt_size,
            raw_size,
            characteristics,
            bytes: chunk.bytes.clone(),
        });
        cursor_raw = raw_off.saturating_add(raw_size);
        cursor_rva = rva_end;
    }

    if base_of_code == 0 {
        base_of_code = 0x1000;
    }
    if first_data_rva == 0 {
        first_data_rva = base_of_code;
    }

    let resolved_base = if image_base == 0 {
        0x0000_0001_4000_0000u64
    } else {
        image_base
    };
    let mut entry_rva = if entry >= source_base && source_base != 0 {
        0x1000u32.saturating_add((entry - source_base).min(u32::MAX as u64) as u32)
    } else {
        first_exec_rva.max(base_of_code)
    };
    if entry_rva < 0x1000 {
        entry_rva = 0x1000;
    }
    if pe_no_entry {
        entry_rva = 0;
    }

    let total_size = headers_raw_size as usize + cursor_raw as usize;
    let size_of_image = align_up_u32(max_rva_end.max(0x1000), section_align);
    let mut out = vec![0u8; total_size];

    out[0] = b'M';
    out[1] = b'Z';
    write_u32_le(&mut out, 0x3c, pe_offset as u32);

    out[pe_offset..pe_offset + 4].copy_from_slice(b"PE\0\0");
    let coff = pe_offset + 4;
    write_u16_le(&mut out, coff + 0, machine);
    write_u16_le(&mut out, coff + 2, section_count as u16);
    write_u32_le(&mut out, coff + 4, 0);
    write_u32_le(&mut out, coff + 8, 0);
    write_u32_le(&mut out, coff + 12, 0);
    write_u16_le(&mut out, coff + 16, 0xF0);
    let mut characteristics = 0x0002u16;
    if pe_large_address_aware {
        characteristics |= 0x0020;
    }
    write_u16_le(&mut out, coff + 18, characteristics);

    let opt = coff + 20;
    write_u16_le(&mut out, opt + 0, 0x20b);
    write_u32_le(&mut out, opt + 4, size_of_code);
    write_u32_le(&mut out, opt + 8, size_of_init_data);
    write_u32_le(&mut out, opt + 16, entry_rva);
    write_u32_le(&mut out, opt + 20, base_of_code);
    write_u64_le(&mut out, opt + 24, resolved_base);
    write_u32_le(&mut out, opt + 32, section_align);
    write_u32_le(&mut out, opt + 36, file_align);
    write_u16_le(&mut out, opt + 48, 6);
    write_u16_le(&mut out, opt + 50, 0);
    write_u32_le(&mut out, opt + 56, size_of_image);
    write_u32_le(&mut out, opt + 60, headers_raw_size);
    write_u16_le(&mut out, opt + 68, 3);
    let mut dll_characteristics = 0x8000u16;
    if pe_dynamic_base {
        dll_characteristics |= 0x0040;
    }
    if pe_nx_compat {
        dll_characteristics |= 0x0100;
    }
    write_u16_le(&mut out, opt + 70, dll_characteristics);
    write_u64_le(&mut out, opt + 72, 0x0010_0000);
    write_u64_le(&mut out, opt + 80, 0x0000_1000);
    write_u64_le(&mut out, opt + 88, 0x0010_0000);
    write_u64_le(&mut out, opt + 96, 0x0000_1000);
    write_u32_le(&mut out, opt + 108, 16);

    let mut sh = opt + 0xF0;
    for plan in &plans {
        write_fixed_name(&mut out, sh, 8, &plan.name);
        write_u32_le(&mut out, sh + 8, plan.virt_size);
        write_u32_le(&mut out, sh + 12, plan.rva);
        write_u32_le(&mut out, sh + 16, plan.raw_size);
        write_u32_le(&mut out, sh + 20, headers_raw_size.saturating_add(plan.raw_off));
        write_u32_le(&mut out, sh + 36, plan.characteristics);
        sh += 40;
    }

    for plan in &plans {
        let start = headers_raw_size as usize + plan.raw_off as usize;
        let end = start.saturating_add(plan.bytes.len());
        if end <= out.len() {
            out[start..end].copy_from_slice(&plan.bytes);
        }
    }

    write_all(path, &out)
}

fn macho_section_names(flags: u64, index: usize) -> (&'static [u8], &'static [u8]) {
    if flags & SHF_EXECINSTR != 0 {
        if index == 0 {
            (b"__text", b"__TEXT")
        } else {
            (b"__textx", b"__TEXT")
        }
    } else if flags & SHF_WRITE != 0 {
        if index == 0 {
            (b"__data", b"__DATA")
        } else {
            (b"__datax", b"__DATA")
        }
    } else if index == 0 {
        (b"__const", b"__DATA_CONST")
    } else {
        (b"__constx", b"__DATA_CONST")
    }
}

fn macho_section_flags(flags: u64) -> u32 {
    if flags & SHF_EXECINSTR != 0 {
        0x8000_0400
    } else {
        0x0
    }
}

fn write_minimal_macho_exec(
    path: &Path,
    entry: u64,
    image_base: u64,
    chunks: &[OutputChunk],
    cpu_type: u32,
    cpu_subtype: u32,
) -> u32 {
    #[derive(Clone)]
    struct MachSectionPlan {
        name: [u8; 16],
        seg: [u8; 16],
        rva: u64,
        size: u64,
        file_offset: u32,
        align_pow: u32,
        flags: u32,
        bytes: Vec<u8>,
    }

    let mut section_chunks = chunks.to_vec();
    if section_chunks.is_empty() {
        section_chunks.push(OutputChunk {
            addr: 0,
            bytes: vec![0xC3u8],
            flags: SHF_ALLOC | SHF_EXECINSTR,
        });
    }

    let nsects = section_chunks.len().min(u32::MAX as usize);
    let header_size = 32usize;
    let segment_cmd_size = 72usize + nsects * 80usize;
    let thread_cmd_size = 184usize;
    let sizeofcmds = segment_cmd_size + thread_cmd_size;
    let fileoff = align_up((header_size + sizeofcmds) as u64, 0x1000) as usize;

    let source_base = section_chunks.first().map(|c| c.addr).unwrap_or(0);
    let mut plans = Vec::<MachSectionPlan>::new();
    let mut cursor_rva = 0u64;
    let mut max_rva_end = 0u64;
    let mut any_write = false;
    let mut any_exec = false;
    let mut first_exec_rva = 0u64;

    for (idx, chunk) in section_chunks.iter().enumerate().take(nsects) {
        let rel = chunk.addr.saturating_sub(source_base);
        let rva = align_up(rel.max(cursor_rva), 0x1000);
        let size = chunk.bytes.len().max(1) as u64;
        let (sect_name, seg_name) = macho_section_names(chunk.flags, idx);
        let mut name = [0u8; 16];
        let mut seg = [0u8; 16];
        let nlen = sect_name.len().min(16);
        let slen = seg_name.len().min(16);
        name[..nlen].copy_from_slice(&sect_name[..nlen]);
        seg[..slen].copy_from_slice(&seg_name[..slen]);
        let writable = chunk.flags & SHF_WRITE != 0;
        let executable = chunk.flags & SHF_EXECINSTR != 0;
        if writable {
            any_write = true;
        }
        if executable {
            any_exec = true;
            if first_exec_rva == 0 {
                first_exec_rva = rva;
            }
        }
        let end = rva.saturating_add(size);
        if end > max_rva_end {
            max_rva_end = end;
        }
        plans.push(MachSectionPlan {
            name,
            seg,
            rva,
            size,
            file_offset: (fileoff as u64 + rva).min(u32::MAX as u64) as u32,
            align_pow: 12,
            flags: macho_section_flags(chunk.flags),
            bytes: chunk.bytes.clone(),
        });
        cursor_rva = align_up(end, 0x1000);
    }

    let payload_size = max_rva_end.max(1);
    let filesize = fileoff.saturating_add(payload_size as usize);
    let vmaddr = if image_base == 0 {
        0x0000_0001_0000_0000u64
    } else {
        image_base
    };
    let mut entry_addr = if entry >= source_base && source_base != 0 {
        vmaddr.saturating_add(entry.saturating_sub(source_base))
    } else if first_exec_rva != 0 {
        vmaddr.saturating_add(first_exec_rva)
    } else {
        vmaddr
    };
    if entry_addr < vmaddr {
        entry_addr = vmaddr;
    }
    let vmsize = align_up(payload_size, 0x1000);

    let mut out = vec![0u8; filesize];
    write_u32_le(&mut out, 0, 0xfeedfacf);
    write_u32_le(&mut out, 4, cpu_type);
    write_u32_le(&mut out, 8, cpu_subtype);
    write_u32_le(&mut out, 12, 2);
    write_u32_le(&mut out, 16, 2);
    write_u32_le(&mut out, 20, sizeofcmds as u32);
    write_u32_le(&mut out, 24, 0);
    write_u32_le(&mut out, 28, 0);

    let seg = header_size;
    write_u32_le(&mut out, seg + 0, 0x19);
    write_u32_le(&mut out, seg + 4, segment_cmd_size as u32);
    write_fixed_name(&mut out, seg + 8, 16, b"__DUSTLINK");
    write_u64_le(&mut out, seg + 24, vmaddr);
    write_u64_le(&mut out, seg + 32, vmsize);
    write_u64_le(&mut out, seg + 40, fileoff as u64);
    write_u64_le(&mut out, seg + 48, payload_size);
    let mut prot = 1u32;
    if any_write {
        prot |= 2;
    }
    if any_exec {
        prot |= 4;
    }
    write_u32_le(&mut out, seg + 56, prot);
    write_u32_le(&mut out, seg + 60, prot);
    write_u32_le(&mut out, seg + 64, nsects as u32);
    write_u32_le(&mut out, seg + 68, 0);

    let mut sec_off = seg + 72;
    for plan in &plans {
        write_fixed_name(&mut out, sec_off, 16, &plan.name);
        write_fixed_name(&mut out, sec_off + 16, 16, &plan.seg);
        write_u64_le(&mut out, sec_off + 32, vmaddr.saturating_add(plan.rva));
        write_u64_le(&mut out, sec_off + 40, plan.size);
        write_u32_le(&mut out, sec_off + 48, plan.file_offset);
        write_u32_le(&mut out, sec_off + 52, plan.align_pow);
        write_u32_le(&mut out, sec_off + 56, 0);
        write_u32_le(&mut out, sec_off + 60, 0);
        write_u32_le(&mut out, sec_off + 64, plan.flags);
        write_u32_le(&mut out, sec_off + 68, 0);
        write_u32_le(&mut out, sec_off + 72, 0);
        write_u32_le(&mut out, sec_off + 76, 0);
        sec_off += 80;
    }

    let thread = seg + segment_cmd_size;
    write_u32_le(&mut out, thread + 0, 0x5);
    write_u32_le(&mut out, thread + 4, thread_cmd_size as u32);
    write_u32_le(&mut out, thread + 8, 4);
    write_u32_le(&mut out, thread + 12, 42);

    let regs = thread + 16;
    write_u64_le(&mut out, regs + (7 * 8), vmaddr + vmsize.saturating_sub(16));
    write_u64_le(&mut out, regs + (16 * 8), entry_addr);
    write_u64_le(&mut out, regs + (17 * 8), 0x202);

    for plan in &plans {
        let start = fileoff.saturating_add(plan.rva as usize);
        let end = start.saturating_add(plan.bytes.len());
        if end <= out.len() {
            out[start..end].copy_from_slice(&plan.bytes);
        }
    }

    write_all(path, &out)
}

fn extract_directive_arg(statement: &str, directive: &str) -> Option<String> {
    let s = statement.trim();
    if s.is_empty() {
        return None;
    }
    let upper = s.to_ascii_uppercase();
    if !upper.starts_with(directive) {
        return None;
    }
    let mut rest = &s[directive.len()..];
    rest = rest.trim_start();
    if !rest.starts_with('(') {
        return None;
    }
    let end = rest.rfind(')')?;
    let inner = &rest[1..end];
    Some(trim_quotes(inner).to_string())
}

fn set_output_format_from_text(state: &mut LinkerState, raw: &str) -> bool {
    let normalized = raw.trim().to_ascii_lowercase();
    if normalized.contains("elf") {
        state.output_format = FORMAT_ELF64;
        true
    } else if normalized.contains("pe") || normalized.contains("pei") {
        state.output_format = FORMAT_PE64;
        true
    } else if normalized.contains("mach") {
        state.output_format = FORMAT_MACHO64;
        true
    } else if normalized.contains("binary") || normalized == "bin" {
        state.output_format = FORMAT_BINARY;
        true
    } else if normalized.contains("mbr") {
        state.output_format = FORMAT_MBR;
        true
    } else {
        false
    }
}

fn apply_linker_script_statement(
    state: &mut LinkerState,
    statement: &str,
    script_dir: &Path,
    depth: u32,
) -> u32 {
    let statement_trimmed = statement.trim();
    let statement_upper = statement_trimmed.to_ascii_uppercase();

    if let Some((name, rhs)) = parse_script_symbol_assignment(statement_trimmed) {
        let value = match eval_script_expr(state, &rhs) {
            Some(v) => v,
            None => return ERR_INVALID_FORMAT,
        };
        let name_hash = fnv1a64(&name);
        if !state.allow_multiple_definition {
            if let Some(existing) = state.globals.get(&name_hash) {
                if existing.defined == 1 {
                    return ERR_MULTIPLE_DEFINITION;
                }
            }
        }
        state.globals.insert(
            name_hash,
            GlobalSymbol {
                object_index: 0,
                symbol_index: 0,
                bind: 1,
                defined: 1,
                address: value,
            },
        );
        return ERR_OK;
    }

    if statement_upper.starts_with("PHDRS") {
        if validate_named_block_statement(statement_trimmed, "PHDRS") {
            return ERR_OK;
        }
        return ERR_INVALID_FORMAT;
    }
    if statement_upper.starts_with("VERSION") {
        if validate_named_block_statement(statement_trimmed, "VERSION") {
            return ERR_OK;
        }
        return ERR_INVALID_FORMAT;
    }

    if let Some(arg) = extract_directive_arg(statement, "ASSERT") {
        let (expr, message) = split_assert_args(&arg);
        let ok = eval_script_condition(state, &expr).unwrap_or(false);
        if ok {
            return ERR_OK;
        }
        if !message.is_empty() {
            eprintln!("dustlink: script ASSERT failed: {}", message);
        }
        return ERR_CONFLICTING_OPTIONS;
    }

    if let Some(arg) = extract_directive_arg(statement, "ENTRY") {
        if let Some(v) = parse_u64_auto(&arg) {
            state.entry = v;
        } else if !arg.is_empty() {
            let hash = fnv1a64(&arg);
            if let Some(global) = state.globals.get(&hash) {
                state.entry = global.address;
            } else {
                require_symbol_hash(state, hash);
            }
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "OUTPUT_FORMAT") {
        if !set_output_format_from_text(state, &arg) {
            return ERR_INVALID_FORMAT;
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "OUTPUT_ARCH") {
        if arg.trim().is_empty() {
            return ERR_INVALID_FORMAT;
        }
        if let Some(target) = parse_target_value(&arg) {
            state.target = target;
            return ERR_OK;
        }
        return ERR_UNSUPPORTED_TARGET;
    }

    if let Some(arg) = extract_directive_arg(statement, "OUTPUT") {
        if !arg.is_empty() {
            let output_path = resolve_script_path(script_dir, &arg);
            state.output_path = intern_string(output_path.to_string_lossy().to_string());
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "TARGET") {
        if arg.trim().is_empty() {
            return ERR_INVALID_FORMAT;
        }
        if let Some(target) = parse_target_value(&arg) {
            state.target = target;
            return ERR_OK;
        }
        return ERR_UNSUPPORTED_TARGET;
    }

    if let Some(arg) = extract_directive_arg(statement, "SEARCH_DIR") {
        if !arg.is_empty() {
            let path = resolve_search_dir_path(state, script_dir, &arg);
            let handle = intern_string(path.to_string_lossy().to_string());
            state.search_paths.push(handle);
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "EXTERN") {
        for token in tokenize_script_args(&arg) {
            require_symbol_hash(state, fnv1a64(&token));
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "PROVIDE_HIDDEN")
        .or_else(|| extract_directive_arg(statement, "PROVIDE"))
    {
        if let Some((name_hash, value)) = parse_defsym_or_symbol_rhs(state, &arg) {
            let already_defined = state
                .globals
                .get(&name_hash)
                .map(|g| g.defined == 1)
                .unwrap_or(false);
            if !already_defined {
                state.globals.insert(
                    name_hash,
                    GlobalSymbol {
                        object_index: 0,
                        symbol_index: 0,
                        bind: 1,
                        defined: 1,
                        address: value,
                    },
                );
            }
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "INPUT")
        .or_else(|| extract_directive_arg(statement, "GROUP"))
    {
        let status = apply_script_input_list(state, script_dir, &arg);
        if status != ERR_OK {
            return status;
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "AS_NEEDED") {
        let prior = state.link_as_needed;
        state.link_as_needed = true;
        let status = apply_script_input_list(state, script_dir, &arg);
        state.link_as_needed = prior;
        if status != ERR_OK {
            return status;
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "NO_AS_NEEDED") {
        let prior = state.link_as_needed;
        state.link_as_needed = false;
        let status = apply_script_input_list(state, script_dir, &arg);
        state.link_as_needed = prior;
        if status != ERR_OK {
            return status;
        }
        return ERR_OK;
    }

    if let Some(arg) = extract_directive_arg(statement, "INCLUDE") {
        if depth >= 32 {
            return ERR_INVALID_FORMAT;
        }
        let include_path = resolve_script_path(script_dir, &arg);
        return apply_linker_script_file(state, &include_path, depth + 1);
    }

    if let Some(origin) = parse_memory_keyword_value(state, statement, "ORIGIN") {
        state.memory_origin = origin;
        state.image_base = origin;
    }
    if let Some(length) = parse_memory_keyword_value(state, statement, "LENGTH") {
        state.memory_length = length;
    }
    if let Some(dot_addr) = parse_sections_location_assignment(state, statement) {
        state.image_base = dot_addr;
    } else if let Some(section_addr) = parse_sections_output_address(state, statement) {
        state.image_base = section_addr;
    } else if let Some(load_addr) = parse_sections_load_address(state, statement) {
        state.pending_elf_image_base = load_addr;
        state.image_base = load_addr;
    } else if parse_sections_region_assignment(statement).is_some() && state.memory_origin != 0 {
        state.image_base = state.memory_origin;
    }

    if let Some(head) = script_head_identifier(statement_trimmed) {
        let upper_head = head.to_ascii_uppercase();
        if !is_supported_script_directive(&upper_head) {
            return ERR_UNSUPPORTED_FLAG;
        }
    }

    ERR_OK
}

fn apply_linker_script_file(state: &mut LinkerState, path: &Path, depth: u32) -> u32 {
    if !path.exists() || !path.is_file() {
        return ERR_FILE_NOT_FOUND;
    }
    let raw = match fs::read_to_string(path) {
        Ok(v) => v,
        Err(_) => return ERR_FILE_NOT_FOUND,
    };

    let mut cleaned = String::new();
    for mut line in raw.lines().map(|l| l.to_string()) {
        if let Some(idx) = line.find("//") {
            line.truncate(idx);
        }
        if let Some(idx) = line.find('#') {
            line.truncate(idx);
        }
        if !line.trim().is_empty() {
            cleaned.push_str(line.trim());
            cleaned.push('\n');
        }
    }

    let script_dir = path.parent().unwrap_or_else(|| Path::new("."));
    for stmt in split_script_statements(&cleaned) {
        let status = apply_linker_script_statement(state, &stmt, script_dir, depth);
        if status != ERR_OK {
            return status;
        }
    }
    ERR_OK
}

fn fnv1a64(text: &str) -> u64 {
    const OFFSET: u64 = 0xcbf29ce484222325;
    const PRIME: u64 = 0x100000001b3;
    let mut hash = OFFSET;
    for b in text.as_bytes() {
        hash ^= *b as u64;
        hash = hash.wrapping_mul(PRIME);
    }
    hash
}

fn write_all(path: &Path, bytes: &[u8]) -> u32 {
    match fs::write(path, bytes) {
        Ok(_) => 0,
        Err(_) => 10,
    }
}

fn ensure_parent(path: &Path) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
}

fn escape_depfile_token(token: &str) -> String {
    let mut out = String::new();
    for ch in token.chars() {
        if ch == ' ' || ch == '#' {
            out.push('\\');
            out.push(ch);
        } else if ch == '$' {
            out.push('$');
            out.push('$');
        } else {
            out.push(ch);
        }
    }
    out
}

fn write_dependency_file_for_state(state: &LinkerState) -> u32 {
    if state.dependency_file == 0 {
        return ERR_OK;
    }
    let dep_path = match to_path(state.dependency_file) {
        Some(v) => v,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&dep_path);
    let output_text = to_path(state.output_path)
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| DEFAULT_OUTPUT_NAME.to_string());

    let mut line = String::new();
    line.push_str(&escape_depfile_token(&output_text));
    line.push(':');

    for handle in &state.inputs {
        if let Some(text) = read_c_string(*handle) {
            let trimmed = text.trim();
            if !trimmed.is_empty() {
                line.push(' ');
                line.push_str(&escape_depfile_token(trimmed));
            }
        }
    }
    for handle in &state.archives {
        if let Some(text) = read_c_string(*handle) {
            let trimmed = text.trim();
            if !trimmed.is_empty() {
                line.push(' ');
                line.push_str(&escape_depfile_token(trimmed));
            }
        }
    }
    line.push('\n');
    match fs::write(dep_path, line) {
        Ok(_) => ERR_OK,
        Err(_) => ERR_WRITE_FAILED,
    }
}

fn build_map_rows(state: &LinkerState) -> Vec<String> {
    let mut rows = build_alloc_segments(state)
        .into_iter()
        .enumerate()
        .map(|(i, (addr, bytes, obj, sec))| {
            format!(
                "#{:04} obj={} sec={} addr=0x{:016x} size=0x{:x}",
                i, obj, sec, addr, bytes.len()
            )
        })
        .collect::<Vec<_>>();

    if state.emit_relocs {
        let mut row_index = rows.len();
        for (obj_index, object) in state.objects.iter().enumerate() {
            for reloc in &object.relocations {
                rows.push(format!(
                    "#{:04} reloc obj={} sec={} off=0x{:x} type={} sym={} addend=0x{:x}",
                    row_index,
                    obj_index,
                    reloc.section,
                    reloc.offset,
                    reloc.reloc_type,
                    reloc.symbol,
                    reloc.addend
                ));
                row_index += 1;
            }
        }
    }

    if rows.is_empty() {
        rows.push(String::new());
    }
    rows
}

#[no_mangle]
pub extern "C" fn host_args_count() -> u32 {
    refresh_args();
    let cache = args().lock().expect("args mutex poisoned");
    cache.len() as u32
}

#[no_mangle]
pub extern "C" fn host_arg_at(index: u32) -> u64 {
    refresh_args();
    let cache = args().lock().expect("args mutex poisoned");
    cache
        .get(index as usize)
        .map(|s| s.as_ptr() as u64)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_args() -> u64 {
    0
}

#[no_mangle]
pub extern "C" fn host_str_after_prefix(text: u64, prefix: u64) -> u64 {
    let a = as_string_or_number(text);
    let b = as_string_or_number(prefix);
    if let Some(rest) = a.strip_prefix(&b) {
        if rest.is_empty() {
            0
        } else {
            intern_string(rest)
        }
    } else {
        0
    }
}

#[no_mangle]
pub extern "C" fn host_print_line(text: u64) -> u32 {
    eprintln!("{}", as_string_or_number(text));
    ERR_OK
}

#[no_mangle]
pub extern "C" fn host_parse_u64(text: u64, base: u32) -> u64 {
    let raw = match read_c_string(text) {
        Some(v) => v.trim().to_string(),
        None => return 0,
    };
    if raw.is_empty() {
        return 0;
    }

    let parsed = if base == 0 {
        if let Some(hex) = raw.strip_prefix("0x").or_else(|| raw.strip_prefix("0X")) {
            u64::from_str_radix(hex, 16).ok()
        } else if let Some(bin) = raw.strip_prefix("0b").or_else(|| raw.strip_prefix("0B")) {
            u64::from_str_radix(bin, 2).ok()
        } else if let Some(oct) = raw.strip_prefix("0o").or_else(|| raw.strip_prefix("0O")) {
            u64::from_str_radix(oct, 8).ok()
        } else {
            raw.parse::<u64>().ok()
        }
    } else {
        u64::from_str_radix(&raw, base).ok()
    };
    parsed.unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_path_exists(path: u64) -> u32 {
    to_path(path).map(|p| p.exists() as u32).unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_path_is_file(path: u64) -> u32 {
    to_path(path)
        .map(|p| p.is_file() as u32)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_path_join(base: u64, leaf: u64) -> u64 {
    let left = as_string_or_number(base);
    let right = as_string_or_number(leaf);
    let joined = Path::new(&left).join(right);
    intern_string(joined.to_string_lossy().to_string())
}

#[no_mangle]
pub extern "C" fn host_str_concat(lhs: u64, rhs: u64) -> u64 {
    let text = format!("{}{}", as_string_or_number(lhs), as_string_or_number(rhs));
    intern_string(text)
}

#[no_mangle]
pub extern "C" fn host_str_eq(lhs: u64, rhs: u64) -> u32 {
    if lhs == rhs {
        return 1;
    }
    let l = as_string_or_number(lhs);
    let r = as_string_or_number(rhs);
    (l == r) as u32
}

#[no_mangle]
pub extern "C" fn host_str_ends_with(text: u64, suffix: u64) -> u32 {
    let a = as_string_or_number(text);
    let b = as_string_or_number(suffix);
    (a.ends_with(&b)) as u32
}

#[no_mangle]
pub extern "C" fn host_str_starts_with(text: u64, prefix: u64) -> u32 {
    let a = as_string_or_number(text);
    let b = as_string_or_number(prefix);
    (a.starts_with(&b)) as u32
}

#[no_mangle]
pub extern "C" fn host_str_hash64(text: u64) -> u64 {
    fnv1a64(&as_string_or_number(text))
}

#[no_mangle]
pub extern "C" fn host_fs_file_size(path: u64) -> u64 {
    to_path(path)
        .and_then(|p| fs::metadata(p).ok())
        .map(|m| m.len())
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_fs_read_u8(path: u64, offset: u64) -> u8 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    let mut f = match OpenOptions::new().read(true).open(p) {
        Ok(v) => v,
        Err(_) => return 0,
    };
    if f.seek(SeekFrom::Start(offset)).is_err() {
        return 0;
    }
    let mut b = [0u8; 1];
    if f.read_exact(&mut b).is_err() {
        return 0;
    }
    b[0]
}

#[no_mangle]
pub extern "C" fn host_fs_write_u8(path: u64, offset: u64, value: u8) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 10,
    };
    ensure_parent(&p);
    let mut f = match OpenOptions::new().create(true).read(true).write(true).open(&p) {
        Ok(v) => v,
        Err(_) => return 10,
    };
    if f.seek(SeekFrom::Start(offset)).is_err() {
        return 10;
    }
    if f.write_all(&[value]).is_err() {
        return 10;
    }
    0
}

#[no_mangle]
pub extern "C" fn host_fs_truncate(path: u64, size: u64) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 10,
    };
    ensure_parent(&p);
    match OpenOptions::new().create(true).write(true).open(&p) {
        Ok(f) => f.set_len(size).map(|_| 0).unwrap_or(10),
        Err(_) => 10,
    }
}

#[no_mangle]
pub extern "C" fn host_fs_append_line(path: u64, line: u64) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 10,
    };
    ensure_parent(&p);
    let content = as_string_or_number(line);
    let mut f = match OpenOptions::new().create(true).append(true).open(&p) {
        Ok(v) => v,
        Err(_) => return 10,
    };
    if writeln!(f, "{}", content).is_err() {
        return 10;
    }
    0
}

#[no_mangle]
pub extern "C" fn host_archive_validate_magic(path: u64) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    match parse_archive_members(&p) {
        Some(_) => 1,
        None => 0,
    }
}

#[no_mangle]
pub extern "C" fn host_archive_member_count(path: u64) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    parse_archive_members(&p)
        .map(|m| m.len() as u32)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_archive_member_is_elf(path: u64, index: u32) -> u32 {
    (host_archive_member_object_kind(path, index) == OBJECT_FORMAT_ELF64) as u32
}

#[no_mangle]
pub extern "C" fn host_archive_member_object_kind(path: u64, index: u32) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return OBJECT_FORMAT_UNKNOWN,
    };
    let raw = match fs::read(&p) {
        Ok(v) => v,
        Err(_) => return OBJECT_FORMAT_UNKNOWN,
    };
    let member = match archive_member_at(&p, index as usize) {
        Some(v) => v,
        None => return OBJECT_FORMAT_UNKNOWN,
    };
    let end = member.data_offset.saturating_add(member.data_size);
    if end > raw.len() || member.data_size == 0 {
        return OBJECT_FORMAT_UNKNOWN;
    }
    let payload = &raw[member.data_offset..end];
    probe_object_format_bytes(payload)
}

#[no_mangle]
pub extern "C" fn host_archive_member_matches_unresolved(path: u64, index: u32) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    let state = linker().lock().expect("linker mutex poisoned");
    archive_member_matches_unresolved_state(&state, &p, index)
}

#[no_mangle]
pub extern "C" fn host_archive_member_is_loaded(path: u64, index: u32) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    let key = archive_member_key(&p, index);
    let state = linker().lock().expect("linker mutex poisoned");
    if state.loaded_archive_members.contains(&key) {
        1
    } else {
        0
    }
}

#[no_mangle]
pub extern "C" fn host_archive_member_mark_loaded(path: u64, index: u32) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    let key = archive_member_key(&p, index);
    let mut state = linker().lock().expect("linker mutex poisoned");
    if state.loaded_archive_members.insert(key) {
        1
    } else {
        0
    }
}

#[no_mangle]
pub extern "C" fn host_archive_extract_member_object(path: u64, index: u32) -> u64 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return 0,
    };
    let raw = match fs::read(&p) {
        Ok(v) => v,
        Err(_) => return 0,
    };
    let member = match archive_member_at(&p, index as usize) {
        Some(v) => v,
        None => return 0,
    };
    let end = member.data_offset.saturating_add(member.data_size);
    if end > raw.len() || member.data_size == 0 {
        return 0;
    }
    let payload = &raw[member.data_offset..end];
    if probe_object_format_bytes(payload) == OBJECT_FORMAT_UNKNOWN {
        return 0;
    }

    let stem = p
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("archive")
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect::<String>();
    let member_tag = member
        .name
        .chars()
        .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
        .collect::<String>();
    let temp_dir = std::env::temp_dir().join("dustlink_ar_extract");
    let _ = fs::create_dir_all(&temp_dir);
    let out_path = temp_dir.join(format!("{}_{}_{}.o", stem, index, member_tag));
    if fs::write(&out_path, payload).is_err() {
        return 0;
    }

    let mut state = linker().lock().expect("linker mutex poisoned");
    state.extracted_members.push(out_path.clone());
    drop(state);
    intern_string(out_path.to_string_lossy().to_string())
}

#[no_mangle]
pub extern "C" fn host_linker_set_output_path(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.output_path = path;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_output_path() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.output_path
}

#[no_mangle]
pub extern "C" fn host_linker_set_output_format(format: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.output_format = format;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_output_format() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.output_format
}

#[no_mangle]
pub extern "C" fn host_linker_set_target(target: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.target = target;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_target() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.target
}

#[no_mangle]
pub extern "C" fn host_linker_set_entry(entry: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.entry = entry;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_entry() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.entry
}

#[no_mangle]
pub extern "C" fn host_linker_set_image_base(base: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.image_base = base;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_image_base() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.image_base
}

#[no_mangle]
pub extern "C" fn host_linker_set_gc_sections(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.gc_sections = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_gc_sections() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.gc_sections { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_allow_multiple_definition(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.allow_multiple_definition = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_allow_multiple_definition() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.allow_multiple_definition { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_enable_build_id_default() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.build_id_mode = BUILD_ID_MODE_FAST;
    state.build_id_bytes.clear();
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_set_build_id(spec: u64) -> u32 {
    let text = match read_c_string(spec) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let (mode, bytes) = match parse_build_id_mode_and_bytes(&text) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.build_id_mode = mode;
    state.build_id_bytes = bytes;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_build_id_mode() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.build_id_mode
}

#[no_mangle]
pub extern "C" fn host_linker_set_z_option(option: u64) -> u32 {
    let text = match read_c_string(option) {
        Some(v) => v.trim().to_ascii_lowercase(),
        None => return ERR_INVALID_FORMAT,
    };
    if text.is_empty() {
        return ERR_INVALID_FORMAT;
    }
    let mut state = linker().lock().expect("linker mutex poisoned");
    match text.as_str() {
        "relro" => state.z_relro = true,
        "norelro" => state.z_relro = false,
        "now" => state.z_now = true,
        "lazy" => state.z_now = false,
        "execstack" => state.z_execstack = true,
        "noexecstack" => state.z_execstack = false,
        "defs" => state.no_undefined = true,
        "undefs" => state.no_undefined = false,
        // Accepted compatibility spellings that currently do not change emit policy.
        "text" | "notext" | "origin" => {}
        _ => return set_last_error(&mut state, ERR_UNSUPPORTED_FLAG),
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_z_flags() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    let mut flags = 0u32;
    if state.z_relro {
        flags |= 1;
    }
    if state.z_now {
        flags |= 2;
    }
    if state.z_execstack {
        flags |= 4;
    }
    flags
}

#[no_mangle]
pub extern "C" fn host_linker_apply_defsym(spec: u64) -> u32 {
    let text = match read_c_string(spec) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let (name_hash, value) = match parse_defsym_spec(&text) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    if !state.allow_multiple_definition {
        if let Some(existing) = state.globals.get(&name_hash) {
            if existing.defined == 1 {
                return set_last_error(&mut state, ERR_MULTIPLE_DEFINITION);
            }
        }
    }
    state.globals.insert(
        name_hash,
        GlobalSymbol {
            object_index: 0,
            symbol_index: 0,
            bind: 1,
            defined: 1,
            address: value,
        },
    );
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_require_symbol(name: u64) -> u32 {
    let text = match read_c_string(name) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return ERR_INVALID_FORMAT;
    }
    let mut state = linker().lock().expect("linker mutex poisoned");
    require_symbol_hash(&mut state, fnv1a64(trimmed));
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_check_required_symbols() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let required_list = state.required_symbols.clone();
    for required in &required_list {
        let defined = state
            .globals
            .get(required)
            .map(|g| g.defined == 1)
            .unwrap_or(false);
        if !defined {
            return set_last_error(&mut state, ERR_UNDEFINED_SYMBOL);
        }
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_required_symbol_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.required_symbols.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_reset_state() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    reset_linker_state_defaults(&mut state);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_set_arch(arch: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.arch = arch;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_set_os(os: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.os = os;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_register_input(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.inputs.push(path);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_register_archive(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.archives.push(path);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_group_push_start() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let archive_count = state.archives.len() as u32;
    state.group_archive_starts.push(archive_count);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_group_pop_start() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.group_archive_starts.pop().unwrap_or(u32::MAX)
}

#[no_mangle]
pub extern "C" fn host_linker_group_depth() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.group_archive_starts.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_add_search_path(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.search_paths.push(path);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_set_sysroot(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.sysroot_path = path;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_sysroot() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.sysroot_path
}

#[no_mangle]
pub extern "C" fn host_linker_add_rpath(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.rpaths.push(path);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_rpath_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.rpaths.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_get_rpath(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.rpaths.get(index as usize).copied().unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_add_rpath_link(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.rpath_links.push(path);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_rpath_link_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.rpath_links.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_get_rpath_link(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.rpath_links.get(index as usize).copied().unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_default_search_path_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    let sysroot = if state.sysroot_path != 0 {
        read_c_string(state.sysroot_path).map(PathBuf::from)
    } else {
        None
    };
    linker_default_search_paths(state.target, sysroot.as_deref()).len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_get_default_search_path(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    let sysroot = if state.sysroot_path != 0 {
        read_c_string(state.sysroot_path).map(PathBuf::from)
    } else {
        None
    };
    let paths = linker_default_search_paths(state.target, sysroot.as_deref());
    if let Some(path) = paths.get(index as usize) {
        intern_string(path)
    } else {
        0
    }
}

#[no_mangle]
pub extern "C" fn host_linker_set_shared(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.link_shared = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_shared() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.link_shared { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_pie(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.link_pie = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_pie() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.link_pie { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_static(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.link_static = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_static() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.link_static { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_as_needed(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.link_as_needed = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_as_needed() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.link_as_needed { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_new_dtags(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.link_new_dtags = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_new_dtags() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.link_new_dtags { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_copy_dt_needed_entries(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.link_copy_dt_needed_entries = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_copy_dt_needed_entries() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.link_copy_dt_needed_entries { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_hash_style(style: u64) -> u32 {
    let raw = match read_c_string(style) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let parsed = match parse_hash_style_value(&raw) {
        Some(v) => v,
        None => return ERR_INVALID_FORMAT,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.hash_style = parsed;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_hash_style() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.hash_style
}

#[no_mangle]
pub extern "C" fn host_linker_set_thread_count(count: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.thread_count = count;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_thread_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.thread_count
}

#[no_mangle]
pub extern "C" fn host_linker_set_eh_frame_hdr(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.emit_eh_frame_hdr = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_set_fatal_warnings(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.fatal_warnings = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_fatal_warnings() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.fatal_warnings { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_color_diagnostics(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.color_diagnostics = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_color_diagnostics() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.color_diagnostics { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_print_gc_sections(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.print_gc_sections = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_print_gc_sections() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.print_gc_sections { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_dependency_file(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.dependency_file = path;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_dependency_file() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.dependency_file
}

#[no_mangle]
pub extern "C" fn host_linker_write_dependency_file() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let status = write_dependency_file_for_state(&state);
    set_last_error(&mut state, status)
}

#[no_mangle]
pub extern "C" fn host_linker_set_emit_relocs(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.emit_relocs = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_emit_relocs() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.emit_relocs { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_icf_mode(mode: u32) -> u32 {
    if mode > ICF_MODE_ALL {
        return ERR_INVALID_FORMAT;
    }
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.icf_mode = mode;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_icf_mode() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.icf_mode
}

#[no_mangle]
pub extern "C" fn host_linker_set_pe_no_entry(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.pe_no_entry = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_pe_no_entry() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.pe_no_entry { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_pe_dynamic_base(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.pe_dynamic_base = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_pe_dynamic_base() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.pe_dynamic_base { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_pe_nx_compat(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.pe_nx_compat = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_pe_nx_compat() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.pe_nx_compat { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_pe_large_address_aware(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.pe_large_address_aware = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_pe_large_address_aware() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.pe_large_address_aware { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_whole_archive(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.whole_archive = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_whole_archive() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.whole_archive { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_set_no_undefined(enabled: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.no_undefined = enabled != 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_no_undefined() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.no_undefined { 1 } else { 0 }
}

#[no_mangle]
pub extern "C" fn host_linker_allow_dynamic_unresolved() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.no_undefined {
        return 0;
    }
    if state.link_static {
        return 0;
    }
    if state.needed_shared_libs.is_empty() {
        return 0;
    }
    1
}

#[no_mangle]
pub extern "C" fn host_linker_set_dynamic_linker(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.dynamic_linker_path = path;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_dynamic_linker() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.dynamic_linker_path
}

#[no_mangle]
pub extern "C" fn host_linker_set_soname(name: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.soname = name;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_get_soname() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.soname
}

#[no_mangle]
pub extern "C" fn host_linker_note_needed_library(requested: u64, resolved: u64, contributed: u32) -> u32 {
    let requested_text = read_c_string(requested).unwrap_or_default();
    let resolved_text = read_c_string(resolved).unwrap_or_default();
    let mut state = linker().lock().expect("linker mutex poisoned");
    if state.link_static {
        return set_last_error(&mut state, ERR_OK);
    }
    if state.link_as_needed && contributed == 0 {
        return set_last_error(&mut state, ERR_OK);
    }
    let normalized = match normalize_needed_library_name(&requested_text, &resolved_text) {
        Some(v) => v,
        None => return set_last_error(&mut state, ERR_INVALID_FORMAT),
    };
    add_needed_library_if_missing(&mut state, &normalized);
    append_copy_dt_needed_entries(&mut state, &resolved_text);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_needed_library_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.needed_shared_libs.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_get_needed_library(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .needed_shared_libs
        .get(index as usize)
        .copied()
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_search_path_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.search_paths.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_archive_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.archives.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_get_archive(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.archives.get(index as usize).copied().unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_archive_progress_reset() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.archive_progress = 0;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_archive_progress_add(delta: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.archive_progress = state.archive_progress.saturating_add(delta);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_archive_progress_get() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.archive_progress
}

#[no_mangle]
pub extern "C" fn host_linker_get_search_path(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .search_paths
        .get(index as usize)
        .copied()
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_ingest_cli_inputs() -> u32 {
    // CLI ingestion is primarily handled by Dust-side parser. Keep as a success path.
    let mut state = linker().lock().expect("linker mutex poisoned");
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_probe_object_format(path: u64) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return OBJECT_FORMAT_UNKNOWN,
    };
    probe_object_format_path(&p)
}

#[no_mangle]
pub extern "C" fn host_linker_ingest_coff_object(path: u64) -> u32 {
    let parsed = match parse_coff_object(path) {
        Ok(v) => v,
        Err(code) => return code,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.objects.push(parsed);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_ingest_macho_object(path: u64) -> u32 {
    let parsed = match parse_macho_object(path) {
        Ok(v) => v,
        Err(code) => return code,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.objects.push(parsed);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_ingest_shared_object(path: u64) -> u32 {
    let p = match to_path(path) {
        Some(v) => v,
        None => return ERR_FILE_NOT_FOUND,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    let status = ingest_shared_object_symbols(&mut state, &p);
    set_last_error(&mut state, status)
}

#[no_mangle]
pub extern "C" fn host_linker_object_begin(path: u64, file_size: u64, elf_type: u16, machine: u16) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.current_object = Some(ObjectRecord {
        path,
        file_size,
        elf_type,
        machine,
        ..ObjectRecord::default()
    });
    state.active_patch_object = None;
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_object_add_section(
    index: u32,
    section_type: u32,
    flags: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    align: u64,
    entsize: u64,
) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    if let Some(current) = state.current_object.as_mut() {
        let data = match section_payload_from_object(current.path, section_type, offset, size) {
            Some(v) => v,
            None => return set_last_error(&mut state, ERR_INVALID_SECTION),
        };
        current.sections.push(ObjectSection {
            index,
            section_type,
            flags,
            offset,
            size,
            link,
            info,
            align,
            entsize,
            data,
        });
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_object_add_symbol(
    name_hash: u64,
    bind: u8,
    sym_type: u8,
    shndx: u32,
    value: u64,
    size: u64,
    strtab_section: u32,
) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    if let Some(current) = state.current_object.as_mut() {
        let resolved_hash = symbol_name_hash_from_strtab(current, name_hash, strtab_section, name_hash);
        current.symbols.push(ObjectSymbol {
            name_hash: resolved_hash,
            bind,
            sym_type,
            shndx: shndx as u16,
            value,
            size,
            strtab_section,
        });
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_object_add_relocation(
    section: u32,
    offset: u64,
    reloc_type: u32,
    symbol: u32,
    addend: u64,
) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    if let Some(current) = state.current_object.as_mut() {
        current.relocations.push(ObjectRelocation {
            section,
            offset,
            reloc_type,
            symbol,
            addend,
        });
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_object_finalize(_path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    if let Some(obj) = state.current_object.take() {
        state.objects.push(obj);
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_object_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.objects.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_object_machine(object_index: u32) -> u16 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .map(|o| o.machine)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_symbol_count(object_index: u32) -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .map(|o| o.symbols.len() as u32)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_symbol_name_hash(object_index: u32, symbol_index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.symbols.get(symbol_index as usize))
        .map(|s| s.name_hash)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_symbol_defined(object_index: u32, symbol_index: u32) -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.symbols.get(symbol_index as usize))
        .map(|s| (s.shndx != 0) as u32)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_symbol_bind(object_index: u32, symbol_index: u32) -> u8 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.symbols.get(symbol_index as usize))
        .map(|s| s.bind)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_relocation_count(object_index: u32) -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .map(|o| o.relocations.len() as u32)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_relocation_section(object_index: u32, reloc_index: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.active_patch_object = Some(object_index);
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.relocations.get(reloc_index as usize))
        .map(|r| r.section)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_relocation_offset(object_index: u32, reloc_index: u32) -> u64 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.active_patch_object = Some(object_index);
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.relocations.get(reloc_index as usize))
        .map(|r| r.offset)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_relocation_type(object_index: u32, reloc_index: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.active_patch_object = Some(object_index);
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.relocations.get(reloc_index as usize))
        .map(|r| r.reloc_type)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_relocation_symbol(object_index: u32, reloc_index: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.active_patch_object = Some(object_index);
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.relocations.get(reloc_index as usize))
        .map(|r| r.symbol)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_relocation_addend(object_index: u32, reloc_index: u32) -> u64 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.active_patch_object = Some(object_index);
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.relocations.get(reloc_index as usize))
        .map(|r| r.addend)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_object_section_size(object_index: u32, section_index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .get(object_index as usize)
        .and_then(|o| o.sections.iter().find(|s| s.index == section_index))
        .map(|s| s.size)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_global_symbol_defined(name_hash: u64) -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .globals
        .get(&name_hash)
        .map(|s| s.defined)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_global_symbol_bind(name_hash: u64) -> u8 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .globals
        .get(&name_hash)
        .map(|s| s.bind)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_global_symbol_address(name_hash: u64) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .globals
        .get(&name_hash)
        .map(|s| s.address)
        .unwrap_or(0)
}

#[no_mangle]
pub extern "C" fn host_linker_global_symbol_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.globals.len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_global_symbol_define_absolute(name_hash: u64, value: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.globals.insert(
        name_hash,
        GlobalSymbol {
            object_index: 0,
            symbol_index: 0,
            bind: 1,
            defined: 1,
            address: value,
        },
    );
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_global_symbol_set(
    name_hash: u64,
    object_index: u32,
    symbol_index: u32,
    bind: u8,
    defined: u32,
) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let address = symbol_runtime_address(&state, object_index, symbol_index);
    state.globals.insert(
        name_hash,
        GlobalSymbol {
            object_index,
            symbol_index,
            bind,
            defined,
            address,
        },
    );
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_unresolved_symbol_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    let mut unresolved = 0u32;
    for object in &state.objects {
        for symbol in &object.symbols {
            if symbol.shndx == SHN_UNDEF && symbol.bind != 0 {
                if symbol.name_hash == 0 {
                    continue;
                }
                if let Some(global) = state.globals.get(&symbol.name_hash) {
                    if global.defined == 1 {
                        continue;
                    }
                }
                unresolved = unresolved.saturating_add(1);
            }
        }
    }
    unresolved
}

#[no_mangle]
pub extern "C" fn host_linker_weak_fallback_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .globals
        .values()
        .filter(|s| s.bind == 2 && s.defined == 1)
        .count() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_resolved_symbol_address(object_index: u32, symbol_index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    symbol_runtime_address(&state, object_index, symbol_index)
}

#[no_mangle]
pub extern "C" fn host_linker_section_runtime_address(object_index: u32, section_index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    build_section_runtime_address(&state, object_index, section_index)
}

#[no_mangle]
pub extern "C" fn host_linker_patch_u32(section_index: u32, offset: u64, value: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let object_index = state.active_patch_object.unwrap_or(0) as usize;
    let object = match state.objects.get_mut(object_index) {
        Some(v) => v,
        None => return set_last_error(&mut state, ERR_INVALID_RELOCATION),
    };
    let section = match object.sections.iter_mut().find(|s| s.index == section_index) {
        Some(v) => v,
        None => return set_last_error(&mut state, ERR_INVALID_SECTION),
    };
    let at = offset as usize;
    if at + 4 > section.data.len() {
        return set_last_error(&mut state, ERR_INVALID_RELOCATION);
    }
    let le = (value as u32).to_le_bytes();
    section.data[at..at + 4].copy_from_slice(&le);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_read_u32(section_index: u32, offset: u64) -> u64 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let object_index = state.active_patch_object.unwrap_or(0) as usize;
    let object = match state.objects.get(object_index) {
        Some(v) => v,
        None => {
            set_last_error(&mut state, ERR_INVALID_RELOCATION);
            return 0;
        }
    };
    let section = match object.sections.iter().find(|s| s.index == section_index) {
        Some(v) => v,
        None => {
            set_last_error(&mut state, ERR_INVALID_SECTION);
            return 0;
        }
    };
    let at = offset as usize;
    if at + 4 > section.data.len() {
        set_last_error(&mut state, ERR_INVALID_RELOCATION);
        return 0;
    }
    let value = u32::from_le_bytes([
        section.data[at],
        section.data[at + 1],
        section.data[at + 2],
        section.data[at + 3],
    ]) as u64;
    set_last_error(&mut state, ERR_OK);
    value
}

#[no_mangle]
pub extern "C" fn host_linker_patch_u64(section_index: u32, offset: u64, value: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let object_index = state.active_patch_object.unwrap_or(0) as usize;
    let object = match state.objects.get_mut(object_index) {
        Some(v) => v,
        None => return set_last_error(&mut state, ERR_INVALID_RELOCATION),
    };
    let section = match object.sections.iter_mut().find(|s| s.index == section_index) {
        Some(v) => v,
        None => return set_last_error(&mut state, ERR_INVALID_SECTION),
    };
    let at = offset as usize;
    if at + 8 > section.data.len() {
        return set_last_error(&mut state, ERR_INVALID_RELOCATION);
    }
    let le = value.to_le_bytes();
    section.data[at..at + 8].copy_from_slice(&le);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_total_symbol_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.objects.iter().map(|o| o.symbols.len() as u32).sum()
}

#[no_mangle]
pub extern "C" fn host_linker_total_relocation_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state
        .objects
        .iter()
        .map(|o| o.relocations.len() as u32)
        .sum()
}

#[no_mangle]
pub extern "C" fn host_linker_add_output_section(section: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.output_sections.push(section);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_output_section_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    if state.output_sections.is_empty() {
        let alloc_count: usize = state
            .objects
            .iter()
            .flat_map(|o| o.sections.iter())
            .filter(|s| s.flags & SHF_ALLOC != 0)
            .count();
        alloc_count as u32
    } else {
        state.output_sections.len() as u32
    }
}

#[no_mangle]
pub extern "C" fn host_linker_emit_output_section(_output: u64, section_index: u32) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let count = if state.output_sections.is_empty() {
        state
            .objects
            .iter()
            .flat_map(|o| o.sections.iter())
            .filter(|s| s.flags & SHF_ALLOC != 0)
            .count() as u32
    } else {
        state.output_sections.len() as u32
    };
    if section_index >= count {
        return set_last_error(&mut state, ERR_INVALID_SECTION);
    }
    // Final writers still perform complete section materialization; this call now validates the
    // requested output section stream index.
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_calculate_image_size() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    let image = build_flat_image_bytes(&state);
    let size = if image.is_empty() {
        4096
    } else {
        image.len().min(u32::MAX as usize) as u32
    };
    state.image_size = size;
    size
}

#[no_mangle]
pub extern "C" fn host_linker_allocate_image(size: u32) -> u64 {
    if size == 0 {
        return 0;
    }
    let boxed = vec![0u8; size as usize].into_boxed_slice();
    let ptr = boxed.as_ptr() as u64;
    std::mem::forget(boxed);
    ptr
}

#[no_mangle]
pub extern "C" fn host_linker_create_image_buffer(format: u32) -> u64 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.output_format = format;
    state.pending_elf_entry = 0;
    state.pending_elf_image_base = 0;
    host_linker_allocate_image(4096)
}

#[no_mangle]
pub extern "C" fn host_linker_write_flat_binary(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return 10,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    let mut bytes = build_flat_image_bytes(&state);
    if bytes.is_empty() {
        bytes.resize(host_linker_calculate_image_size().max(1) as usize, 0);
    }
    drop(state);
    write_all(&path, &bytes)
}

fn write_elf_output_for_state(
    path: &Path,
    state: &LinkerState,
    entry_override: u64,
    image_base_override: u64,
) -> u32 {
    let image = build_flat_image_bytes(state);
    let build_id = build_id_payload(state, &image);
    let execstack = state.z_execstack;
    let z_now = state.z_now;
    let new_dtags = state.link_new_dtags;
    let link_shared = state.link_shared;
    let link_pie = state.link_pie;
    let link_static = state.link_static;
    let et_type = if link_shared || link_pie { ET_DYN } else { ET_EXEC };
    let entry = if link_shared {
        0
    } else if entry_override != 0 {
        entry_override
    } else if state.pending_elf_entry != 0 {
        state.pending_elf_entry
    } else {
        state.entry
    };
    let image_base = if image_base_override != 0 {
        image_base_override
    } else if state.pending_elf_image_base != 0 {
        state.pending_elf_image_base
    } else if et_type == ET_DYN {
        state.image_base
    } else {
        state.image_base.max(0x0010_0000)
    };
    let elf_machine = target_elf_machine(state.target);
    let dynamic_linker = if link_static || link_shared {
        None
    } else if state.dynamic_linker_path != 0 {
        read_c_string(state.dynamic_linker_path)
    } else if state.target == TARGET_AARCH64_LINUX {
        Some("/lib/ld-linux-aarch64.so.1".to_string())
    } else if target_is_linux(state.target) {
        Some("/lib64/ld-linux-x86-64.so.2".to_string())
    } else {
        None
    };
    let soname = if state.soname != 0 {
        read_c_string(state.soname)
    } else {
        None
    };
    let mut needed_libs = Vec::new();
    for handle in &state.needed_shared_libs {
        if let Some(name) = read_c_string(*handle) {
            let trimmed = name.trim().to_string();
            if !trimmed.is_empty() {
                needed_libs.push(trimmed);
            }
        }
    }
    let mut runpaths = Vec::new();
    for handle in &state.rpaths {
        if let Some(path_text) = read_c_string(*handle) {
            let trimmed = path_text.trim().to_string();
            if !trimmed.is_empty() {
                runpaths.push(trimmed);
            }
        }
    }
    let runpath = if link_static || runpaths.is_empty() {
        None
    } else {
        Some(runpaths.join(":"))
    };
    write_minimal_elf_exec(
        path,
        entry,
        image_base,
        &image,
        &build_id,
        execstack,
        et_type,
        elf_machine,
        dynamic_linker.as_deref(),
        soname.as_deref(),
        &needed_libs,
        runpath.as_deref(),
        new_dtags,
        z_now,
        state.hash_style,
    )
}

#[no_mangle]
pub extern "C" fn host_linker_write_elf_headers(output: u64, entry: u64, image_base: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return 10,
    };
    ensure_parent(&path);
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.pending_elf_entry = entry;
    state.pending_elf_image_base = image_base;
    write_elf_output_for_state(&path, &state, entry, image_base)
}

#[no_mangle]
pub extern "C" fn host_linker_finalize_elf(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    write_elf_output_for_state(&path, &state, 0, 0)
}

#[no_mangle]
pub extern "C" fn host_linker_write_pe_image(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    let chunks = build_output_chunks(&state);
    let entry = if state.pending_elf_entry != 0 {
        state.pending_elf_entry
    } else {
        state.entry
    };
    let image_base = if state.pending_elf_image_base != 0 {
        state.pending_elf_image_base
    } else if state.image_base != 0 {
        state.image_base
    } else {
        0x0000_0001_4000_0000u64
    };
    let machine = target_pe_machine(state.target);
    let pe_no_entry = state.pe_no_entry;
    let pe_dynamic_base = state.pe_dynamic_base;
    let pe_nx_compat = state.pe_nx_compat;
    let pe_large_address_aware = state.pe_large_address_aware;
    drop(state);
    write_minimal_pe_exec(
        &path,
        entry,
        image_base,
        &chunks,
        machine,
        pe_no_entry,
        pe_dynamic_base,
        pe_nx_compat,
        pe_large_address_aware,
    )
}

#[no_mangle]
pub extern "C" fn host_linker_write_macho_image(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    let chunks = build_output_chunks(&state);
    let entry = if state.pending_elf_entry != 0 {
        state.pending_elf_entry
    } else {
        state.entry
    };
    let image_base = if state.pending_elf_image_base != 0 {
        state.pending_elf_image_base
    } else if state.image_base != 0 {
        state.image_base
    } else {
        0x0000_0001_0000_0000u64
    };
    let (cpu_type, cpu_subtype) = target_macho_cpu_type(state.target);
    drop(state);
    write_minimal_macho_exec(&path, entry, image_base, &chunks, cpu_type, cpu_subtype)
}

#[no_mangle]
pub extern "C" fn host_linker_write_mbr_image(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return 10,
    };
    ensure_parent(&path);
    let mut bytes = vec![0u8; 512];
    bytes[510] = 0x55;
    bytes[511] = 0xaa;
    write_all(&path, &bytes)
}

#[no_mangle]
pub extern "C" fn host_linker_write_mbr_boot_image(output: u64, kernel: u64, kernel_size: u32) -> u32 {
    let out_path = match to_path(output) {
        Some(p) => p,
        None => return 10,
    };
    ensure_parent(&out_path);
    let mut bytes = vec![0u8; 512];
    bytes[510] = 0x55;
    bytes[511] = 0xaa;
    let mut payload = Vec::new();
    if let Some(kernel_path) = to_path(kernel) {
        if let Ok(data) = fs::read(kernel_path) {
            payload.extend_from_slice(&data);
        }
    }
    if payload.is_empty() && kernel_size > 0 {
        payload.resize(kernel_size as usize, 0);
    }
    bytes.extend_from_slice(&payload);
    write_all(&out_path, &bytes)
}

#[no_mangle]
pub extern "C" fn host_linker_write_efi_image(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return 10,
    };
    ensure_parent(&path);
    write_all(&path, b"EFI")
}

#[no_mangle]
pub extern "C" fn host_linker_finalize_image() -> u32 {
    ERR_OK
}

#[no_mangle]
pub extern "C" fn host_linker_format_map_row(index: u32) -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    let rows = build_map_rows(&state);
    if let Some(row) = rows.get(index as usize) {
        intern_string(row)
    } else {
        intern_string("")
    }
}

#[no_mangle]
pub extern "C" fn host_linker_map_row_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    build_map_rows(&state).len() as u32
}

#[no_mangle]
pub extern "C" fn host_linker_apply_script(script: u64) -> u32 {
    let p = match to_path(script) {
        Some(v) => v,
        None => return ERR_FILE_NOT_FOUND,
    };
    let mut state = linker().lock().expect("linker mutex poisoned");
    let status = apply_linker_script_file(&mut state, &p, 0);
    if status != ERR_OK {
        return set_last_error(&mut state, status);
    }
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_kernel_size(kernel: u64) -> u32 {
    host_fs_file_size(kernel).min(u32::MAX as u64) as u32
}

#[no_mangle]
pub extern "C" fn host_linker_last_error() -> u64 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.last_error
}

#[no_mangle]
pub extern "C" fn host_linker_dump_sections() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    eprintln!("dustlink host: {} object(s)", state.objects.len());
    0
}

#[no_mangle]
pub extern "C" fn host_linker_dump_symbols() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    let count: usize = state.objects.iter().map(|o| o.symbols.len()).sum();
    eprintln!("dustlink host: {} symbol(s)", count);
    0
}

#[no_mangle]
pub extern "C" fn host_linker_dump_relocations() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    let count: usize = state.objects.iter().map(|o| o.relocations.len()).sum();
    eprintln!("dustlink host: {} relocation(s)", count);
    0
}

#[no_mangle]
pub extern "C" fn host_linker_strip_debug() -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.strip_debug = true;
    set_last_error(&mut state, ERR_OK)
}
