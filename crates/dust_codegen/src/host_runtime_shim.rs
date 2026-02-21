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
const PT_LOAD: u32 = 1;
const EM_X86_64: u16 = 62;
const ET_EXEC: u16 = 2;
const EV_CURRENT: u32 = 1;

const FORMAT_ELF64: u32 = 1;
const FORMAT_BINARY: u32 = 2;
const FORMAT_MBR: u32 = 3;
const FORMAT_PE64: u32 = 4;
const FORMAT_MACHO64: u32 = 5;

const OBJECT_FORMAT_UNKNOWN: u32 = 0;
const OBJECT_FORMAT_ELF64: u32 = 1;
const OBJECT_FORMAT_COFF64: u32 = 2;
const OBJECT_FORMAT_MACHO64: u32 = 3;

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
    output_sections: Vec<u64>,
    image_size: u32,
    active_patch_object: Option<u32>,
    strip_debug: bool,
    gc_sections: bool,
    allow_multiple_definition: bool,
    pending_elf_entry: u64,
    pending_elf_image_base: u64,
    extracted_members: Vec<PathBuf>,
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
    LINKER.get_or_init(|| Mutex::new(LinkerState::default()))
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

fn write_minimal_elf_exec(path: &Path, entry: u64, image_base: u64, image_payload: &[u8]) -> u32 {
    let segment_offset = 0x1000usize;
    let payload = if image_payload.is_empty() {
        vec![0u8; 1]
    } else {
        image_payload.to_vec()
    };
    let total_size = segment_offset.saturating_add(payload.len());
    let mut out = vec![0u8; total_size];

    // ELF header.
    out[0] = 0x7f;
    out[1] = b'E';
    out[2] = b'L';
    out[3] = b'F';
    out[4] = 2; // 64-bit
    out[5] = 1; // little-endian
    out[6] = 1; // version
    write_u16_le(&mut out, 16, ET_EXEC);
    write_u16_le(&mut out, 18, EM_X86_64);
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
    write_u16_le(&mut out, 56, 1); // phnum
    write_u16_le(&mut out, 58, 0); // shentsize
    write_u16_le(&mut out, 60, 0); // shnum
    write_u16_le(&mut out, 62, 0); // shstrndx

    // PT_LOAD program header.
    let phoff = 64usize;
    write_u32_le(&mut out, phoff + 0, PT_LOAD);
    write_u32_le(&mut out, phoff + 4, 0x7); // RWX for MVP
    write_u64_le(&mut out, phoff + 8, segment_offset as u64);
    write_u64_le(&mut out, phoff + 16, image_base);
    write_u64_le(&mut out, phoff + 24, image_base);
    write_u64_le(&mut out, phoff + 32, payload.len() as u64);
    write_u64_le(&mut out, phoff + 40, payload.len() as u64);
    write_u64_le(&mut out, phoff + 48, 0x1000);

    out[segment_offset..segment_offset + payload.len()].copy_from_slice(&payload);
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
        if machine == 0x8664 {
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

fn map_coff_relocation_type(reloc_type: u16) -> u32 {
    match reloc_type {
        0 => 0,
        1 => 1,
        2 | 3 => 10,
        4 | 5 | 6 | 7 | 8 | 9 => 2,
        _ => 1,
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
    if machine != 0x8664 {
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
        machine: EM_X86_64,
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
                let reloc_type = map_coff_relocation_type(reloc_kind_raw);
                let symbol = if raw_symbol < raw_to_canonical.len() {
                    raw_to_canonical[raw_symbol]
                } else {
                    0
                };
                let section_ref = &record.sections[sec_idx + 1];
                let width = if reloc_type == 1 { 8 } else { 4 };
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
    if cpu_type != 0x0100_0007 {
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
        machine: EM_X86_64,
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
            let reloc_type = if pcrel == 1 {
                2
            } else if length == 3 {
                1
            } else {
                10
            };
            let section_ref = match record.sections.iter().find(|s| s.index == reloc.section_index) {
                Some(v) => v,
                None => continue,
            };
            let width = if reloc_type == 1 { 8 } else { 4 };
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

fn write_minimal_pe_exec(path: &Path, entry: u64, image_base: u64, image_payload: &[u8]) -> u32 {
    let payload = if image_payload.is_empty() {
        vec![0xC3u8]
    } else {
        image_payload.to_vec()
    };
    let file_align = 0x200u32;
    let section_align = 0x1000u32;
    let pe_offset = 0x80usize;
    let headers_raw_size = align_up_u32((pe_offset + 4 + 20 + 0xF0 + 40) as u32, file_align) as usize;
    let text_raw_size = align_up_u32(payload.len().max(1) as u32, file_align) as usize;
    let size_of_image = align_up_u32(0x1000u32.saturating_add(payload.len().max(1) as u32), section_align);

    let mut out = vec![0u8; headers_raw_size + text_raw_size];

    out[0] = b'M';
    out[1] = b'Z';
    write_u32_le(&mut out, 0x3c, pe_offset as u32);

    out[pe_offset..pe_offset + 4].copy_from_slice(b"PE\0\0");
    let coff = pe_offset + 4;
    write_u16_le(&mut out, coff + 0, 0x8664);
    write_u16_le(&mut out, coff + 2, 1);
    write_u32_le(&mut out, coff + 4, 0);
    write_u32_le(&mut out, coff + 8, 0);
    write_u32_le(&mut out, coff + 12, 0);
    write_u16_le(&mut out, coff + 16, 0xF0);
    write_u16_le(&mut out, coff + 18, 0x0022);

    let opt = coff + 20;
    write_u16_le(&mut out, opt + 0, 0x20b);
    let resolved_base = if image_base == 0 {
        0x0000_0001_4000_0000u64
    } else {
        image_base
    };
    let entry_rva = if entry >= resolved_base {
        (entry - resolved_base) as u32
    } else {
        0x1000
    };
    write_u32_le(&mut out, opt + 16, entry_rva.max(0x1000));
    write_u32_le(&mut out, opt + 20, 0x1000);
    write_u64_le(&mut out, opt + 24, resolved_base);
    write_u32_le(&mut out, opt + 32, section_align);
    write_u32_le(&mut out, opt + 36, file_align);
    write_u16_le(&mut out, opt + 48, 6);
    write_u16_le(&mut out, opt + 50, 0);
    write_u32_le(&mut out, opt + 56, size_of_image);
    write_u32_le(&mut out, opt + 60, headers_raw_size as u32);
    write_u16_le(&mut out, opt + 68, 3);
    write_u16_le(&mut out, opt + 70, 0x8140);
    write_u64_le(&mut out, opt + 72, 0x0010_0000);
    write_u64_le(&mut out, opt + 80, 0x0000_1000);
    write_u64_le(&mut out, opt + 88, 0x0010_0000);
    write_u64_le(&mut out, opt + 96, 0x0000_1000);
    write_u32_le(&mut out, opt + 108, 16);

    let sh = opt + 0xF0;
    out[sh + 0..sh + 8].copy_from_slice(b".text\0\0\0");
    write_u32_le(&mut out, sh + 8, payload.len().max(1) as u32);
    write_u32_le(&mut out, sh + 12, 0x1000);
    write_u32_le(&mut out, sh + 16, text_raw_size as u32);
    write_u32_le(&mut out, sh + 20, headers_raw_size as u32);
    write_u32_le(&mut out, sh + 36, 0x6000_0020);

    out[headers_raw_size..headers_raw_size + payload.len()].copy_from_slice(&payload);
    write_all(path, &out)
}

fn write_minimal_macho_exec(path: &Path, entry: u64, image_base: u64, image_payload: &[u8]) -> u32 {
    let payload = if image_payload.is_empty() {
        vec![0xC3u8]
    } else {
        image_payload.to_vec()
    };
    let header_size = 32usize;
    let segment_cmd_size = 72usize;
    let thread_cmd_size = 184usize;
    let sizeofcmds = segment_cmd_size + thread_cmd_size;
    let fileoff = header_size + sizeofcmds;
    let filesize = fileoff + payload.len();
    let vmaddr = if image_base == 0 {
        0x0000_0001_0000_0000u64
    } else {
        image_base
    };
    let entry_addr = if entry == 0 {
        vmaddr + fileoff as u64
    } else {
        entry
    };
    let vmsize = align_up(filesize as u64, 0x1000);

    let mut out = vec![0u8; filesize];
    write_u32_le(&mut out, 0, 0xfeedfacf);
    write_u32_le(&mut out, 4, 0x0100_0007);
    write_u32_le(&mut out, 8, 3);
    write_u32_le(&mut out, 12, 2);
    write_u32_le(&mut out, 16, 2);
    write_u32_le(&mut out, 20, sizeofcmds as u32);
    write_u32_le(&mut out, 24, 0);
    write_u32_le(&mut out, 28, 0);

    let seg = header_size;
    write_u32_le(&mut out, seg + 0, 0x19);
    write_u32_le(&mut out, seg + 4, segment_cmd_size as u32);
    out[seg + 8..seg + 14].copy_from_slice(b"__TEXT");
    write_u64_le(&mut out, seg + 24, vmaddr);
    write_u64_le(&mut out, seg + 32, vmsize);
    write_u64_le(&mut out, seg + 40, 0);
    write_u64_le(&mut out, seg + 48, filesize as u64);
    write_u32_le(&mut out, seg + 56, 7);
    write_u32_le(&mut out, seg + 60, 5);
    write_u32_le(&mut out, seg + 64, 0);
    write_u32_le(&mut out, seg + 68, 0);

    let thread = seg + segment_cmd_size;
    write_u32_le(&mut out, thread + 0, 0x5);
    write_u32_le(&mut out, thread + 4, thread_cmd_size as u32);
    write_u32_le(&mut out, thread + 8, 4);
    write_u32_le(&mut out, thread + 12, 42);

    let regs = thread + 16;
    write_u64_le(&mut out, regs + (7 * 8), vmaddr + vmsize - 16);
    write_u64_le(&mut out, regs + (16 * 8), entry_addr);
    write_u64_le(&mut out, regs + (17 * 8), 0x202);

    out[fileoff..fileoff + payload.len()].copy_from_slice(&payload);
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

fn apply_linker_script_statement(state: &mut LinkerState, statement: &str) {
    if let Some(arg) = extract_directive_arg(statement, "ENTRY") {
        if let Some(v) = parse_u64_auto(&arg) {
            state.entry = v;
        } else if !arg.is_empty() {
            let hash = fnv1a64(&arg);
            if let Some(global) = state.globals.get(&hash) {
                state.entry = global.address;
            }
        }
        return;
    }

    if let Some(arg) = extract_directive_arg(statement, "OUTPUT_FORMAT") {
        let normalized = arg.trim().to_ascii_lowercase();
        if normalized.contains("elf") {
            state.output_format = FORMAT_ELF64;
        } else if normalized.contains("pe") || normalized.contains("pei") {
            state.output_format = FORMAT_PE64;
        } else if normalized.contains("mach") {
            state.output_format = FORMAT_MACHO64;
        } else if normalized.contains("binary") || normalized == "bin" {
            state.output_format = FORMAT_BINARY;
        }
        return;
    }

    if let Some(arg) = extract_directive_arg(statement, "SEARCH_DIR") {
        if !arg.is_empty() {
            let handle = intern_string(arg);
            state.search_paths.push(handle);
        }
        return;
    }

    if let Some(arg) = extract_directive_arg(statement, "INPUT")
        .or_else(|| extract_directive_arg(statement, "GROUP"))
    {
        for token in arg
            .split(|c: char| c.is_ascii_whitespace() || c == ',')
            .map(trim_quotes)
            .filter(|t| !t.is_empty())
        {
            state.inputs.push(intern_string(token));
        }
    }
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
pub extern "C" fn host_linker_add_search_path(path: u64) -> u32 {
    let mut state = linker().lock().expect("linker mutex poisoned");
    state.search_paths.push(path);
    set_last_error(&mut state, ERR_OK)
}

#[no_mangle]
pub extern "C" fn host_linker_search_path_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    state.search_paths.len() as u32
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
pub extern "C" fn host_linker_emit_output_section(_output: u64, _section_index: u32) -> u32 {
    // Sections are emitted in a single pass by the final writer.
    ERR_OK
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
    // Prime output with a valid ELF ident immediately.
    let mut ident = vec![0u8; 64];
    ident[0] = 0x7f;
    ident[1] = b'E';
    ident[2] = b'L';
    ident[3] = b'F';
    ident[4] = 2;
    ident[5] = 1;
    ident[6] = 1;
    drop(state);
    write_all(&path, &ident)
}

#[no_mangle]
pub extern "C" fn host_linker_finalize_elf(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    let image = build_flat_image_bytes(&state);
    let entry = if state.pending_elf_entry != 0 {
        state.pending_elf_entry
    } else {
        state.entry
    };
    let image_base = if state.pending_elf_image_base != 0 {
        state.pending_elf_image_base
    } else {
        state.image_base.max(0x0010_0000)
    };
    drop(state);
    write_minimal_elf_exec(&path, entry, image_base, &image)
}

#[no_mangle]
pub extern "C" fn host_linker_write_pe_image(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    let image = build_flat_image_bytes(&state);
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
    drop(state);
    write_minimal_pe_exec(&path, entry, image_base, &image)
}

#[no_mangle]
pub extern "C" fn host_linker_write_macho_image(output: u64) -> u32 {
    let path = match to_path(output) {
        Some(p) => p,
        None => return ERR_WRITE_FAILED,
    };
    ensure_parent(&path);
    let state = linker().lock().expect("linker mutex poisoned");
    let image = build_flat_image_bytes(&state);
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
    drop(state);
    write_minimal_macho_exec(&path, entry, image_base, &image)
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
    let rows = build_alloc_segments(&state)
        .into_iter()
        .enumerate()
        .map(|(i, (addr, bytes, obj, sec))| {
            format!(
                "#{:04} obj={} sec={} addr=0x{:016x} size=0x{:x}",
                i, obj, sec, addr, bytes.len()
            )
        })
        .collect::<Vec<_>>();
    drop(state);
    if let Some(row) = rows.get(index as usize) {
        intern_string(row)
    } else {
        intern_string("")
    }
}

#[no_mangle]
pub extern "C" fn host_linker_map_row_count() -> u32 {
    let state = linker().lock().expect("linker mutex poisoned");
    let rows = build_alloc_segments(&state).len();
    rows.max(1) as u32
}

#[no_mangle]
pub extern "C" fn host_linker_apply_script(script: u64) -> u32 {
    let p = match to_path(script) {
        Some(v) => v,
        None => return ERR_FILE_NOT_FOUND,
    };
    if !p.exists() || !p.is_file() {
        return ERR_FILE_NOT_FOUND;
    }
    let raw = match fs::read_to_string(&p) {
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

    let mut state = linker().lock().expect("linker mutex poisoned");
    for statement in cleaned.split(';').flat_map(|s| s.split('\n')) {
        let stmt = statement.trim();
        if stmt.is_empty() {
            continue;
        }
        apply_linker_script_statement(&mut state, stmt);
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
