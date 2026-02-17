// crates/dust_driver/src/main.rs

use anyhow::{anyhow, bail, Context, Result};
use clap::{Parser, Subcommand};
use serde::Deserialize;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command as ProcCommand;

use dust_dir::DirProgram;

#[derive(Parser)]
#[command(name = "dust")]
#[command(version)]
#[command(about = "Dust Programming Language compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Parse + validate DPL (.ds) inputs
    Check {
        #[arg(default_value = ".")]
        path: PathBuf,
    },

    /// Emit canonical DIR JSON
    Dir {
        #[arg(default_value = ".")]
        path: PathBuf,
        #[arg(short, long, default_value = "target/dir")]
        out: PathBuf,
        #[arg(long)]
        print: bool,
    },

    /// Build a native executable (v0.1 executable subset)
    Build {
        /// Path to a single .ds file (or a directory containing exactly one .ds)
        #[arg(default_value = ".")]
        path: PathBuf,
        /// Output executable path (default: target/dust/<stem>[.exe])
        #[arg(short, long)]
        out: Option<PathBuf>,
    },

    /// Build an object file for linking (e.g., with dustlink)
    Obj {
        /// One or more .ds files and/or directories
        #[arg(required = true, num_args = 1..)]
        inputs: Vec<PathBuf>,
        /// Output object path (single source only)
        #[arg(short, long)]
        out: Option<PathBuf>,
        /// Output directory for multi-source object builds (default: target/dust/obj)
        #[arg(long)]
        out_dir: Option<PathBuf>,
        /// Entry proc to compile when targeting bare-metal objects (default: main)
        #[arg(long, default_value = "main")]
        entry: String,
        /// Target triple (default: host)
        /// Examples: x86_64-unknown-linux-gnu, x86_64-pc-none-elf
        #[arg(long)]
        target: Option<String>,
        /// Build for bare-metal kernel (no libc)
        /// Generates flat binary instead of ELF object
        #[arg(long)]
        bare_metal: bool,
        /// Auto-select first K proc when requested entry is missing (bare-metal object mode)
        #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
        auto_entry: bool,
        /// Skip *_tests.ds inputs (enabled by default)
        #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
        skip_tests: bool,
    },

    /// Build then run (v0.1 executable subset)
    Run {
        /// Path to a single .ds file (or a directory containing exactly one .ds)
        #[arg(default_value = ".")]
        path: PathBuf,
        /// Arguments passed to the program
        #[arg(last = true)]
        args: Vec<String>,
    },

    /// GCC-style multi-object kernel build + link for bare-metal DPL modules
    KernelLink {
        /// One or more .ds files and/or directories
        #[arg(required = true, num_args = 1..)]
        inputs: Vec<PathBuf>,
        /// Output kernel binary path (default: target/dust/kernel.bin)
        #[arg(short, long)]
        out: Option<PathBuf>,
        /// Intermediate object directory (default: target/dust/kernel_objs)
        #[arg(long)]
        obj_dir: Option<PathBuf>,
        /// Entry K proc name (default: main)
        #[arg(long, default_value = "main")]
        entry: String,
        /// Linked start symbol (default: _dust_kernel_start)
        #[arg(long, default_value = "_dust_kernel_start")]
        start_symbol: String,
        /// Target triple used for object emission
        #[arg(long, default_value = "x86_64-pc-none-elf")]
        target: String,
        /// External linker command (default: ld.lld)
        #[arg(long, default_value = "ld.lld")]
        linker: String,
        /// Skip *_tests.ds inputs (enabled by default)
        #[arg(long, default_value_t = true, action = clap::ArgAction::Set)]
        skip_tests: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Check { path } => cmd_check(&path),
        Command::Dir { path, out, print } => cmd_dir(&path, &out, print),
        Command::Build { path, out } => cmd_build(&path, out.as_deref()),
        Command::Obj {
            inputs,
            out,
            out_dir,
            entry,
            target,
            bare_metal,
            auto_entry,
            skip_tests,
        } => cmd_obj(
            &inputs,
            out.as_deref(),
            out_dir.as_deref(),
            &entry,
            target.as_deref(),
            bare_metal,
            auto_entry,
            skip_tests,
        ),
        Command::Run { path, args } => cmd_run(&path, &args),
        Command::KernelLink {
            inputs,
            out,
            obj_dir,
            entry,
            start_symbol,
            target,
            linker,
            skip_tests,
        } => cmd_kernel_link(
            &inputs,
            out.as_deref(),
            obj_dir.as_deref(),
            &entry,
            &start_symbol,
            &target,
            &linker,
            skip_tests,
        ),
    }
}

fn cmd_check(path: &Path) -> Result<()> {
    let files = collect_ds_files(path)?;
    if files.is_empty() {
        return Err(anyhow!(
            "no .ds or .dust files found under {}",
            path.display()
        ));
    }

    for f in files {
        let src = fs::read_to_string(&f)?;
        dust_semantics::parse_and_check(&src).map_err(|e| {
            anyhow!(
                "{}: {} at {}..{}",
                f.display(),
                e.message,
                e.span.start,
                e.span.end
            )
        })?;
    }

    println!("OK");
    Ok(())
}

fn cmd_dir(path: &Path, out: &Path, print: bool) -> Result<()> {
    let files = collect_ds_files(path)?;
    if files.is_empty() {
        return Err(anyhow!(
            "no .ds or .dust files found under {}",
            path.display()
        ));
    }

    let mut programs = Vec::new();
    for f in files {
        let src = fs::read_to_string(&f)?;
        let ast = dust_semantics::parse_and_check(&src).map_err(|e| {
            anyhow!(
                "{}: {} at {}..{}",
                f.display(),
                e.message,
                e.span.start,
                e.span.end
            )
        })?;
        let dir = dust_semantics::lower_to_dir(&ast);
        programs.push((f, dir));
    }

    fs::create_dir_all(out)?;

    for (path, dir) in programs {
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("module");
        let out_file = out.join(format!("{}.dir.json", name));
        let json = serde_json::to_string_pretty(&dir)?;
        fs::write(&out_file, &json)?;
        if print {
            println!("// {}", out_file.display());
            println!("{}", json);
        }
    }

    Ok(())
}

fn cmd_build(path: &Path, out: Option<&Path>) -> Result<()> {
    let file = single_ds_file(path)?;
    let src = fs::read_to_string(&file)?;
    let ast = dust_semantics::parse_and_check(&src).map_err(|e| {
        anyhow!(
            "{}: {} at {}..{}",
            file.display(),
            e.message,
            e.span.start,
            e.span.end
        )
    })?;

    let dir = dust_semantics::lower_to_dir(&ast);

    // Default output path: target/dust/<stem>
    let out_path = match out {
        Some(p) => p.to_path_buf(),
        None => default_out_path(&file)?,
    };

    // Ensure parent exists
    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let exe = dust_codegen::build_executable(&dir, &out_path)?;
    println!("{}", exe.display());
    Ok(())
}

fn cmd_obj(
    inputs: &[PathBuf],
    out: Option<&Path>,
    out_dir: Option<&Path>,
    entry: &str,
    target: Option<&str>,
    bare_metal: bool,
    auto_entry: bool,
    skip_tests: bool,
) -> Result<()> {
    let source_files = collect_ds_files_from_inputs(inputs, skip_tests)?;
    if source_files.is_empty() {
        bail!("obj: no source files found from provided inputs");
    }
    if out.is_some() && source_files.len() > 1 {
        bail!("obj: --out is only valid when exactly one source file is compiled");
    }

    let obj_root = if out.is_none() {
        let root = out_dir
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("target").join("dust").join("obj"));
        fs::create_dir_all(&root)?;
        Some(root)
    } else {
        None
    };

    for file in source_files {
        let src = fs::read_to_string(&file)?;
        let ast = dust_semantics::parse_and_check(&src).map_err(|e| {
            anyhow!(
                "{}: {} at {}..{}",
                file.display(),
                e.message,
                e.span.start,
                e.span.end
            )
        })?;
        let dir = dust_semantics::lower_to_dir(&ast);

        let out_path = match out {
            Some(p) => p.to_path_buf(),
            None => default_obj_output_path(&file, obj_root.as_ref().unwrap(), bare_metal),
        };
        if let Some(parent) = out_path.parent() {
            fs::create_dir_all(parent)?;
        }

        let result_path = emit_obj_for_source(
            &dir,
            &file,
            &out_path,
            entry,
            target,
            bare_metal,
            auto_entry,
        )?;
        println!("{}", result_path.display());
    }
    Ok(())
}

fn emit_obj_for_source(
    dir: &DirProgram,
    source: &Path,
    out_path: &Path,
    entry: &str,
    target: Option<&str>,
    bare_metal: bool,
    auto_entry: bool,
) -> Result<PathBuf> {
    if bare_metal {
        return dust_codegen::build_bare_metal_kernel(dir, out_path);
    }

    if let Some(t) = target {
        if is_bare_metal_target(t) {
            if let Ok(path) = dust_codegen::build_kernel_entry_object(dir, out_path, entry, t, true)
            {
                return Ok(path);
            }
            if auto_entry {
                if let Some(fallback_entry) = first_k_proc_name(dir) {
                    return dust_codegen::build_kernel_entry_object(
                        dir,
                        out_path,
                        &fallback_entry,
                        t,
                        true,
                    )
                    .with_context(|| {
                        format!(
                            "obj: failed to compile '{}' with fallback entry '{}'",
                            source.display(),
                            fallback_entry
                        )
                    });
                }
            }
            bail!(
                "obj: no K::{} found for bare-metal object compilation in {}",
                entry,
                source.display()
            );
        }
        return dust_codegen::build_object_file_for_target(dir, out_path, t);
    }

    dust_codegen::build_object_file(dir, out_path)
}

fn is_bare_metal_target(target: &str) -> bool {
    target.contains("none")
}

fn first_k_proc_name(dir: &DirProgram) -> Option<String> {
    for forge in &dir.forges {
        for proc in &forge.procs {
            if proc.regime == "K" {
                return Some(proc.name.clone());
            }
        }
    }
    None
}

fn default_obj_output_path(source: &Path, root: &Path, bare_metal: bool) -> PathBuf {
    let mut hasher = DefaultHasher::new();
    source.to_string_lossy().hash(&mut hasher);
    let hash = hasher.finish();
    let stem = source
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let safe_stem = stem
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || c == '_' || c == '-' {
                c
            } else {
                '_'
            }
        })
        .collect::<String>();
    let mut out = root.join(format!("{}_{:016x}", safe_stem, hash));
    out.set_extension(if bare_metal { "bin" } else { "o" });
    out
}

fn cmd_run(path: &Path, args: &[String]) -> Result<()> {
    let file = single_ds_file(path)?;
    let out_path = default_out_path(&file)?;

    // Build first
    cmd_build(&file, Some(&out_path))?;

    // Codegen returns platform-normalized exe path; mirror that logic here.
    let exe = if cfg!(windows) && out_path.extension().is_none() {
        let mut p = out_path.clone();
        p.set_extension("exe");
        p
    } else {
        out_path
    };

    let status = ProcCommand::new(&exe).args(args).status()?;
    if !status.success() {
        bail!("program exited with {}", status);
    }
    Ok(())
}

fn cmd_kernel_link(
    inputs: &[PathBuf],
    out: Option<&Path>,
    obj_dir: Option<&Path>,
    entry: &str,
    start_symbol: &str,
    target: &str,
    linker: &str,
    skip_tests: bool,
) -> Result<()> {
    eprintln!(
        "warning: 'dust kernel-link' is deprecated; use 'dust obj' and link with 'dustlink' instead"
    );

    let source_files = collect_ds_files_from_inputs(inputs, skip_tests)?;
    if source_files.is_empty() {
        bail!("kernel-link: no source files found from provided inputs");
    }

    let mut lowered: Vec<(PathBuf, DirProgram)> = Vec::new();
    for file in &source_files {
        let src = fs::read_to_string(file)?;
        let ast = dust_semantics::parse_and_check(&src).map_err(|e| {
            anyhow!(
                "{}: {} at {}..{}",
                file.display(),
                e.message,
                e.span.start,
                e.span.end
            )
        })?;
        let dir = dust_semantics::lower_to_dir(&ast);
        lowered.push((file.clone(), dir));
    }

    let merged_programs = lowered
        .iter()
        .map(|(_, dir)| dir.clone())
        .collect::<Vec<_>>();
    let merged = dust_codegen::merge_dir_programs(&merged_programs);

    let obj_root = obj_dir.map(Path::to_path_buf).unwrap_or_else(|| {
        PathBuf::from("target")
            .join("dust")
            .join("kernel_objs")
            .join("link")
    });
    fs::create_dir_all(&obj_root)?;

    let merged_entry_obj = dust_codegen::build_kernel_entry_object(
        &merged,
        &obj_root.join("__dust_kernel_entry.o"),
        entry,
        target,
        true,
    )?;

    let entry_symbol = entry
        .split("::")
        .filter(|s| !s.is_empty())
        .last()
        .unwrap_or(entry);

    let bootstrap_obj = dust_codegen::build_kernel_bootstrap_object(
        &obj_root.join("__dust_kernel_start.o"),
        target,
        start_symbol,
        entry_symbol,
    )?;

    let out_path = out
        .map(Path::to_path_buf)
        .unwrap_or_else(|| PathBuf::from("target").join("dust").join("kernel.bin"));
    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let mut link_objects = Vec::new();
    link_objects.push(bootstrap_obj.clone());
    link_objects.push(merged_entry_obj.clone());

    link_kernel_objects(&link_objects, &out_path, linker, start_symbol)?;

    println!("{}", out_path.display());
    Ok(())
}

fn collect_ds_files_from_inputs(inputs: &[PathBuf], skip_tests: bool) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    for input in inputs {
        out.extend(collect_ds_files(input)?);
    }

    out.sort();
    out.dedup();

    if skip_tests {
        out.retain(|p| !is_test_module(p));
    }
    Ok(out)
}

fn is_test_module(path: &Path) -> bool {
    path.file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.ends_with("_tests"))
        .unwrap_or(false)
}

fn link_kernel_objects(
    objects: &[PathBuf],
    out_path: &Path,
    linker: &str,
    start_symbol: &str,
) -> Result<()> {
    if objects.is_empty() {
        bail!("kernel-link: no objects to link");
    }

    let mut cmd = ProcCommand::new(linker);
    cmd.arg("-m")
        .arg("elf_x86_64")
        .arg("-nostdlib")
        .arg("--oformat=binary")
        .arg("--image-base")
        .arg("0x100000")
        .arg("-Ttext")
        .arg("0x100000")
        .arg("-e")
        .arg(start_symbol)
        .arg("-o")
        .arg(out_path);

    for object in objects {
        cmd.arg(object);
    }

    let out = cmd
        .output()
        .with_context(|| format!("invoke kernel linker '{}'", linker))?;

    if !out.status.success() {
        bail!(
            "kernel link failed: {}\nstdout:\n{}\nstderr:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
    }

    Ok(())
}

fn default_out_path(ds_file: &Path) -> Result<PathBuf> {
    let stem = ds_file
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| anyhow!("input has no valid file stem"))?;
    Ok(PathBuf::from("target").join("dust").join(stem))
}

/// For build/run, require exactly one .ds or .dust file.
fn single_ds_file(path: &Path) -> Result<PathBuf> {
    let files = collect_ds_files(path)?;
    match files.len() {
        0 => Err(anyhow!(
            "no .ds or .dust files found under {}",
            path.display()
        )),
        1 => Ok(files[0].clone()),
        n => Err(anyhow!(
            "expected exactly one .ds or .dust file for build/run, found {} under {}",
            n,
            path.display()
        )),
    }
}

fn collect_ds_files(path: &Path) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    if path.is_file() {
        if is_dust_file(path) {
            out.push(path.to_path_buf());
        }
        return Ok(out);
    }

    for entry in walkdir::WalkDir::new(path) {
        let entry = entry?;
        if entry.file_type().is_file() {
            let p = entry.path();
            if is_dust_file(p) {
                out.push(p.to_path_buf());
            }
        }
    }

    out.sort();
    Ok(out)
}

fn is_dust_file(path: &Path) -> bool {
    match path.extension().and_then(|s| s.to_str()) {
        Some("ds") | Some("dust") => true,
        _ => false,
    }
}

/// Read project configuration from State.toml
#[derive(Debug, Deserialize)]
pub struct StateToml {
    pub workspace: Option<WorkspaceConfig>,
    pub sector: Option<SectorConfig>,
}

#[derive(Debug, Deserialize)]
pub struct WorkspaceConfig {
    pub name: Option<String>,
    pub version: Option<String>,
    pub description: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct SectorConfig {
    pub name: Option<String>,
    pub version: Option<String>,
    pub description: Option<String>,
}

fn read_state_toml(path: &Path) -> Result<Option<StateToml>> {
    let state_path = path.join("State.toml");
    if !state_path.exists() {
        return Ok(None);
    }

    let content = fs::read_to_string(&state_path)?;
    let state: StateToml =
        toml::from_str(&content).map_err(|e| anyhow!("failed to parse State.toml: {}", e))?;
    Ok(Some(state))
}
