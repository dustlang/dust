// crates/dust_driver/src/main.rs

use anyhow::{anyhow, bail, Result};
use clap::{Parser, Subcommand};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command as ProcCommand;

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
        /// Path to a single .ds file
        #[arg(default_value = ".")]
        path: PathBuf,
        /// Output object path (default: target/dust/<stem>.o)
        #[arg(short, long)]
        out: Option<PathBuf>,
        /// Target triple (default: host)
        /// Examples: x86_64-unknown-linux-gnu, x86_64-pc-none-elf
        #[arg(long)]
        target: Option<String>,
        /// Build for bare-metal kernel (no libc)
        /// Generates flat binary instead of ELF object
        #[arg(long)]
        bare_metal: bool,
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
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Check { path } => cmd_check(&path),
        Command::Dir { path, out, print } => cmd_dir(&path, &out, print),
        Command::Build { path, out } => cmd_build(&path, out.as_deref()),
        Command::Obj {
            path,
            out,
            target,
            bare_metal,
        } => cmd_obj(&path, out.as_deref(), target.as_deref(), bare_metal),
        Command::Run { path, args } => cmd_run(&path, &args),
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

fn cmd_obj(path: &Path, out: Option<&Path>, target: Option<&str>, bare_metal: bool) -> Result<()> {
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

    // Default output path
    let out_path = match out {
        Some(p) => p.to_path_buf(),
        None => {
            let mut p = default_out_path(&file)?;
            if bare_metal {
                p.set_extension("bin");
            } else {
                p.set_extension("o");
            }
            p
        }
    };

    // Ensure parent exists
    if let Some(parent) = out_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let result_path = if bare_metal {
        // Build bare-metal kernel (flat binary)
        dust_codegen::build_bare_metal_kernel(&dir, &out_path)?
    } else if let Some(t) = target {
        // Custom target
        dust_codegen::build_object_file_for_target(&dir, &out_path, t)?
    } else {
        // Default: host object file
        dust_codegen::build_object_file(&dir, &out_path)?
    };
    println!("{}", result_path.display());
    Ok(())
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
