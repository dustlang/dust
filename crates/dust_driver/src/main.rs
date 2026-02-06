use anyhow::{anyhow, Result};
use clap::{Parser, Subcommand};
use std::fs;
use std::path::{Path, PathBuf};

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
    /// Reserved (not implemented in v0.1 compiler core)
    Build {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Reserved (not implemented in v0.1 compiler core)
    Run {
        #[arg(default_value = ".")]
        path: PathBuf,
        #[arg(last = true)]
        args: Vec<String>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Check { path } => cmd_check(&path),
        Command::Dir { path, out, print } => cmd_dir(&path, &out, print),
        Command::Build { .. } => Err(anyhow!("dust build: not implemented (v0.1 emits DIR only)")),
        Command::Run { .. } => Err(anyhow!("dust run: not implemented (v0.1 emits DIR only)")),
    }
}

fn cmd_check(path: &Path) -> Result<()> {
    let files = collect_ds_files(path)?;
    if files.is_empty() {
        return Err(anyhow!("no .ds files found under {}", path.display()));
    }

    for f in files {
        let src = fs::read_to_string(&f)?;
        dust_semantics::parse_and_check(&src).map_err(|e| {
            anyhow!("{}: {} at {}..{}", f.display(), e.message, e.span.start, e.span.end)
        })?;
    }

    println!("OK");
    Ok(())
}

fn cmd_dir(path: &Path, out: &Path, print: bool) -> Result<()> {
    let files = collect_ds_files(path)?;
    if files.is_empty() {
        return Err(anyhow!("no .ds files found under {}", path.display()));
    }

    let mut programs = Vec::new();
    for f in files {
        let src = fs::read_to_string(&f)?;
        let ast = dust_semantics::parse_and_check(&src).map_err(|e| {
            anyhow!("{}: {} at {}..{}", f.display(), e.message, e.span.start, e.span.end)
        })?;
        let dir = dust_semantics::lower_to_dir(&ast);
        programs.push((f, dir));
    }

    fs::create_dir_all(out)?;

    for (path, dir) in programs {
        let name = path.file_stem().and_then(|s| s.to_str()).unwrap_or("module");
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

fn collect_ds_files(path: &Path) -> Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("ds") {
            out.push(path.to_path_buf());
        }
        return Ok(out);
    }

    for entry in walkdir::WalkDir::new(path) {
        let entry = entry?;
        if entry.file_type().is_file() {
            let p = entry.path();
            if p.extension().and_then(|s| s.to_str()) == Some("ds") {
                out.push(p.to_path_buf());
            }
        }
    }

    out.sort();
    Ok(out)
}