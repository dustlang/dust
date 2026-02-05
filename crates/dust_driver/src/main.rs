use anyhow::Result;
use clap::{Parser, Subcommand};

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
    Check,
    Dir,
    Build,
    Run,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Command::Check => {
            println!("dust check (not yet implemented)");
        }
        Command::Dir => {
            println!("dust dir (not yet implemented)");
        }
        Command::Build => {
            println!("dust build (not yet implemented)");
        }
        Command::Run => {
            println!("dust run (not yet implemented)");
        }
    }

    Ok(())
}