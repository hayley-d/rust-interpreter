use clap::{Parser, Subcommand};
use miette::{Context, IntoDiagnostic};
use rust_interpreter::{lex, Lexer};
use std::fs;
use std::path::PathBuf;

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
    Run { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args: Args = Args::parse();

    match args.command {
        Commands::Tokenize { filename } => {
            let mut any_cc_err = false;

            // Use Miette WrapErr to turn non Miette error into a Miette error
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = match token {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{e:?}");
                        if let Some(unrecognized) = e.downcast_ref::<lex::SingleTokenError>() {
                            any_cc_err = true;
                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                unrecognized.line(),
                                unrecognized.token
                            );
                        } else if let Some(unterminated) =
                            e.downcast_ref::<lex::StringTerminationError>()
                        {
                            any_cc_err = true;
                            eprintln!("[line {}] Error: Unterminated string.", unterminated.line(),);
                        }
                        continue;
                    }
                };
                println!("{token}");
            }
            println!("EOF  null");

            if any_cc_err {
                std::process::exit(65);
            }
        }
        _ => (),
    }

    Ok(())
}
