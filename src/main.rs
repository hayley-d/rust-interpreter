use std::io::{self, Write};
use std::{env, process};

use codecrafters_interpreter::scanning::scanning::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    match command.as_str() {
        "tokenize" => {
            /*let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
                String::new()
            });*/

            let mut scanner = Scanner::new(filename.to_string());
            let errors = match scanner.scan_tokens() {
                Ok(t) => t,
                Err(e) => {
                    eprintln!("{e}");
                    return;
                }
            };

            for token in scanner.tokens {
                println!("{}", token);
            }

            if errors > 0 {
                process::exit(65);
            }
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
