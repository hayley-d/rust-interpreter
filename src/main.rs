use std::io::{self, Write};
use std::{env, process};

use rust_interpreter::parser::Parser;
use rust_interpreter::scanning::scanning::Scanner;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    let mut scanner: Scanner;

    match command.as_str() {
        "tokenize" => {
            scanner = Scanner::new(filename.to_string());
            let errors = match scanner.scan_tokens() {
                Ok(t) => t,
                Err(e) => {
                    eprintln!("{e}");
                    return;
                }
            };

            for token in &scanner.tokens {
                println!("{}", token);
            }

            if errors > 0 {
                process::exit(65);
            }

            let paser: Parser = Parser::new(scanner.tokens);
        }
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
