# Scanner Module

This module provides a **Scanner** (also known as a tokenizer or lexer) for parsing source code into a sequence of tokens. It is particularly useful for interpreting or compiling programming languages.

## Overview

The `Scanner` module reads a source file, processes its content line-by-line, and identifies various tokens such as keywords, operators, literals, and identifiers. It constructs a list of tokens that can be further used for syntax analysis or other stages of the compilation/interpreting process. The module is designed to be highly extensible and can be integrated into larger language processors or compilers.


## Key Features

- **Tokenization**: The `Scanner` breaks down a source file into meaningful tokens. It handles different types of tokens like keywords, operators, and identifiers.
- **Multi-line Comments**: The module supports both single-line (`//`) and multi-line (`/* ... */`) comments.
- **String Literals**: It can parse both single-line and multi-line string literals.
- **Error Handling**: The module uses the `anyhow` crate to handle and report errors, providing detailed information for common issues like unterminated strings or comments.
- **Custom Token Types**: Supports a variety of token types such as `Number`, `String`, `Keyword`, and various operators (`+`, `-`, `=`, etc.).

## Language Features Used

- **Result<T, E>**: Error handling is done using Rust's `Result` type, allowing the module to return success or failure with detailed error messages via the `anyhow` crate.
- **Pattern Matching**: Rust's powerful pattern matching is used extensively in the `scan_tokens` function to handle different types of characters and token categories.
- **String Manipulation**: Efficient string handling using `String` and `&str` allows the module to accumulate string literals and handle dynamic input.
- **Static Arrays**: Static arrays are used to define keywords, ensuring that only recognized keywords are accepted.
- **Enumerations (`enum`)**: The `TokenType` enum is used to define and categorize different types of tokens, making the code more maintainable and readable.
- **Iterators and `for` Loops**: Iterators and `for` loops are utilized to process the lines of the source file and individual characters in each line.
- **`Option<T>`**: Used for optional values, particularly for managing literals and errors when parsing different parts of the source.
## Usage

#### Creating a Scanner Instance
To create a new Scanner instance, provide the path to the source file:
```rust
let mut scanner = Scanner::new("source_code.txt".to_string());
```
#### Scanning Tokens
To scan tokens from the source file, use the scan_tokens method. It will return a result indicating the number of errors encountered:
```rust
let result = scanner.scan_tokens();
match result {
    Ok(errors) => println!("Scanning complete with {} errors.", errors),
    Err(e) => println!("Error: {}", e),
}

```
To use the `Scanner` module, simply create a new `Scanner` instance by providing the path to the source code file, and then call the `scan_tokens` method to perform tokenization.

### Example

```rust
use scanning::Scanner;

fn main() {
    let mut scanner = Scanner::new("path_to_source_code.rs".to_string());
    match scanner.scan_tokens() {
        Ok(error_count) => {
            if error_count == 0 {
                println!("Tokens scanned successfully:");
                for token in &scanner.tokens {
                    println!("{}", token);
                }
            } else {
                println!("Encountered {} errors during scanning.", error_count);
            }
        }
        Err(e) => eprintln!("Error scanning tokens: {}", e),
    }
}
