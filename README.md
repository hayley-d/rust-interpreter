# Lox Interpreter

This repository implements a Lox interpreter in Rust, based on the Pratt Parsing technique. The project consists of a lexer for tokenizing the input, a Pratt parser for syntax analysis, and an evaluator for interpreting the parsed abstract syntax tree (AST).

## Error Handling
The project employs the miette crate to provide robust error diagnostics across all components. This ensures that any error encountered during lexing, parsing, or evaluation is reported with detailed context and suggestions for resolution.

- **Lexer Errors:** Invalid tokens, unrecognized characters, and incomplete literals are reported with their location in the source code.

- **Parser Errors:** Syntax errors, such as missing parentheses or mismatched braces, include a labeled span and helpful messages.

- **Detailed Context:** Errors are annotated with line numbers, column positions, and previews of the surrounding code.

- **Actionable Suggestions:** Common issues include guidance to help developers resolve them quickly.

By leveraging miette, the interpreter not only catches errors but also provides developers with the tools to debug and fix issues effectively.

## Lexer

The Lexer is responsible for converting the raw source code into a stream of tokens, which the parser consumes. The Lexer is responsible for breaking down the source code into tokens, capturing their type, value, and position within the input. Each token represents a meaningful unit in the source code, such as keywords, identifiers, operators, or literals.

### Key Features of the Lexer:

- **Tokenizes** source code into tokens, capturing their type, value, and position.

- Handles different types of tokens like keywords (if, while, var), operators (+, -, *, /), and literals (numbers, strings, booleans).

- Efficiently iterates over input and skips irrelevant characters like whitespace and comments.

## Parser

The Parser uses a **Pratt Parsing** approach to process the tokens and produce a parse tree representing the syntactic structure of the source code. It can handle both prefix and infix expressions, with support for operator precedence and associativity.

#### Structure
The parser is implemented in the `Parser` struct:
```rust
pub struct Parser<'a> {
    input: &'a str,
    lexer: Lexer<'a>,
}
```

### Expression Parsing

The parser uses **binding power** to handle operator precedence and associativity. The `parse_statement_within` and `parse_expression_within` methods recursively parse subexpressions based on their binding power.

#### Operators are categorized with binding powers to manage precedence:

- Prefix operators (e.g., -, !) are parsed using `prefix_binding_power`.

- Postfix operators (e.g., function calls) are parsed using `postfix_binding_power`.

- Infix operators (e.g., +, -, *, /) are parsed using `infix_binding_power`.

### Abstract Syntax Tree (AST)

The `ParseTree` enum represents the structure of parsed code:

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum ParseTree<'a> {
    Literal(Literal<'a>),
    Cons(Operator, Vec<ParseTree<'a>>),
    Fun {
        name: Literal<'a>,
        parameters: Vec<Token<'a>>,
        body: Box<ParseTree<'a>>,
    },
    Call {
        callee: Box<ParseTree<'a>>,
        arguments: Vec<ParseTree<'a>>,
    },
    If {
        condition: Box<ParseTree<'a>>,
        yes: Box<ParseTree<'a>>,
        no: Option<Box<ParseTree<'a>>>,
    },
}
```

## References

- Nystrom, R., Crafting Interpreters. Available at: https://craftinginterpreters.com [Accessed 15 Dec. 2024].

- Matklad, 2020. Simple but powerful Pratt parsing. Available at: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html [Accessed 15 Dec. 2024].

- Gjengset, J., 2020. Implementing a Lox interpreter in Rust. YouTube. Available at: https://www.youtube.com/watch?v=mNOLaw-_Buc&t=2237s [Accessed 15 Dec. 2024].

- Nystrom, R., 2011. Pratt Parsers: Expression parsing made easy. Available at: https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/ [Accessed 15 Dec. 2024].

- LangDev Stack Exchange, 2024. What exactly is Pratt parsing used for and how does it work?. Available at: https://langdev.stackexchange.com/questions/3254/what-exactly-is-pratt-parsing-used-for-and-how-does-it-work [Accessed 15 Dec. 2024].
- Jrop, 2024. Pratt Parsing. Dev.to. Available at: https://dev.to/jrop/pratt-parsing [Accessed 15 Dec. 2024].



