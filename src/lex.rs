use core::fmt;
use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};
use std::borrow::Cow;
use std::fmt::Display;
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected character")]
pub struct LexError {
    message: String,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    RightParen,
    RightBrace,
    And,
    Bang,
    BangEqual,
    Class,
    Comma,
    Dot,
    Else,
    Equal,
    EqualEqual,
    False,
    For,
    Fun,
    Greater,
    GreaterEqual,
    Identifier,
    If,
    LessEqual,
    Less,
    LeftBrace,
    LeftParen,
    Minus,
    Number(f64),
    Nil,
    Or,
    Print,
    Plus,
    Return,
    Semicolon,
    Star,
    Slash,
    String,
    Super,
    This,
    True,
    Var,
    While,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token<'a> {
    pub lexeme: &'a str,
    pub line: u64,
    pub token_type: TokenType,
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let lexeme = self.lexeme;

        match self.token_type {
            TokenType::And => write!(f, "AND {lexeme} null"),
            TokenType::Bang => write!(f, "BANG {lexeme} null"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL {lexeme} null"),
            TokenType::Class => write!(f, "CLASS {lexeme} null"),
            TokenType::Comma => write!(f, "COMMA {lexeme} null"),
            TokenType::Dot => write!(f, "DOT {lexeme} null"),
            TokenType::Else => write!(f, "ELSE {lexeme} null"),
            TokenType::Equal => write!(f, "EQUAL {lexeme} null"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL {lexeme} null"),
            TokenType::False => write!(f, "FALSE {lexeme} null"),
            TokenType::For => write!(f, "FOR {lexeme} null"),
            TokenType::Fun => write!(f, "FUN {lexeme} null"),
            TokenType::Greater => write!(f, "GREATER {lexeme} null"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL {lexeme} null"),
            TokenType::Identifier => write!(f, "IDENTIFIER {lexeme} null"),
            TokenType::If => write!(f, "IF {lexeme} null"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE {lexeme} null"),
            TokenType::LeftParen => write!(f, "LEFT_PAREN {lexeme} null"),
            TokenType::Less => write!(f, "LESS {lexeme} null"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL {lexeme} null"),
            TokenType::Minus => write!(f, "MINUS {lexeme} null"),
            TokenType::Nil => write!(f, "NIL {lexeme} null"),
            TokenType::Number(n) => {
                if n == n.trunc() {
                    write!(f, "NUMBER {lexeme} {:.1}", n)
                } else {
                    write!(f, "NUMBER {lexeme} {n}")
                }
            }
            TokenType::Or => write!(f, "OR {lexeme} null"),
            TokenType::Plus => write!(f, "PLUS {lexeme} null"),
            TokenType::Print => write!(f, "PRINT {lexeme} null"),
            TokenType::RightParen => write!(f, "RIGHT_PAREN {lexeme} null"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE {lexeme} null"),
            TokenType::Return => write!(f, "RETURN {lexeme} null"),
            TokenType::Semicolon => write!(f, "SEMICOLON {lexeme} null"),
            TokenType::Star => write!(f, "STAR {lexeme} null"),
            TokenType::Slash => write!(f, "SLASH {lexeme} null"),
            TokenType::String => write!(f, "STRING {lexeme} {}", Token::unescape(lexeme)),
            TokenType::Super => write!(f, "SUPER {lexeme} null"),
            TokenType::This => write!(f, "THIS {lexeme} null"),
            TokenType::True => write!(f, "TRUE {lexeme} null"),
            TokenType::Var => write!(f, "VAR {lexeme} null"),
            TokenType::While => write!(f, "WHILE {lexeme} null"),
        }
    }
}

impl<'a> Token<'a> {
    pub fn new(lexeme: &'a str, offset: u64, token: TokenType) -> Self {
        return Token {
            lexeme,
            line: offset,
            token_type: token,
        };
    }

    // use 'o instead of 'a as the lifetimes are different this lifetime is not dependent on the
    // token lifetime
    pub fn unescape<'o>(s: &'o str) -> Cow<'o, str> {
        // Lox has no escaping
        // Since it has no escaping, strings can't contain ", so trim won't trim multiple
        Cow::Borrowed(s.trim_matches('"'))
    }
}

pub struct Lexer<'a> {
    // Original input str
    input: &'a str,
    // Remaining unlexed chars
    remaining: &'a str,
    // The position of the current char in the original input str
    byte_offset: u64,

    peeked: Option<Result<Token<'a>, miette::Error>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        return Lexer {
            input,
            remaining: input,
            byte_offset: 0,
            peeked: None,
        };
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'a>, miette::Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        } else {
            self.peeked = self.next();
            return self.peeked.as_ref();
        }
    }

    /*pub fn expect(
        &mut self,
        mut check: impl FnMut(&Token<'a>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'a>, miette::Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette!{
                labels = vec![LabeledSpan::at(token.line..token.line + token.lexeme.len(),"here"),],help=format!("Expected {token:?}"),"{unexpected}",
            }.with_source_code(self.input.to_string())),
            Some(Err(e)) => Err(e),
            None => Err("EOF"),
        }
    }*/
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, miette::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.peeked.take() {
            return Some(c);
        }

        loop {
            let mut remaining_chars = self.remaining.chars();

            // Attempt to get the first char, if the string is empty then the option returned is
            // None
            let current = remaining_chars.next()?;

            let length = current.len_utf8();

            let starting_position = self.byte_offset;

            let current_str = &self.remaining[..length];

            let remainder = self.remaining;

            // Set the remaining chars in the Lexer
            self.remaining = remaining_chars.as_str();

            // Increase the current position to after the current token
            self.byte_offset += length as u64;

            enum Group {
                Slash,
                String,
                Number,
                Identifier,
                IfEqualElse(TokenType, TokenType),
            }

            let group = match current {
                '(' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::LeftParen,
                    )))
                }
                ')' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::RightParen,
                    )))
                }
                '{' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::LeftBrace,
                    )))
                }
                '}' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::RightBrace,
                    )))
                }
                ',' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::Comma,
                    )))
                }
                '.' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::Dot,
                    )))
                }
                '-' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::Minus,
                    )))
                }
                '+' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::Plus,
                    )))
                }
                ';' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::Semicolon,
                    )))
                }
                '*' => {
                    return Some(Ok(Token::new(
                        current_str,
                        self.byte_offset,
                        TokenType::Star,
                    )))
                }
                '/' => Group::Slash,
                '<' => Group::IfEqualElse(TokenType::LessEqual, TokenType::Less),
                '>' => Group::IfEqualElse(TokenType::GreaterEqual, TokenType::Greater),
                '!' => Group::IfEqualElse(TokenType::BangEqual, TokenType::Bang),
                '=' => Group::IfEqualElse(TokenType::EqualEqual, TokenType::Equal),
                '"' => Group::String,
                '0'..='9' => Group::Number,
                'a'..='z' | 'A'..='Z' | '_' => Group::Identifier,
                c if c.is_whitespace() => continue,
                c => {
                    return Some(Err(SingleTokenError {
                        src: self.input.to_string(),
                        token: c,
                        err_span: SourceSpan::from(
                            self.byte_offset as usize - c.len_utf8()..self.byte_offset as usize,
                        ),
                    }
                    .into()));
                }
            };

            break match group {
                Group::String => {
                    if let Some(end) = self.remaining.find('"') {
                        let literal = &remainder[..end + 1 + 1];
                        self.byte_offset += end as u64 + 1;
                        self.remaining = &self.remaining[end + 1..];
                        Some(Ok(Token {
                            lexeme: literal,
                            line: starting_position,
                            token_type: TokenType::String,
                        }))
                    } else {
                        let err = StringTerminationError {
                            src: self.input.to_string(),
                            err_span: SourceSpan::from(
                                self.byte_offset as usize - current.len_utf8()..self.input.len(),
                            ),
                        };

                        // swallow the remainder of input as being a string
                        self.byte_offset += self.remaining.len() as u64;
                        self.remaining = &self.remaining[self.remaining.len()..];

                        return Some(Err(err.into()));
                    }
                }
                Group::Slash => {
                    if self.remaining.starts_with('/') {
                        // Comment
                        let line_end = self
                            .remaining
                            .find('\n')
                            .unwrap_or_else(|| self.remaining.len());
                        self.byte_offset += line_end as u64;
                        self.remaining = &self.remaining[line_end..];
                        continue;
                    } else {
                        Some(Ok(Token {
                            lexeme: current_str,
                            line: starting_position,
                            token_type: TokenType::Slash,
                        }))
                    }
                }
                Group::Identifier => {
                    let first_non_ident = remainder
                        .find(|c| !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_'))
                        .unwrap_or_else(|| remainder.len());

                    let literal = &remainder[..first_non_ident];
                    let extra_bytes = literal.len() - current.len_utf8();
                    self.byte_offset += extra_bytes as u64;
                    self.remaining = &self.remaining[extra_bytes..];

                    let token_type = match literal {
                        "and" => TokenType::And,
                        "class" => TokenType::Class,
                        "else" => TokenType::Else,
                        "false" => TokenType::False,
                        "for" => TokenType::For,
                        "fun" => TokenType::Fun,
                        "if" => TokenType::If,
                        "nil" => TokenType::Nil,
                        "or" => TokenType::Or,
                        "print" => TokenType::Print,
                        "return" => TokenType::Return,
                        "super" => TokenType::Super,
                        "this" => TokenType::This,
                        "true" => TokenType::True,
                        "var" => TokenType::Var,
                        "while" => TokenType::While,
                        _ => TokenType::Identifier,
                    };

                    return Some(Ok(Token {
                        lexeme: literal,
                        line: starting_position,
                        token_type,
                    }));
                }
                Group::Number => {
                    let first_non_digit = remainder
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or_else(|| remainder.len());

                    let mut literal = &remainder[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            literal = &literal[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(two), None) if two.is_empty() => {
                            literal = &literal[..one.len()];
                        }
                        _ => {
                            // leave literal as-is
                        }
                    }
                    let extra_bytes = literal.len() - current.len_utf8();
                    self.byte_offset += extra_bytes as u64;
                    self.remaining = &self.remaining[extra_bytes..];

                    let n = match literal.parse() {
                        Ok(n) => n,
                        Err(e) => {
                            return Some(Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(self.byte_offset as usize - literal.len()..self.byte_offset as usize, "this numeric literal"),
                                ],
                                "{e}",
                            }.with_source_code(self.input.to_string())));
                        }
                    };

                    return Some(Ok(Token {
                        lexeme: literal,
                        line: starting_position,
                        token_type: TokenType::Number(n),
                    }));
                }
                Group::IfEqualElse(yes, no) => {
                    self.remaining = self.remaining.trim_start();
                    let trimmed = remainder.len() - self.remaining.len() - 1;
                    self.byte_offset += trimmed as u64;
                    if self.remaining.starts_with('=') {
                        let span = &remainder[..current.len_utf8() + trimmed + 1];
                        self.remaining = &self.remaining[1..];
                        self.byte_offset += 1;
                        Some(Ok(Token {
                            lexeme: span,
                            line: starting_position,
                            token_type: yes,
                        }))
                    } else {
                        Some(Ok(Token {
                            lexeme: current_str,
                            line: starting_position,
                            token_type: no,
                        }))
                    }
                }
            };
        }
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected EOF")]
pub struct Eof;

#[derive(Diagnostic, Debug, Error)]
#[error("Unexpected token '{token}'")]
pub struct SingleTokenError {
    #[source_code]
    src: String,

    pub token: char,

    #[label = "this character"]
    err_span: SourceSpan,
}

impl SingleTokenError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct StringTerminationError {
    #[source_code]
    src: String,

    #[label = "this string literal"]
    err_span: SourceSpan,
}

impl StringTerminationError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}
