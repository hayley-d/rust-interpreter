use core::fmt;
use std::borrow::Cow;
use std::fmt::Display;

use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};

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
    Ident,
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
            TokenType::Ident => write!(f, "IDENTIFIER {lexeme} null"),
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

impl Token<'_> {
    pub fn unescape<'de>(s: &'de str) -> Cow<'de, str> {
        // Lox has no escaping
        // Since it has no escaping, strings can't contain ", so trim won't trim multiple
        Cow::Borrowed(s.trim_matches('"'))
    }
}

pub struct Lexer<'a> {
    input: &'a str,
    remaining: &'a str,
    current: u64,
    peeked: Option<Result<Token<'a>, miette::Error>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        return Lexer {
            input,
            remaining: input,
            current: 0,
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

    pub fn expect(
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
    }
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

            let starting_position = self.current;

            let current = &self.remaining[..length];

            let remainder = self.remaining;

            // Set the remaining chars in the Lexer
            self.remaining = remaining_chars.as_str();

            self.current += length as u64;

            todo!()
        }
    }
}
