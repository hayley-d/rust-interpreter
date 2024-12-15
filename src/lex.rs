use miette::{Diagnostic, Error, LabeledSpan, SourceSpan};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    Equal,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

pub struct Token<'a> {
    pub lexeme: &'a str,
    pub line: u64,
    pub token_type: TokenType,
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
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, miette::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
