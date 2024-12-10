pub mod scanning {
    use anyhow::{anyhow, Result};
    use std::fmt::Display;
    use std::fs;

    pub struct Scanner {
        source: String,
        tokens: Vec<Token>,
    }

    impl Scanner {
        pub fn new(path: String) -> Self {
            return Scanner {
                source: path,
                tokens: Vec::new(),
            };
        }

        pub fn scan_tokens() -> Vec<Token> {
            return Vec::new();
        }
    }

    pub struct Token {
        token_type: TokenType,
        literal: String,
        lexeme: String,
        line: u64,
    }

    pub fn read_file(path: &str) -> Result<String> {
        match fs::read_to_string(path) {
            Ok(c) => Ok(c),
            Err(_) => Err(anyhow!("Unable to read file at {}", path)),
        }
    }

    pub enum TokenType {
        And,
        Bang,
        BangEqual,
        Class,
        Comma,
        Dot,
        Else,
        Eof,
        Equal,
        EqualEqual,
        False,
        For,
        Fun,
        Greater,
        GreaterEqual,
        Identifier,
        If,
        LeftBrace,
        LeftParenthesis,
        Less,
        LessEqual,
        Minus,
        Nil,
        Number,
        Or,
        Plus,
        Print,
        Return,
        RightBrace,
        RightParenthesis,
        Semicolon,
        Slash,
        Star,
        String,
        Super,
        This,
        True,
        Var,
        While,
    }

    impl Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {} {}", self.token_type, self.lexeme, self.literal)
        }
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                TokenType::And => write!(f, "AND"),
                TokenType::Bang => write!(f, "!"),
                TokenType::BangEqual => write!(f, "!="),
                TokenType::Class => write!(f, "class"),
                TokenType::Comma => write!(f, ","),
                TokenType::Dot => write!(f, "."),
                TokenType::Else => write!(f, "else"),
                TokenType::Eof => write!(f, "EOF"),
                TokenType::Equal => write!(f, "="),
                TokenType::EqualEqual => write!(f, "=="),
                TokenType::False => write!(f, "false"),
                TokenType::For => write!(f, "for"),
                TokenType::Fun => write!(f, "fun"),
                TokenType::Greater => write!(f, ">"),
                TokenType::GreaterEqual => write!(f, ">="),
                TokenType::Identifier => write!(f, "identifier"),
                TokenType::If => write!(f, "if"),
                TokenType::LeftBrace => write!(f, "{{"),
                TokenType::LeftParenthesis => write!(f, "("),
                TokenType::Less => write!(f, "<"),
                TokenType::LessEqual => write!(f, "<="),
                TokenType::Minus => write!(f, "-"),
                TokenType::Nil => write!(f, "Nil"),
                TokenType::Number => write!(f, "Number"),
                TokenType::Or => write!(f, "OR"),
                TokenType::Plus => write!(f, "+"),
                TokenType::Print => write!(f, "print"),
                TokenType::Return => write!(f, "return"),
                TokenType::RightBrace => write!(f, "}}"),
                TokenType::RightParenthesis => write!(f, ")"),
                TokenType::Semicolon => write!(f, ";"),
                TokenType::Slash => write!(f, "/"),
                TokenType::Star => write!(f, "*"),
                TokenType::String => write!(f, "string"),
                TokenType::Super => write!(f, "super"),
                TokenType::This => write!(f, "this"),
                TokenType::True => write!(f, "true"),
                TokenType::Var => write!(f, "var"),
                TokenType::While => write!(f, "while"),
            }
        }
    }
}
