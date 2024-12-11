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

        pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
            let contents = match fs::read_to_string(&self.source) {
                Ok(c) => c,
                Err(_) => return Err(anyhow!("Unable to read file at {}", &self.source)),
            };
            for (idx, mut line) in contents.lines().enumerate() {
                if line.is_empty() {
                    continue;
                }

                line = line.trim();

                let mut start = 0;

                let mut i = 0;

                while i < line.chars().count() {
                    // unwrap is safe as the index is in bounds
                    let c = line.chars().nth(i).unwrap();

                    match c {
                        '(' => self.tokens.push(Token::new(
                            TokenType::LeftParenthesis,
                            None,
                            format!("("),
                            idx as u64,
                        )),
                        ')' => self.tokens.push(Token::new(
                            TokenType::RightParenthesis,
                            None,
                            format!(")"),
                            idx as u64,
                        )),
                        '{' => self.tokens.push(Token::new(
                            TokenType::LeftBrace,
                            None,
                            format!("{{"),
                            idx as u64,
                        )),
                        '}' => self.tokens.push(Token::new(
                            TokenType::RightBrace,
                            None,
                            format!("}}"),
                            idx as u64,
                        )),

                        ',' => self.tokens.push(Token::new(
                            TokenType::Comma,
                            None,
                            format!(","),
                            idx as u64,
                        )),
                        '.' => self.tokens.push(Token::new(
                            TokenType::Dot,
                            None,
                            format!("."),
                            idx as u64,
                        )),
                        ';' => self.tokens.push(Token::new(
                            TokenType::Semicolon,
                            None,
                            format!(";"),
                            idx as u64,
                        )),
                        '!' => {
                            let other_c: Option<char> = line.chars().nth(i + 1);
                            if other_c == Some('=') {
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::BangEqual,
                                    None,
                                    format!("!="),
                                    idx as u64,
                                ))
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Bang,
                                    None,
                                    format!("!"),
                                    idx as u64,
                                ))
                            }
                        }
                        '+' => {
                            if line.chars().nth(i + 1) == Some('=') {
                                // Special case
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Plus,
                                    None,
                                    format!("+"),
                                    idx as u64,
                                ))
                            }
                        }
                        '-' => {
                            if line.chars().nth(i + 1) == Some('=') {
                                // Special case
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Minus,
                                    None,
                                    format!("-"),
                                    idx as u64,
                                ))
                            }
                        }
                        '*' => {
                            if line.chars().nth(i + 1) == Some('=') {
                                // Special case
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Star,
                                    None,
                                    format!("*"),
                                    idx as u64,
                                ))
                            }
                        }
                        '/' => {
                            let other_c: Option<char> = line.chars().nth(i + 1);
                            if other_c == Some('/') {
                                // Comment line //
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::Comment,
                                    Some(
                                        line.chars().collect::<Vec<char>>()[i + 1..]
                                            .iter()
                                            .collect::<String>(),
                                    ),
                                    format!("//"),
                                    idx as u64,
                                ))
                            }
                            if other_c == Some('*') {
                                // multiline comment /*
                            }
                            if other_c == Some('=') {
                                // divide equals /=
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Slash,
                                    None,
                                    format!("/"),
                                    idx as u64,
                                ))
                            }
                        }
                        '>' => {
                            if line.chars().nth(i + 1) == Some('=') {
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::GreaterEqual,
                                    None,
                                    format!(">="),
                                    idx as u64,
                                ))
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Greater,
                                    None,
                                    format!(">"),
                                    idx as u64,
                                ))
                            }
                        }
                        '<' => {
                            if line.chars().nth(i + 1) == Some('=') {
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::LessEqual,
                                    None,
                                    format!("<="),
                                    idx as u64,
                                ))
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Less,
                                    None,
                                    format!("<"),
                                    idx as u64,
                                ))
                            }
                        }
                        '=' => {
                            if line.chars().nth(i + 1) == Some('=') {
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::EqualEqual,
                                    None,
                                    format!("=="),
                                    idx as u64,
                                ))
                            } else {
                                self.tokens.push(Token::new(
                                    TokenType::Equal,
                                    None,
                                    format!("="),
                                    idx as u64,
                                ))
                            }
                        }
                        _ => return Err(anyhow!("Unsupported character")),
                    }
                    i += 1;
                }
            }

            self.tokens.push(Token::new(
                TokenType::Eof,
                None,
                String::new(),
                contents.lines().count() as u64,
            ));

            return Ok(Vec::new());
        }
    }

    pub struct Token {
        token_type: TokenType,
        lexeme: String,
        literal: Option<String>,
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
        Comment,
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

    impl Token {
        pub fn new(
            token_type: TokenType,
            literal: Option<String>,
            lexeme: String,
            line: u64,
        ) -> Self {
            return Token {
                token_type,
                lexeme,
                literal,
                line,
            };
        }
    }
    impl Display for Token {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let nil: String = String::from("null");
            write!(
                f,
                "{} {} {}",
                self.token_type,
                self.lexeme,
                match &self.literal {
                    Some(l) => l,
                    None => &nil,
                }
            )
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
                TokenType::Comment => write!(f, "Comment"),
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
