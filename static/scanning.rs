pub mod scanning {

    use anyhow::{anyhow, Result};
    use std::fmt::Display;
    use std::fs;

    static KEYWORDS: [&'static str; 16] = [
        "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return",
        "super", "this", "true", "var", "while",
    ];

    pub struct Scanner {
        source: String,
        pub tokens: Vec<Token>,
    }

    impl Scanner {
        pub fn new(path: String) -> Self {
            return Scanner {
                source: path,
                tokens: Vec::new(),
            };
        }

        pub fn scan_tokens(&mut self) -> Result<u64> {
            let mut error_count = 0;
            let contents = match fs::read_to_string(&self.source) {
                Ok(c) => c,
                Err(_) => return Err(anyhow!("Unable to read file at {}", &self.source)),
            };

            let mut multiline_comment: bool = false;
            let mut comment_line: u64 = 0;
            let mut open_string: bool = false;
            let mut string_line: u64 = 0;

            let mut string_buffer: String = String::new();

            for (idx, mut line) in contents.lines().enumerate() {
                if line.is_empty() && !open_string || (multiline_comment && !line.contains("*/")) {
                    continue;
                } else if multiline_comment && line.contains("*/") {
                    // unwrap is safe as we know */ is in the line
                    let index = line.find("*/").unwrap() + 1;
                    line = &line[index + 1..];
                    multiline_comment = false;
                } else if line.is_empty() && open_string {
                    string_buffer.push_str("\n");
                    continue;
                } else if open_string {
                    if line.contains("\"") {
                        let index = line.find("\"").unwrap() - 1;
                        string_buffer.push_str(&line[..index]);
                        line = &line[index + 2..];
                        open_string = false;

                        self.tokens.push(Token::new(
                            TokenType::String,
                            Some(string_buffer.clone()),
                            format!("\"{}\"", string_buffer),
                            idx as u64,
                        ));

                        // reset
                        string_buffer = String::new();
                    } else {
                        string_buffer.push_str(line);
                        continue;
                    }
                }

                line = line.trim();

                let mut i = 0;

                while i < line.chars().count() {
                    // unwrap is safe as the index is in bounds
                    let c = line.chars().nth(i).unwrap();

                    match c {
                        '\t' | '\r' | ' ' => (),
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
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::PlusEqual,
                                    None,
                                    format!("+="),
                                    idx as u64,
                                ))
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
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::MinusEqual,
                                    None,
                                    format!("-="),
                                    idx as u64,
                                ))
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
                            /*let other_c: Option<char> = line.chars().nth(i + 1);
                            if other_c == Some('=') {
                                // Special case
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::StarEqual,
                                    None,
                                    format!("*="),
                                    idx as u64,
                                ))
                            } else {*/
                            self.tokens.push(Token::new(
                                TokenType::Star,
                                None,
                                format!("*"),
                                idx as u64,
                            ))
                            //}
                        }
                        '/' => {
                            let other_c: Option<char> = line.chars().nth(i + 1);
                            if other_c == Some('/') {
                                // Comment line //
                                break;
                            }
                            if other_c == Some('*') {
                                // multiline comment /*
                                multiline_comment = true;
                                comment_line = idx as u64;
                                if line.contains("*/") {
                                    i = line.find("*/").unwrap() + 2;
                                    multiline_comment = false;
                                    continue;
                                } else {
                                    break;
                                }
                            }
                            if other_c == Some('=') {
                                // divide equals /=
                                i += 1;
                                self.tokens.push(Token::new(
                                    TokenType::SlashEqual,
                                    None,
                                    format!("/="),
                                    idx as u64,
                                ))
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
                        '"' => {
                            if !open_string {
                                // start of new string
                                if line[i + 1..].contains('"') {
                                    // string ends on same line
                                    let mut string_literal: String = String::new();
                                    i += 1;
                                    while i < line.len() {
                                        let n = line.chars().nth(i).unwrap();
                                        if n == '"' {
                                            break;
                                        } else {
                                            string_literal.push(n);
                                        }
                                        i += 1;
                                    }

                                    self.tokens.push(Token::new(
                                        TokenType::String,
                                        Some(string_literal.clone()),
                                        format!("\"{}\"", string_literal),
                                        idx as u64,
                                    ));
                                } else {
                                    // multi-line string
                                    string_line = idx as u64;
                                    open_string = true;
                                    string_buffer.push_str(&line[i + 1..]);
                                    break;
                                }
                            }
                        }
                        '0'..='9' => {
                            let next: Option<char> = line.chars().nth(i + 1);
                            match next {
                                Some(n) => {
                                    if n.is_digit(10) || n == '.' {
                                        let mut num: String = String::from(c);
                                        num.push(n);
                                        i += 2;

                                        while i < line.len() {
                                            let n = line.chars().nth(i).unwrap();
                                            if n.is_digit(10) {
                                                num.push(n);
                                            } else if n == '.' {
                                                if num.contains(".") {
                                                    return Err(anyhow!("Invalid number format!"));
                                                } else {
                                                    num.push(n);
                                                }
                                            } else {
                                                break;
                                            }
                                            i += 1;
                                        }

                                        let num_form = num.parse::<f64>().unwrap();

                                        self.tokens.push(Token::new(
                                            TokenType::Number,
                                            Some(num_form.to_string()),
                                            format!("{}", num),
                                            idx as u64,
                                        ));

                                        continue;
                                    } else {
                                        self.tokens.push(Token::new(
                                            TokenType::Number,
                                            Some(c.to_string()),
                                            format!("{}", c),
                                            idx as u64,
                                        ))
                                    }
                                }
                                None => self.tokens.push(Token::new(
                                    TokenType::Number,
                                    Some(c.to_string()),
                                    format!("{}", c),
                                    idx as u64,
                                )),
                            }
                        }
                        'A'..='Z' | 'a'..='z' | '_' => {
                            let next: Option<char> = line.chars().nth(i + 1);

                            match next {
                                Some(n) => {
                                    if n.is_ascii_alphabetic() || n == '_' || n.is_digit(10) {
                                        let mut identifier: String = String::from(c);
                                        identifier.push(n);
                                        i += 2;

                                        while i < line.len() {
                                            let n = line.chars().nth(i).unwrap();
                                            if n.is_ascii_alphabetic() || n == '_' || n.is_digit(10)
                                            {
                                                identifier.push(n);
                                            } else if n == ' ' {
                                                break;
                                            } else {
                                                break;
                                            }
                                            i += 1;
                                        }

                                        if KEYWORDS.iter().any(|&k| k == identifier) {
                                            let token_type = TokenType::get_keyword(&identifier);
                                            let token_str = token_type.print();
                                            self.tokens.push(Token::new(
                                                token_type, None, token_str, idx as u64,
                                            ));
                                            continue;
                                        } else {
                                            self.tokens.push(Token::new(
                                                TokenType::Identifier,
                                                None,
                                                format!("{}", identifier),
                                                idx as u64,
                                            ));
                                        }

                                        continue;
                                    } else {
                                        self.tokens.push(Token::new(
                                            TokenType::Identifier,
                                            None,
                                            format!("{}", c),
                                            idx as u64,
                                        ))
                                    }
                                }
                                None => self.tokens.push(Token::new(
                                    TokenType::Number,
                                    Some(c.to_string()),
                                    format!("{}", c),
                                    idx as u64,
                                )),
                            }
                        }
                        _ => {
                            let error =
                                anyhow!("[line {}] Error: Unexpected character: {}", idx + 1, c);
                            eprintln!("{error}");
                            error_count += 1;
                        }
                    }
                    i += 1;
                }
            }

            if multiline_comment {
                eprintln!(
                    "{}",
                    anyhow!(
                        "[line {}] Error: Unterminated multi-line comment.",
                        comment_line + 1
                    )
                );
                error_count += 1;
            }

            if open_string {
                eprintln!(
                    "{}",
                    anyhow!("[line {}] Error: Unterminated string.", string_line + 1)
                );
                error_count += 1;
            }

            self.tokens.push(Token::new(
                TokenType::Eof,
                None,
                String::new(),
                contents.lines().count() as u64,
            ));

            return Ok(error_count);
        }
    }

    #[allow(dead_code)]
    #[derive(Debug)]
    pub struct Token {
        pub token_type: TokenType,
        pub lexeme: String,
        pub literal: Option<String>,
        pub line: u64,
    }

    pub fn read_file(path: &str) -> Result<String> {
        match fs::read_to_string(path) {
            Ok(c) => Ok(c),
            Err(_) => Err(anyhow!("Unable to read file at {}", path)),
        }
    }

    #[derive(PartialEq, Eq, Debug)]
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
        MinusEqual,
        Nil,
        Number,
        Or,
        Plus,
        PlusEqual,
        Print,
        Return,
        RightBrace,
        RightParenthesis,
        Semicolon,
        Slash,
        SlashEqual,
        Star,
        StarEqual,
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
            if self.token_type == TokenType::String {
                write!(
                    f,
                    "{} {} {}",
                    self.token_type,
                    self.lexeme,
                    match &self.literal {
                        Some(l) => l,
                        None => "",
                    }
                )
            } else if self.token_type == TokenType::Number {
                let num: String = self.literal.clone().unwrap();

                if num.contains(".") {
                    write!(f, "{} {} {}", self.token_type, self.lexeme, num)
                } else {
                    write!(
                        f,
                        "{} {} {:.1}",
                        self.token_type,
                        self.lexeme,
                        &num.clone().parse::<f64>().unwrap(),
                    )
                }
            } else {
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
    }

    impl TokenType {
        pub fn get_keyword(keyword: &str) -> TokenType {
            match keyword {
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
                "true" => TokenType::True,
                "super" => TokenType::Super,
                "this" => TokenType::This,
                "var" => TokenType::Var,
                "while" => TokenType::While,
                _ => TokenType::Nil,
            }
        }

        pub fn print(&self) -> String {
            match self {
                TokenType::And => String::from("and"),
                TokenType::Class => String::from("class"),
                TokenType::Else => String::from("else"),
                TokenType::False => String::from("false"),
                TokenType::For => String::from("for"),
                TokenType::Fun => String::from("fun"),
                TokenType::If => String::from("if"),
                TokenType::Nil => String::from("nil"),
                TokenType::Or => String::from("or"),
                TokenType::Print => String::from("print"),
                TokenType::Return => String::from("return"),
                TokenType::Super => String::from("super"),
                TokenType::This => String::from("this"),
                TokenType::True => String::from("true"),
                TokenType::Var => String::from("var"),
                TokenType::While => String::from("while"),
                _ => String::new(),
            }
        }
    }

    impl Display for TokenType {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                TokenType::And => write!(f, "AND"),
                TokenType::Bang => write!(f, "BANG"),
                TokenType::BangEqual => write!(f, "BANG_EQUAL"),
                TokenType::Class => write!(f, "CLASS"),
                TokenType::Comma => write!(f, "COMMA"),
                TokenType::Comment => write!(f, "COMMENT"),
                TokenType::Dot => write!(f, "DOT"),
                TokenType::Else => write!(f, "ELSE"),
                TokenType::Eof => write!(f, "EOF"),
                TokenType::Equal => write!(f, "EQUAL"),
                TokenType::EqualEqual => write!(f, "EQUAL_EQUAL"),
                TokenType::False => write!(f, "FALSE"),
                TokenType::For => write!(f, "FOR"),
                TokenType::Fun => write!(f, "FUN"),
                TokenType::Greater => write!(f, "GREATER"),
                TokenType::GreaterEqual => write!(f, "GREATER_EQUAL"),
                TokenType::Identifier => write!(f, "IDENTIFIER"),
                TokenType::If => write!(f, "IF"),
                TokenType::LeftBrace => write!(f, "LEFT_BRACE"),
                TokenType::LeftParenthesis => write!(f, "LEFT_PAREN"),
                TokenType::Less => write!(f, "LESS"),
                TokenType::LessEqual => write!(f, "LESS_EQUAL"),
                TokenType::Minus => write!(f, "MINUS"),
                TokenType::MinusEqual => write!(f, "MINUS_EQUAL"),
                TokenType::Nil => write!(f, "NIL"),
                TokenType::Number => write!(f, "NUMBER"),
                TokenType::Or => write!(f, "OR"),
                TokenType::Plus => write!(f, "PLUS"),
                TokenType::PlusEqual => write!(f, "PLUS_EQUAL"),
                TokenType::Print => write!(f, "PRINT"),
                TokenType::Return => write!(f, "RETURN"),
                TokenType::RightBrace => write!(f, "RIGHT_BRACE"),
                TokenType::RightParenthesis => write!(f, "RIGHT_PAREN"),
                TokenType::Semicolon => write!(f, "SEMICOLON"),
                TokenType::Slash => write!(f, "SLASH"),
                TokenType::SlashEqual => write!(f, "SLASH_EQUAL"),
                TokenType::Star => write!(f, "STAR"),
                TokenType::StarEqual => write!(f, "STAR_EQUAl"),
                TokenType::String => write!(f, "STRING"),
                TokenType::Super => write!(f, "SUPER"),
                TokenType::This => write!(f, "THIS"),
                TokenType::True => write!(f, "TRUE"),
                TokenType::Var => write!(f, "VAR"),
                TokenType::While => write!(f, "WHILE"),
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        fn token_to_string(token: &Token) -> String {
            format!(
                "{{type: {}, lexeme: '{}', literal: {:?}, line: {}}}",
                token.token_type, token.lexeme, token.literal, token.line
            )
        }

        #[test]
        fn test_scanner_empty_file() {
            let mut scanner = Scanner::new("static/test/empty_file.txt".to_string());
            let errors = scanner.scan_tokens().unwrap();
            assert_eq!(errors, 0);
            assert_eq!(scanner.tokens.len(), 1);
            assert_eq!(scanner.tokens[0].token_type, TokenType::Eof);
        }

        #[test]
        fn test_scanner_single_line_tokens() {
            let mut scanner = Scanner::new("static/test/single_line.txt".to_string());
            let _ = scanner.scan_tokens().unwrap();

            let expected = vec![
                TokenType::LeftParenthesis,
                TokenType::RightParenthesis,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Comma,
                TokenType::Dot,
                TokenType::Semicolon,
                TokenType::Eof,
            ];

            assert_eq!(scanner.tokens.len(), expected.len());
            for (i, token) in scanner.tokens.iter().enumerate() {
                assert_eq!(token.token_type, expected[i]);
            }
        }

        #[test]
        fn test_scanner_multiline_comment() {
            let mut scanner = Scanner::new("static/test/multiline_comment.txt".to_string());
            let _ = scanner.scan_tokens().unwrap();

            assert!(scanner
                .tokens
                .iter()
                .all(|t| t.token_type != TokenType::Comment));
            assert_eq!(scanner.tokens.last().unwrap().token_type, TokenType::Eof);
        }

        #[test]
        fn test_scanner_operators() {
            let mut scanner = Scanner::new("static/test/operators.txt".to_string());
            let _ = scanner.scan_tokens().unwrap();

            let expected = vec![
                TokenType::Bang,
                TokenType::BangEqual,
                TokenType::Equal,
                TokenType::EqualEqual,
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
                TokenType::Plus,
                TokenType::PlusEqual,
                TokenType::Minus,
                TokenType::MinusEqual,
                TokenType::Star,
                TokenType::StarEqual,
                TokenType::Slash,
                TokenType::SlashEqual,
                TokenType::Eof,
            ];

            assert_eq!(scanner.tokens.len(), expected.len());
            for (i, token) in scanner.tokens.iter().enumerate() {
                assert_eq!(
                    token.token_type,
                    expected[i],
                    "Unexpected token at index {}: {}",
                    i,
                    token_to_string(token)
                );
            }
        }

        #[test]
        fn test_scanner_invalid_character() {
            let mut scanner = Scanner::new("static/test/invalid_character.txt".to_string());
            let errors = scanner.scan_tokens().unwrap();
            assert_ne!(errors, 0);
        }

        /*#[test]
        fn test_scanner_realistic_code() {
            let mut scanner = Scanner::new("static/test/realistic_code.txt".to_string());
            let tokens = scanner.scan_tokens().unwrap();

            assert!(tokens.len() > 1);
            assert_eq!(tokens.last().unwrap().token_type, TokenType::Eof);
        }*/
    }
}
