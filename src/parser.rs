use anyhow::{anyhow, Error, Result};
use std::fmt::Display;

use crate::{Token, TokenType};

pub enum Expression<'a> {
    String(&'a str),
    Number(f64),
    Boolean(bool),
    Nil,
    EOF,
    Unary(&Token, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, &Token, Box<Expression<'a>>),
    Grouping(Box<Expression<'a>>),
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: u64,
    parse_tree: Vec<Expression<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>) -> Self {
        return Parser {
            tokens,
            current: 0,
            parse_tree: Vec::with_capacity(1),
        };
    }

    // This is the entry point all call recursive functions
    pub fn parse(&self) -> Result<Expression, Error> {
        for i in 0..self.tokens.len() {
            match &self.tokens[i].token_type {
                TokenType::And => {
                    return self.evaluate_binary(i);
                }
                TokenType::Bang => {
                    return self.evaluate_unary(i);
                }
                TokenType::BangEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::Class => {
                    todo!()
                }
                TokenType::Comma => {}
                TokenType::Comment => {
                    //nothing
                }
                TokenType::Dot => {
                    todo!()
                }
                TokenType::Else => {
                    todo!()
                }
                TokenType::Eof => {
                    break;
                }
                TokenType::Equal => {
                    todo!()
                }
                TokenType::EqualEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::False => {
                    return Ok(Expression::Boolean(false));
                }
                TokenType::For => {
                    todo!()
                }
                TokenType::Fun => {
                    todo!()
                }
                TokenType::Greater => {
                    return self.evaluate_binary(i);
                }
                TokenType::GreaterEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::Identifier => {
                    todo!()
                }
                TokenType::If => {
                    todo!()
                }
                TokenType::LeftBrace => {}
                TokenType::LeftParenthesis => {}
                TokenType::Less => {
                    return self.evaluate_binary(i);
                }
                TokenType::LessEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::Minus => {
                    if i > 0 {
                        match self.tokens[i - 1].token_type {
                            TokenType::Number | TokenType::String => {
                                return self.evaluate_binary(i);
                            }
                            TokenType::Semicolon
                            | TokenType::Equal
                            | TokenType::LeftParenthesis => return self.evaluate_unary(i),
                            _ => {
                                return Err(anyhow!(
                                    "[line {}] Error at '-': Expect expression",
                                    self.tokens[i].line
                                ))
                            }
                        }
                    }
                    return self.evaluate_binary(i);
                }
                TokenType::MinusEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::Nil => {
                    return Ok(Expression::Nil);
                }
                TokenType::Number => {
                    return Ok(Expression::Number(
                        self.tokens[i].lexeme.parse::<f64>().unwrap(),
                    ));
                }
                TokenType::Or => {
                    return self.evaluate_binary(i);
                }
                TokenType::Plus => {
                    return self.evaluate_binary(i);
                }
                TokenType::PlusEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::Print => {
                    todo!()
                }
                TokenType::Return => {
                    todo!()
                }
                TokenType::RightBrace => {
                    return Err(anyhow!(
                        "[line {}] Error at '}}': Expect opening brace.",
                        self.tokens[i].line
                    ));
                }
                TokenType::RightParenthesis => {
                    return Err(anyhow!(
                        "[line {}] Error at ')': Expect opening parenthesis.",
                        self.tokens[i].line
                    ));
                }
                TokenType::Semicolon => {
                    todo!()
                }
                TokenType::Slash => {
                    return self.evaluate_binary(i);
                }
                TokenType::SlashEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::Star => {
                    return self.evaluate_binary(i);
                }
                TokenType::StarEqual => {
                    return self.evaluate_binary(i);
                }
                TokenType::String => {
                    return Ok(Expression::String(&self.tokens[i].lexeme));
                }
                TokenType::Super => {
                    todo!()
                }
                TokenType::This => {
                    todo!()
                }
                TokenType::True => {
                    return Ok(Expression::Boolean(true));
                }
                TokenType::Var => {
                    todo!()
                }
                TokenType::While => {
                    todo!()
                }
            } // end match
        } // end for
        todo!()
    }

    // returns a Literal Expression
    // The only evaluate function that is not recursive
    fn evaluate_literal(&'a self) -> Result<Expression<'a>, Error> {
        if self.current as usize >= self.tokens.len() {
            return Ok(Expression::EOF);
        } else {
            let current_token: &Token = &self.tokens[self.current as usize];
            match current_token.token_type {
                TokenType::Eof => {
                    return Ok(Expression::EOF);
                }
                TokenType::False => {
                    return Ok(Expression::Boolean(false));
                }
                TokenType::True => {
                    return Ok(Expression::Boolean(true));
                }
                TokenType::Number => {
                    return Ok(Expression::Number(
                        current_token.lexeme.parse::<f64>().unwrap(),
                    ));
                }
                TokenType::String => {
                    return Ok(Expression::String(&current_token.lexeme));
                }
                TokenType::Nil => {
                    return Ok(Expression::Nil);
                }
                _ => {
                    return Err(anyhow!("Error: Not a literal"));
                }
            }
        }
    }

    // this expects the current token to be a ! or - or +
    fn evaluate_unary(&'a mut self) -> Result<Expression<'a>, Error> {
        if self.current as usize >= self.tokens.len() - 1 {
            return Ok(Expression::EOF);
        } else {
            let current_token: &Token = &self.tokens[self.current as usize];
            let next_token: &Token = &self.tokens[self.current as usize + 1];

            match current_token.token_type {
                TokenType::Bang => match next_token.token_type {
                    TokenType::String
                    | TokenType::Number
                    | TokenType::Nil
                    | TokenType::False
                    | TokenType::True => {
                        self.current += 1;

                        let expression: Expression = match self.evaluate_literal() {
                            Ok(e) => e,
                            Err(e) => return Err(e),
                        };

                        return Ok(Expression::Unary(current_token, Box::new(expression)));
                    }
                    TokenType::LeftParenthesis => {
                        self.current += 1;

                        let expression: Expression = match self.evaluate_group() {
                            Ok(e) => e,
                            Err(e) => return Err(e),
                        };
                        return Ok(Expression::Unary(current_token, Box::new(expression)));
                    }
                    _ => {
                        return Err(anyhow!("Error: Unexpected token after unary operator"));
                    }
                },
                TokenType::Minus => {}
                TokenType::Plus => {}
                _ => {
                    return Err(anyhow!("Error: Not a literal"));
                }
            }
        }
        todo!()
    }

    fn evaluate_binary(&'a mut self, start: usize) -> Result<Expression<'a>, Error> {
        if start >= self.tokens.len() || start == 0 {
            return Err(anyhow!("Error: Unexpected error when parsing tokens"));
        }

        todo!()
    }

    fn evaluate_group(&'a mut self) -> Result<Expression<'a>, Error> {
        todo!()
    }
}

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::String(value) => {
                write!(f, "{value}")
            }
            Expression::Number(value) => {
                write!(f, "{:.1}", value)
            }
            Expression::Boolean(value) => {
                write!(f, "{value}")
            }
            Expression::Nil => {
                write!(f, "nil")
            }
            Expression::Unary(token, expression) => {
                write!(f, "{} {expression}", token.lexeme)
            }
            Expression::Binary(expression1, token, expression2) => {
                write!(f, "{expression1} {} {expression2}", token.lexeme)
            }
            Expression::Grouping(expression) => {
                write!(f, "({expression})")
            }
            _ => write!(f, ""),
        }
    }
}
