use anyhow::{anyhow, Error, Result};
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

use crate::{Token, TokenType};

pub enum Expression<'a> {
    String(&'a str),
    Number(f64),
    Boolean(bool),
    Nil,
    EOF,
    Unary(&Token, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, &'a Token, Box<Expression<'a>>),
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
        /*for i in 0..self.tokens.len() {
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
            */
        todo!()
    }

    // returns a Literal Expression
    // The only evaluate function that is not recursive
    pub fn evaluate_literal(
        &'a self,
        parser: Rc<RefCell<Parser<'a>>>,
    ) -> Result<Expression<'a>, Error> {
        if parser.borrow().current as usize >= parser.borrow().tokens.len() {
            return Ok(Expression::EOF);
        } else {
            let current_token: &Token = &parser.borrow().tokens[parser.borrow().current as usize];
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
                    return Ok(Expression::String(
                        &self.tokens[self.current as usize].lexeme,
                    ));
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
    pub fn evaluate_unary(
        &'a self,
        parser: Rc<RefCell<Parser<'a>>>,
    ) -> Result<Expression<'a>, Error> {
        if parser.borrow().current as usize >= parser.borrow().tokens.len() - 1 {
            return Ok(Expression::EOF);
        } else {
            let current_token: &Token = &parser.borrow().tokens[parser.borrow().current as usize];
            let next_token: &Token = &parser.borrow().tokens[parser.borrow().current as usize + 1];

            match current_token.token_type {
                TokenType::Bang | TokenType::Minus | TokenType::Plus => match next_token.token_type
                {
                    TokenType::String
                    | TokenType::Number
                    | TokenType::Nil
                    | TokenType::False
                    | TokenType::True => {
                        parser.borrow_mut().current += 1;

                        let expression: Expression = match self.evaluate_literal(Rc::clone(&parser))
                        {
                            Ok(e) => e,
                            Err(e) => return Err(e),
                        };

                        return Ok(Expression::Unary(
                            &self.tokens[self.current as usize - 1],
                            Box::new(expression),
                        ));
                    }
                    TokenType::LeftParenthesis => {
                        parser.borrow_mut().current += 1;

                        let expression: Expression = match self.evaluate_group(Rc::clone(&parser)) {
                            Ok(e) => e,
                            Err(e) => return Err(e),
                        };
                        return Ok(Expression::Unary(
                            &self.tokens[self.current as usize - 1],
                            Box::new(expression),
                        ));
                    }
                    _ => {
                        return Err(anyhow!("Error: Unexpected token after unary operator"));
                    }
                },
                _ => {
                    return Err(anyhow!("Error: Not a literal"));
                }
            }
        }
    }

    fn evaluate_binary(&'a self, parser: Rc<RefCell<Parser<'a>>>) -> Result<Expression<'a>, Error> {
        todo!()
    }

    fn evaluate_group(&'a self, parser: Rc<RefCell<Parser<'a>>>) -> Result<Expression<'a>, Error> {
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
