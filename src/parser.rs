use crate::{Lexer, Token, TokenType};
use miette::{Error, LabeledSpan, WrapErr};
use std::{borrow::Cow, fmt};

pub struct Parser<'a> {
    input: &'a str,
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        return Parser {
            input,
            lexer: Lexer::new(input),
        };
    }

    pub fn parser(self) -> Result<ParseTree<'a>, miette::Error> {
        todo!()
    }

    pub fn parse_expression(mut self) -> Result<ParseTree<'a>, miette::Error> {
        return self.parse_expression_within(0);
    }

    // Parses a Code block {...}
    pub fn parse_block(&mut self) -> Result<ParseTree<'a>, Error> {
        self.lexer.expect(TokenType::LeftBrace, "missing {")?;
        let block: ParseTree = self.parse_statement_within(0)?;
        self.lexer.expect(TokenType::RightBrace, "missing }")?;
        return Ok(block);
    }

    pub fn parse_function_calls(&mut self) -> Result<Vec<ParseTree<'a>>, Error> {
        let mut arguments: Vec<ParseTree> = Vec::new();

        if matches!(
            self.lexer.peek(),
            Some(Ok(Token {
                token_type: TokenType::RightParen,
                ..
            }))
        ) {
            // No arguments for function call
        } else {
            loop {
                let arg: ParseTree = self.parse_expression_within(0).wrap_err_with(|| {
                    format!("in argument #{} of function call", arguments.len() + 1)
                })?;

                arguments.push(arg);

                let token: Token = self
                    .lexer
                    .expect_where(
                        |token| {
                            matches!(token.token_type, TokenType::RightParen | TokenType::Comma)
                        },
                        "continuing argument list",
                    )
                    .wrap_err("in argument list of function call")?;

                if token.token_type == TokenType::RightParen {
                    break;
                }
            }
        }

        return Ok(arguments);
    }

    // The parser parses expression with a binding power higher than min_bp and will stop as soon
    // as it comes accross a weaker binding power
    pub fn parse_statement_within(&mut self, min_bp: u8) -> Result<ParseTree<'a>, Error> {
        let lhs: Token = match self.lexer.next() {
            Some(Ok(token)) => token,
            Some(Err(e)) => return Err(e).wrap_err("on the left hand side"),
            None => return Ok(ParseTree::Literal(Literal::Nil)),
        };

        let mut lhs: ParseTree = match lhs {
            Token {
                token_type: TokenType::Identifier,
                lexeme,
                ..
            } => ParseTree::Literal(Literal::Identifier(lexeme)),
            Token {
                token_type: TokenType::Super,
                ..
            } => ParseTree::Literal(Literal::Super),
            Token {
                token_type: TokenType::This,
                ..
            } => ParseTree::Literal(Literal::This),
            Token {
                token_type: TokenType::LeftParen,
                ..
            } => {
                let lhs: ParseTree = self
                    .parse_expression_within(0)
                    .wrap_err("in expression group")?;
                self.lexer
                    .expect(TokenType::RightParen, "Unexpected end to expression group")
                    .wrap_err("after expression group")?;
                ParseTree::Cons(Operator::Group, vec![lhs])
            }
            Token {
                token_type: TokenType::Print | TokenType::Return,
                ..
            } => {
                // If the operator is a print/return  get the binding power of the right hand side
                let operator: Operator = match lhs.token_type {
                    TokenType::Print => Operator::Print,
                    TokenType::Return => Operator::Return,
                    _ => unreachable!("by the outer arm pattern"),
                };

                let ((), right_bp): ((), u8) = prefix_binding_power(operator);

                let rhs: ParseTree = self
                    .parse_expression_within(right_bp)
                    .wrap_err_with(|| format!("on the right-hand side of {operator:?}"))?;

                return Ok(ParseTree::Cons(operator, vec![rhs]));
            }
            Token {
                token_type: TokenType::This,
                ..
            } => ParseTree::Literal(Literal::This),
        };

        todo!()
    }

    pub fn parse_expression_within(&mut self, min_bp: u8) -> Result<ParseTree<'a>, Error> {
        todo!()
    }
}

// Enum that represents different possibilities for literal types.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Identifier(&'de str),
    Super,
    This,
}

impl fmt::Display for Literal<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(s) => write!(f, "\"{s}\""),
            Literal::Number(n) => {
                if *n == n.trunc() {
                    write!(f, "{:.1}", n)
                } else {
                    write!(f, "{n}")
                }
            }
            Literal::Nil => write!(f, "nil"),
            Literal::Bool(b) => write!(f, "{b:?}"),
            Literal::Identifier(i) => write!(f, "{i}"),
            Literal::Super => write!(f, "super"),
            Literal::This => write!(f, "this"),
        }
    }
}

// Enum represents different types of operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Minus,
    Plus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    And,
    Or,
    Call,
    For,
    Class,
    Print,
    Return,
    Field,
    Var,
    While,
    Group,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Minus => "-",
                Operator::Plus => "+",
                Operator::Star => "*",
                Operator::BangEqual => "!=",
                Operator::EqualEqual => "==",
                Operator::LessEqual => "<=",
                Operator::GreaterEqual => ">=",
                Operator::Less => "<",
                Operator::Greater => ">",
                Operator::Slash => "/",
                Operator::Bang => "!",
                Operator::And => "and",
                Operator::Or => "or",
                Operator::For => "for",
                Operator::Class => "class",
                Operator::Print => "print",
                Operator::Return => "return",
                Operator::Field => ".",
                Operator::Var => "var",
                Operator::While => "while",
                Operator::Call => "call",
                Operator::Group => "group",
            }
        )
    }
}

pub struct Ast {}

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

impl fmt::Display for ParseTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseTree::Literal(i) => write!(f, "{}", i),
            ParseTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {s}")?
                }
                write!(f, ")")
            }
            ParseTree::Fun {
                name,
                parameters,
                body,
            } => {
                write!(f, "(def {name}")?;
                for p in parameters {
                    write!(f, " {p}")?
                }
                write!(f, " {body})")
            }
            ParseTree::Call { callee, arguments } => {
                write!(f, "({callee}")?;
                for a in arguments {
                    write!(f, " {a}")?
                }
                write!(f, ")")
            }
            ParseTree::If { condition, yes, no } => {
                write!(f, "(if {condition} {yes}")?;
                if let Some(no) = no {
                    write!(f, " {no}")?
                }
                write!(f, ")")
            }
        }
    }
}

fn prefix_binding_power(op: Operator) -> ((), u8) {
    match op {
        Operator::Print | Operator::Return => ((), 1),
        Operator::Bang | Operator::Minus => ((), 11),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: Operator) -> Option<(u8, ())> {
    let res = match op {
        Operator::Call => (13, ()),
        _ => return None,
    };
    Some(res)
}

// Computes the left and right powers for the given operator.
// u8 is used to represent the power, the lowest power is 1 (0 is reserved to signify end of input)
fn infix_binding_power(op: Operator) -> Option<(u8, u8)> {
    let res = match op {
        Operator::And | Operator::Or => (3, 4),
        Operator::BangEqual
        | Operator::EqualEqual
        | Operator::Less
        | Operator::LessEqual
        | Operator::Greater
        | Operator::GreaterEqual => (5, 6),
        Operator::Plus | Operator::Minus => (7, 8),
        Operator::Star | Operator::Slash => (9, 10),
        Operator::Field => (16, 15),
        _ => return None,
    };
    Some(res)
}
/*use anyhow::{anyhow, Error, Result};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::fmt::Display;
use std::rc::Rc;

use crate::{Token, TokenType};

pub enum Expression<'a> {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
    EOF,
    Unary(Token, Box<Expression<'a>>),
    Binary(Box<Expression<'a>>, &'a Token, Box<Expression<'a>>),
    Grouping(Box<Expression<'a>>),
}

pub struct AST;

pub struct Parser<'a> {
    tokens: VecDeque<Token>,
    current: u64,
    parse_tree: Vec<Expression<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>) -> Rc<RefCell<Self>> {
        return Rc::new(RefCell::new(Parser {
            tokens: VecDeque::from(tokens),
            current: 0,
            parse_tree: Vec::with_capacity(1),
        }));
    }

    // This is the entry point all call recursive functions
    pub fn parse(parser: Rc<RefCell<Parser<'a>>>) -> Result<Expression, Error> {
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
    pub fn evaluate_literal(&'a mut self) -> Result<Expression<'a>, Error> {
        if self.current as usize >= self.tokens.len() {
            return Ok(Expression::EOF);
        } else {
            let current_token: Token = self.tokens.pop_front().unwrap();
            self.current += 1;
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
                    return Ok(Expression::String(current_token.lexeme));
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
    pub fn evaluate_unary(&'a mut self) -> Result<Expression<'a>, Error> {
        if self.current as usize >= self.tokens.len() - 1 {
            return Ok(Expression::EOF);
        } else {
            let current_token: Token = self.tokens.pop_front().unwrap();
            let next_token: &Token = self.tokens.front().unwrap();

            match current_token.token_type {
                TokenType::Bang | TokenType::Minus | TokenType::Plus => match next_token.token_type
                {
                    TokenType::String
                    | TokenType::Number
                    | TokenType::Nil
                    | TokenType::False
                    | TokenType::True => {
                        self.current += 1;

                        let expression: Expression = match self.evaluate_literal() {
                            Ok(e) => match e {
                                Expression::EOF => {
                                    return Err(anyhow!(
                                        "Error: Incomplete unary, missing expression"
                                    ));
                                }
                                _ => e,
                            },
                            Err(e) => return Err(e),
                        };

                        return Ok(Expression::Unary(current_token, Box::new(expression)));
                    }
                    TokenType::LeftParenthesis => {
                        self.current += 1;

                        let expression: Expression = match self.evaluate_group() {
                            Ok(e) => match e {
                                Expression::EOF => {
                                    return Err(anyhow!(
                                        "Error: Incomplete unary, missing expression"
                                    ));
                                }
                                _ => e,
                            },
                            Err(e) => return Err(e),
                        };
                        return Ok(Expression::Unary(current_token, Box::new(expression)));
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

    fn evaluate_binary(&'a mut self) -> Result<Expression<'a>, Error> {
        if self.current as usize >= self.tokens.len() - 1 {
            return Ok(Expression::EOF);
        } else {
            let current_token: Token = self.tokens.pop_front().unwrap();
            let next_token: &Token = self.tokens.front().unwrap();

            match current_token.token_type {
                TokenType::Bang | TokenType::Plus | TokenType::Minus => {
                    // unary expression

                    let expression = match self.evaluate_unary() {
                        Ok(e) => match e {
                            Expression::EOF => {
                                return Err(anyhow!("Error: Expected expression after parenthesis"))
                            }
                            _ => e,
                        },
                        Err(e) => return Err(e),
                    };

                    return Ok(Expression::Grouping(Box::new(expression)));
                }
                TokenType::String | TokenType::Number => {
                    match next_token.token_type {
                        TokenType::Plus | TokenType::Minus => {
                        },
                        TokenType::Star | TokenType::Slash => {
                        },
                        TokenType::StarEqual | TokenType::SlashEqual | TokenType::PlusEqual | TokenType::MinusEqual => {
                        }
                        _ => return Err(anyhow!("Error: Unimplemented case")),
                    },
                    todo!()
                }
                TokenType::RightParenthesis => {
                    return Err(anyhow!("Error: Empty parenthesis"));
                }
                _ => return Err(anyhow!("Error: unexpected token after parenthesis")),
            }
        }
    }

    fn evaluate_group(&'a mut self) -> Result<Expression<'a>, Error> {
        if self.current as usize >= self.tokens.len() - 1 {
            return Err(anyhow!("Error: Expression expected"));
        } else {
            let current_token: Token = self.tokens.pop_front().unwrap();
            let next_token: &Token = self.tokens.front().unwrap();

            match current_token.token_type {
                TokenType::Bang | TokenType::Plus | TokenType::Minus => {
                    // unary expression
                }
                TokenType::String | TokenType::Number => {}
                TokenType::RightParenthesis => {
                    return Err(anyhow!("Error: Empty parenthesis"));
                }
                _ => return Err(anyhow!("Error: unexpected token after parenthesis")),
            }
        }
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
}*/
