use std::fmt;

use super::token::{self, Token, TokenType};
use crate::object;

pub enum ParserError {
    InvalidCode(String),
    ExpectedToken(String),
    ValidationError(String),
    FileNotFound(String),
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::InvalidCode(s) => write!(f, "{}", s),
            ParserError::ExpectedToken(s) => write!(f, "{}", s),
            ParserError::ValidationError(s) => write!(f, "{}", s),
            ParserError::FileNotFound(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParserError::InvalidCode(s) => write!(f, "{}", s),
            ParserError::ExpectedToken(s) => write!(f, "{}", s),
            ParserError::ValidationError(s) => write!(f, "{}", s),
            ParserError::FileNotFound(s) => write!(f, "{}", s),
        }
    }
}

pub struct Parser<'a> {
    lexer: Box<&'a mut dyn Iterator<Item = Token>>,
    cur_tok: Token,
    peek_tok: Token,
}

impl<'a> Parser<'a> {
    pub fn new<I>(lexer: &'a mut I) -> Self
    where
        I: Iterator<Item = Token>,
    {
        let cur = lexer
            .next()
            .unwrap_or_else(|| token::Token::simple(TokenType::Eof, 0, 0, ""));
        let peek = lexer
            .next()
            .unwrap_or_else(|| token::Token::simple(TokenType::Eof, 0, 0, ""));

        Parser {
            lexer: Box::new(lexer),
            cur_tok: cur,
            peek_tok: peek,
        }
    }

    pub fn parse(mut self) -> Result<object::Program, ParserError> {
        let mut forms = Vec::new();

        while self.cur_tok.ttype != TokenType::Eof {
            let res: Result<object::Node, ParserError> = match self.cur_tok.ttype {
                // Skip empty lines
                TokenType::Comment => {
                    self.read_token();
                    continue;
                }

                TokenType::LParen => self.parse_list(),

                _ => Err(ParserError::InvalidCode(format!(
                    "{}: line {}, col {} Expected (, got {}",
                    self.cur_tok.file, self.cur_tok.line, self.cur_tok.col, self.cur_tok.ttype
                ))),
            };

            match res {
                Ok(node) => forms.push(node),
                Err(e) => return Err(e),
            };

            self.read_token()
        }

        Ok(forms
            .into_iter()
            .rev()
            .fold(object::cons_list::ConsList::new(), |acc, elem| {
                acc.append(elem)
            }))
    }

    fn read_token(&mut self) {
        self.cur_tok = self.peek_tok.clone();
        self.peek_tok = self
            .lexer
            .next()
            .unwrap_or_else(|| token::Token::simple(TokenType::Eof, 0, 0, ""));

        while self.peek_tok.ttype == TokenType::Comment {
            self.peek_tok = self
                .lexer
                .next()
                .unwrap_or_else(|| token::Token::simple(TokenType::Eof, 0, 0, ""));
        }

        // dbg!(&self.cur_tok);
    }

    // Utility methods
    fn cur_token_is(&self, t: TokenType) -> bool {
        self.cur_tok.ttype == t
    }

    // fn parse_err(&self, msg: &str) -> ParserError {
    //     ParserError::InvalidCode(format!(
    //         "{} on line {} in {}",
    //         msg, self.cur_tok.line, self.cur_tok.file
    //     ))
    // }

    // fn token_err(&self, t: TokenType) -> ParserError {
    //     ParserError::ExpectedToken(format!(
    //         "expected {} on line {} in {}, got {}",
    //         t, self.cur_tok.line, self.cur_tok.file, self.cur_tok.ttype
    //     ))
    // }

    fn tokens_err(&self, t: &[TokenType]) -> ParserError {
        ParserError::ExpectedToken(format!(
            "expected {:?} on line {} in {}, got {}",
            t, self.cur_tok.line, self.cur_tok.file, self.cur_tok.ttype
        ))
    }

    // fn expect_token(&mut self, t: TokenType) -> Result<(), ParserError> {
    //     self.read_token();
    //     if !self.cur_token_is(t) {
    //         Err(self.token_err(t))
    //     } else {
    //         Ok(())
    //     }
    // }

    fn parse_list(&mut self) -> Result<object::Node, ParserError> {
        let mut elems = Vec::new();
        self.read_token();

        while !self.cur_token_is(TokenType::RParen) {
            elems.push(self.parse_item()?);
            self.read_token();
        }

        Ok(object::Node::List(
            elems
                .into_iter()
                .rev()
                .fold(object::cons_list::ConsList::new(), |acc, elem| {
                    acc.append(elem)
                }),
        ))
    }

    fn parse_item(&mut self) -> Result<object::Node, ParserError> {
        Ok(match self.cur_tok.ttype {
            TokenType::Symbol => {
                let s = object::Symbol::new(&self.cur_tok.literal);
                object::Node::Symbol(s.into_ref())
            }

            TokenType::Keyword => object::Node::new_keyword(&self.cur_tok.literal),

            TokenType::Number => {
                let n = parse_u64(&self.cur_tok.literal).ok_or_else(|| {
                    ParserError::InvalidCode(format!(
                        "{}: line {}, col {} Failed parsing number",
                        self.cur_tok.file, self.cur_tok.line, self.cur_tok.col,
                    ))
                })?;
                object::Node::Number(n)
            }

            TokenType::Float => {
                let n = self.cur_tok.literal.parse().map_err(|_| {
                    ParserError::InvalidCode(format!(
                        "{}: line {}, col {} Failed parsing float",
                        self.cur_tok.file, self.cur_tok.line, self.cur_tok.col,
                    ))
                })?;
                object::Node::Float(n)
            }

            TokenType::String => object::Node::String(self.cur_tok.literal.clone()),
            TokenType::LParen => self.parse_list()?,

            TokenType::Quote => {
                self.read_token();
                let elem = self.parse_item()?;
                object::Node::List(
                    object::cons_list::ConsList::new()
                        .append(elem)
                        .append(object::Symbol::new("quote").into_node()),
                )
            }

            TokenType::Quasiquote => {
                self.read_token();
                let elem = self.parse_item()?;
                if let object::Node::List(_) = elem {
                    object::Node::List(
                        object::cons_list::ConsList::new()
                            .append(elem)
                            .append(object::Symbol::new("quasiquote").into_node()),
                    )
                } else {
                    return Err(ParserError::InvalidCode(format!(
                        "quasiquote can only be used on a list, got {}",
                        elem.type_str()
                    )));
                }
            }

            TokenType::Mark => {
                self.read_token();
                let elem = self.parse_item()?;
                object::Node::List(
                    object::cons_list::ConsList::new()
                        .append(elem)
                        .append(object::Symbol::new("stop-unquote").into_node()),
                )
            }

            TokenType::Unquote => {
                self.read_token();
                let unquote_func = if self.cur_token_is(TokenType::At) {
                    self.read_token();
                    "unquote-splice"
                } else {
                    "unquote"
                };

                let elem = self.parse_item()?;
                object::Node::List(
                    object::cons_list::ConsList::new()
                        .append(elem)
                        .append(object::Symbol::new(unquote_func).into_node()),
                )
            }

            _ => {
                return Err(self.tokens_err(&[
                    TokenType::LParen,
                    TokenType::Number,
                    TokenType::Float,
                    TokenType::String,
                    TokenType::Symbol,
                    TokenType::Keyword,
                    TokenType::Quote,
                    TokenType::Quasiquote,
                ]));
            }
        })
    }
}

fn parse_u64(s: &str) -> Option<i64> {
    if s.starts_with("0x") {
        match i64::from_str_radix(s.trim_start_matches("0x"), 16) {
            Ok(n) => Some(n),
            Err(_) => None,
        }
    } else {
        match s.parse::<i64>() {
            Ok(n) => Some(n),
            Err(_) => None,
        }
    }
}
