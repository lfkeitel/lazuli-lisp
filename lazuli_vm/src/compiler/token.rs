#[derive(Copy, Clone, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,
    Comment,
    Quasiquote,
    Symbol,
    Keyword,
    Number,
    Float,
    String,
    RParen,
    LParen,
    Quote,
    Unquote,
    At,
    Mark,
}

impl ::std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenType::Illegal => "Illegal",
                TokenType::Eof => "Eof",
                TokenType::Comment => "Comment",
                TokenType::Quasiquote => "Quasiquote",
                TokenType::Symbol => "Symbol",
                TokenType::Keyword => "Keyword",
                TokenType::Number => "Number",
                TokenType::Float => "Float",
                TokenType::String => "String",
                TokenType::RParen => "RParen",
                TokenType::LParen => "LParen",
                TokenType::Quote => "Quote",
                TokenType::Unquote => "Unquote",
                TokenType::At => "At",
                TokenType::Mark => "Mark",
            }
        )
    }
}

impl ::std::fmt::Debug for TokenType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Debug)]
pub struct Token {
    pub ttype: TokenType,
    pub literal: String,
    pub line: u32,
    pub col: u32,
    pub file: String,
}

impl Token {
    pub fn with_literal(t: TokenType, lit: String, line: u32, col: u32, file: &str) -> Self {
        Token {
            ttype: t,
            literal: lit,
            line,
            col,
            file: file.to_owned(),
        }
    }

    pub fn simple(t: TokenType, line: u32, col: u32, file: &str) -> Self {
        Self::with_literal(t, "".to_string(), line, col, file)
    }
}
