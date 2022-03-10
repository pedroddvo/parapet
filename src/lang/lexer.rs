use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum Token {
    #[regex(r"[0-9]+\.[0-9]+")]
    NumberFloat,

    #[regex(r"0x[0-9a-fA-F]+")]
    NumberHex,

    #[regex(r"[0-9]+")]
    NumberInt,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*\??")]
    Symbol,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,

    #[token("->")]
    Arrow,
    #[token("=")]
    Equals,

    #[token("def")]
    KeywordDef,
    #[token("const")]
    KeywordConst,
    #[token("end")]
    KeywordEnd,
    #[token("do")]
    KeywordDo,

    // user defined operator
    #[regex("[+-/*><][+-/*>=<]*")]
    Operator,

    #[regex("\n+")]
    Newline,

    #[error]
    #[regex(r"[ \t\r\f]+", logos::skip)]
    Error,
}

pub type Span = logos::Span;

pub fn lexer(src: &str) -> logos::Lexer<Token> {
    Token::lexer(src)
}
