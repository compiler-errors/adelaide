use std::fmt::{Display, Formatter, Result};

use crate::util::Id;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Language symbols
    Dot,         //    .
    DotDot,      //    ..
    Ellipsis,    //    ...
    DotDotEq,    //    ..=
    Comma,       //    ,
    Commalipses, //    ,,,
    Colon,       //    :
    ColonColon,  //    ::
    SemiColon,   //    ;
    ColonLt,     //    :<
    AtSymbol,    //    @
    LBrace,      //    {
    RBrace,      //    }
    LSqBracket,  //    [
    RSqBracket,  //    ]
    Lt,          //    <
    Gt,          //    >
    LParen,      //    (
    RParen,      //    )
    RArrow,      //    ->
    RBigArrow,   //    =>
    Underscore,  //    _
    Question,    //    ?

    // Mathematical operators (excluding LSquare, RSquare)
    LessEqual,    //    <=
    GreaterEqual, //    >=
    EqualsEquals, //    ==
    NotEquals,    //    !=
    And,          //    &
    Pipe,         //    |
    AndShort,     //    &?
    PipeShort,    //    |?
    Equals,       //    =
    Bang,         //    !
    Plus,         //    +
    Minus,        //    -
    Star,         //    *
    Slash,        //    /
    Modulo,       //    %

    // Special keywords -- all are lowercase
    Use,
    Mod,
    Fn,
    Extern,
    Let,
    Trait,
    Impl,
    Where,
    For,
    In,
    As,
    If,
    Else,
    Loop,
    While,
    Break,
    Continue,
    At,
    Return,
    Assert,
    True,
    False,
    Object,
    Struct,
    Type,
    SelfRef,
    Allocate,
    Enum,
    Match,
    Instruction,
    Async,
    Await,

    // Privileged Types
    Int,
    Float,
    Bool,
    StringType,
    Char,
    SelfType,
    FnTrait,
    DynTrait,

    // Literals
    String(Id<str>),
    IntLiteral(Id<str>),
    FloatLiteral(Id<str>),
    InstructionLiteral(Id<str>),
    CharLiteral(char),

    InterpolateBegin(Id<str>),
    InterpolateContinue(Id<str>),
    InterpolateEnd(Id<str>),

    Identifier(Id<str>),
    TypeName(Id<str>),

    /// End of file
    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Token::Dot => write!(f, "\".\""),
            Token::DotDot => write!(f, "\"..\""),
            Token::Ellipsis => write!(f, "\"...\""),
            Token::DotDotEq => write!(f, "\"..=\""),
            Token::Comma => write!(f, "\",\""),
            Token::Commalipses => write!(f, "\",,,\""),
            Token::Colon => write!(f, "\":\""),
            Token::ColonColon => write!(f, "\"::\""),
            Token::ColonLt => write!(f, "\":<\""),
            Token::SemiColon => write!(f, "\";\""),
            Token::AtSymbol => write!(f, "\"@\""),
            Token::LBrace => write!(f, "\"{{\""),
            Token::RBrace => write!(f, "\"}}\""),
            Token::LSqBracket => write!(f, "\"[\""),
            Token::RSqBracket => write!(f, "\"]\""),
            Token::Lt => write!(f, "\"<\""),
            Token::Gt => write!(f, "\">\""),
            Token::LParen => write!(f, "\"(\""),
            Token::RParen => write!(f, "\")\""),
            Token::RArrow => write!(f, "\"->\""),
            Token::RBigArrow => write!(f, "\"=>\""),
            Token::Underscore => write!(f, "\"_\""),
            Token::Question => write!(f, "\"?\""),
            Token::LessEqual => write!(f, "\"<=\""),
            Token::GreaterEqual => write!(f, "\">=\""),
            Token::EqualsEquals => write!(f, "\"==\""),
            Token::NotEquals => write!(f, "\"!=\""),
            Token::And => write!(f, "\"&\""),
            Token::Pipe => write!(f, "\"|\""),
            Token::AndShort => write!(f, "\"&?\""),
            Token::PipeShort => write!(f, "\"|?\""),
            Token::Equals => write!(f, "\"=\""),
            Token::Bang => write!(f, "\"!\""),
            Token::Plus => write!(f, "\"+\""),
            Token::Minus => write!(f, "\"-\""),
            Token::Star => write!(f, "\"*\""),
            Token::Slash => write!(f, "\"/\""),
            Token::Modulo => write!(f, "\"%\""),
            Token::Use => write!(f, "\"use\""),
            Token::Mod => write!(f, "\"mod\""),
            Token::Fn => write!(f, "\"fn\""),
            Token::Extern => write!(f, "\"extern\""),
            Token::Trait => write!(f, "\"trait\""),
            Token::Impl => write!(f, "\"impl\""),
            Token::Where => write!(f, "\"where\""),
            Token::For => write!(f, "\"for\""),
            Token::In => write!(f, "\"in\""),
            Token::As => write!(f, "\"as\""),
            Token::Let => write!(f, "\"let\""),
            Token::If => write!(f, "\"if\""),
            Token::Else => write!(f, "\"else\""),
            Token::Loop => write!(f, "\"loop\""),
            Token::While => write!(f, "\"while\""),
            Token::Break => write!(f, "\"break\""),
            Token::Continue => write!(f, "\"continue\""),
            Token::At => write!(f, "\"at\""),
            Token::Return => write!(f, "\"return\""),
            Token::Assert => write!(f, "\"assert\""),
            Token::True => write!(f, "\"true\""),
            Token::False => write!(f, "\"false\""),
            Token::Object => write!(f, "\"object\""),
            Token::Struct => write!(f, "\"struct\""),
            Token::Type => write!(f, "\"type\""),
            Token::SelfRef => write!(f, "\"self\""),
            Token::Allocate => write!(f, "\"allocate\""),
            Token::Enum => write!(f, "\"enum\""),
            Token::Match => write!(f, "\"match\""),
            Token::Instruction => write!(f, "\"instruction\""),
            Token::Async => write!(f, "\"async\""),
            Token::Await => write!(f, "\"await\""),
            Token::Int => write!(f, "\"Int\""),
            Token::Float => write!(f, "\"Float\""),
            Token::Bool => write!(f, "\"Bool\""),
            Token::Char => write!(f, "\"Char\""),
            Token::StringType => write!(f, "\"String\""),
            Token::SelfType => write!(f, "\"Self\""),
            Token::FnTrait => write!(f, "\"Fn\""),
            Token::DynTrait => write!(f, "\"Dyn\""),
            Token::String(_) => write!(f, "String"),
            Token::InterpolateBegin(_) => write!(f, "InterpolateBegin"),
            Token::InterpolateContinue(_) => write!(f, "InterpolateContinue"),
            Token::InterpolateEnd(_) => write!(f, "InterpolateEnd"),
            Token::IntLiteral(_) => write!(f, "IntLiteral"),
            Token::FloatLiteral(_) => write!(f, "FloatLiteral"),
            Token::CharLiteral(_) => write!(f, "CharLiteral"),
            Token::InstructionLiteral(_) => write!(f, "InstructionLiteral"),
            Token::Identifier(_) => write!(f, "Identifier"),
            Token::TypeName(_) => write!(f, "TypeName"),
            Token::EOF => write!(f, "EOF"),
        }
    }
}
