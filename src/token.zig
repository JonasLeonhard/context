const std = @import("std");

pub const Location = struct {
    start: usize,
    end: usize,
};

pub const TokenType = union(enum) {
    // -------- Special --------
    /// Token not known to the compiler
    Unknown,

    /// End of file
    Eof,

    /// "ident" or "continue"
    /// At this step, keywords are also considered identifiers.
    Ident,

    /// Strings, floats ... example: `12u8`, `1.0e-40`, `b"123"`
    Literal: TokenLiteral,

    // -------- Keywords --------
    /// mut
    Mut,
    /// return
    Return,
    /// if
    If,
    /// else
    Else,
    /// false
    False,
    /// true
    True,
    /// null
    Null,
    /// fn
    Fn,
    /// where
    Where,
    /// pub
    Pub,
    /// enum
    Enum,
    /// Struct
    Struct,
    /// impl
    Impl,
    /// switch
    Switch,
    /// for
    For,
    /// While
    While,
    /// Break
    Break,
    /// defer
    Defer,

    //  -------- One-char tokens:  --------
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "@"
    At,
    /// "#"
    Pound,
    /// "~"
    Tilde,
    /// "?"
    Question,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Eq,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "&"
    And,
    /// "|"
    Or,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "\"
    BackSlash,
    /// "^"
    Caret,
    /// "%"
    Percent,

    // ------- Two-char tokens: --------
    /// "// comment"
    LineComment,
    /// `/* block comment */`
    ///
    /// Block comments can be recursive, so a sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment,
    /// Declare assign ':='
    DeclareAssign,
    /// "=>"
    Arrow,
    /// &&
    AndAnd,
    /// ||
    OrOr,
    /// ==
    EqEq,
    /// !=
    NotEq,
    /// %=
    ArithmeticAssign,
    /// &=
    BitwiseAssign,
    /// *=
    MulAssign,
    /// +=
    AddAssign,
    /// -=
    SubAssign,
    /// ..
    DotDot,
    /// ..=
    DotDotEq,
    /// /=
    DivAssign,
    /// <<
    LtLt,
    /// <<=
    LtLtAssign,
    // >>
    GtGt,
    // >>=
    GtGtAssign,
    // ^=
    CaretAssign,
    // |=
    OrAssign,

    pub fn compareEq(self: TokenType, other: TokenType) bool {
        return switch (self) {
            TokenType.Literal => |a_literal| switch (other) {
                TokenType.Literal => |b_literal| a_literal == b_literal,
                else => false,
            },
            else => @intFromEnum(self) == @intFromEnum(other),
        };
    }
};

pub const TokenLiteral = enum {
    /// "12_u8", "0o100", "0b120i99", "1f32".
    Int,
    /// "12.34f32", "1e3", but not "1f32".
    Float,
    /// "'a'", "'\\'", "'''", "';"
    Char,
    /// "b'a'", "b'\\'", "b'''", "b';"
    Byte,
    /// ""abc"", ""abc"
    Str,
    /// "b"abc"", "b"abc"
    ByteStr,
    /// `c"abc"`, `c"abc`
    CStr,
    /// "r"abc"", "r#"abc"#", "r####"ab"###"c"####", "r#"a". `None` indicates
    /// an invalid literal.
    RawStr,
    /// "br"abc"", "br#"abc"#", "br####"ab"###"c"####", "br#"a". `None`
    /// indicates an invalid literal.
    RawByteStr,
    /// `cr"abc"`, "cr#"abc"#", `cr#"a`. `None` indicates an invalid literal.
    RawCStr,
};

const ident_map = std.StaticStringMap(TokenType).initComptime(.{
    .{ "mut", TokenType.Mut },
    .{ "return", TokenType.Return },
    .{ "true", TokenType.True },
    .{ "false", TokenType.False },
    .{ "if", TokenType.If },
    .{ "else", TokenType.Else },
    .{ "fn", TokenType.Fn },
    .{ "where", TokenType.Where },
    .{ "pub", TokenType.Pub },
    .{ "enum", TokenType.Enum },
    .{ "struct", TokenType.Struct },
    .{ "impl", TokenType.Impl },
    .{ "switch", TokenType.Switch },
    .{ "for", TokenType.For },
    .{ "while", TokenType.While },
    .{ "break", TokenType.Break },
    .{ "defer", TokenType.Defer },
    .{ "null", TokenType.Null },
});

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    location: ?Location,

    pub fn lookupIdent(ident: []const u8) TokenType {
        const possible_type = ident_map.get(ident);
        if (possible_type) |actual_type| {
            return actual_type;
        }
        return TokenType.Ident;
    }
};
