const std = @import("std");

pub const TokenType = enum {
    illegal,
    eof,

    // Indentifiers + literals
    ident,

    // TODO: what do i really need here?
    // types: numbers
    int,
    float,
    usize,
    u128,
    u64,
    u32,
    u16,
    u8,
    isize,
    i128,
    i64,
    i32,
    i16,
    i8,
    f128,
    f64,
    f32,
    f16,
    f8,

    // types: strings
    string,

    // Operators
    plus,
    minus,

    // Delimiters
    comma,
    semi_colon,

    l_paren,
    r_paren,
    l_brace,
    r_brace,

    // Keywords
    function, // () => {}
    assign, // =
    declaration, // :
    mutable, // mut
    block_return, // return
    return_block, // =>
};

const ident_map = std.StaticStringMap(TokenType).initComptime(.{
    .{ "mut", TokenType.mutable },
    .{ "return", TokenType.block_return },
});

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    pub fn lookupIdent(ident: []const u8) TokenType {
        const possible_type = ident_map.get(ident);
        if (possible_type) |actual_type| {
            return actual_type;
        }
        return TokenType.ident;
    }
};
