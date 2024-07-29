const std = @import("std");

pub const TokenType = enum {
    illegal,
    eof,

    // Indentifiers + literals
    ident,

    int, // digits -> they will later
    string, // datastructure for []u8 utf-8 strings, basicly []u8 but with extra methods
    char, // 'a' single bit representation of a string as u8

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
    fn_return, // TODO: rename? | =>
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
