const std = @import("std");
const testing = std.testing;
const token = @import("token.zig");

pub const Lexer = struct {
    input: []const u8,
    /// current position in input (points to current char)
    position: usize = 0,
    /// current reading position in input (after current char)
    read_position: usize = 0,
    /// current char under examination
    char: u8 = 0,

    pub fn init(input: []const u8) Lexer {
        var lexer = Lexer{
            .input = input,
        };

        // use readchar so our Lexer is in a fully working state before anyone calls NextToken
        lexer.readChar();

        return lexer;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        var tok = token.Token{ .type = token.TokenType.illegal, .literal = &[_]u8{self.char} };

        self.skipWhiteSpace();

        switch (self.char) {
            '=' => {
                const peeked = self.peek(1);
                if (peeked == '>') {
                    tok.type = token.TokenType.fn_return;
                    tok.literal = "=>";
                    self.readChar();
                } else {
                    tok.type = token.TokenType.assign;
                    tok.literal = "=";
                }
            },
            ';' => {
                tok.type = token.TokenType.semi_colon;
                tok.literal = ";";
            },
            '(' => {
                tok.type = token.TokenType.l_paren;
                tok.literal = "(";
            },
            ')' => {
                tok.type = token.TokenType.r_paren;
                tok.literal = ")";
            },
            ',' => {
                tok.type = token.TokenType.comma;
                tok.literal = ",";
            },
            '+' => {
                tok.type = token.TokenType.plus;
                tok.literal = "+";
            },
            '{' => {
                tok.type = token.TokenType.l_brace;
                tok.literal = "{";
            },
            '}' => {
                tok.type = token.TokenType.r_brace;
                tok.literal = "}";
            },
            ':' => {
                const peeked = self.peek(1);
                if (peeked == '=') {
                    tok.type = token.TokenType.declare_assign;
                    tok.literal = ":=";
                    self.readChar();
                } else {
                    tok.type = token.TokenType.declaration;
                    tok.literal = ":";
                }
            },
            0 => {
                tok.type = token.TokenType.eof;
                tok.literal = "";
            },
            else => {
                if (Lexer.isLetter(self.char)) {
                    const ident = self.readIdentifier();
                    tok.literal = ident;
                    tok.type = token.Token.lookupIdent(ident);
                    return tok; // INFO: readIdentifier advances the next char, so we have to return here!
                } else if (Lexer.isDigit(self.char)) {
                    tok.type = token.TokenType.int;
                    tok.literal = self.readNumber();
                    return tok; // INFO: readNumber advances the next char, so we have to return here!
                }
            },
        }

        self.readChar();
        return tok;
    }

    /// give us the next ch (character) and advance the position in the input string
    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.char = 0;
        } else {
            self.char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    // read a whole TokenType.ident if we found a letter
    fn readIdentifier(self: *Lexer) []const u8 {
        const position = self.position;

        while (Lexer.isLetter(self.char)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    fn readNumber(self: *Lexer) []const u8 {
        const position = self.position;

        while (Lexer.isDigit(self.char)) {
            self.readChar();
        }

        return self.input[position..self.position];
    }

    fn peek(self: Lexer, offset: usize) u8 {
        const index = self.position + offset;

        if (index < self.input.len - 1) {
            return self.input[index];
        }

        return 0;
    }

    fn skipWhiteSpace(self: *Lexer) void {
        while (self.char == ' ' or self.char == '\t' or self.char == '\n' or self.char == '\r') {
            self.readChar();
        }
    }

    fn isLetter(char: u8) bool {
        return ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or (char == '_');
    }

    fn isDigit(char: u8) bool {
        return '0' <= char and char <= '9';
    }
};

test "Next Token" {
    const input =
        \\five := 5;
        \\mut ten: usize = 10;
        \\add_stuff := (x: usize, y: usize) => {
        \\  return x + y;
        \\};
        \\result := add_stuff(five, ten);
    ;

    const tests = .{
        .{ .type = token.TokenType.ident, .literal = "five" },
        .{ .type = token.TokenType.declare_assign, .literal = ":=" },
        .{ .type = token.TokenType.int, .literal = "5" },
        .{ .type = token.TokenType.semi_colon, .literal = ";" },
        .{ .type = token.TokenType.mutable, .literal = "mut" },
        .{ .type = token.TokenType.ident, .literal = "ten" },
        .{ .type = token.TokenType.declaration, .literal = ":" },
        .{ .type = token.TokenType.ident, .literal = "usize" },
        .{ .type = token.TokenType.assign, .literal = "=" },
        .{ .type = token.TokenType.int, .literal = "10" },
        .{ .type = token.TokenType.semi_colon, .literal = ";" },
        .{ .type = token.TokenType.ident, .literal = "add_stuff" },
        .{ .type = token.TokenType.declare_assign, .literal = ":=" },
        .{ .type = token.TokenType.l_paren, .literal = "(" },
        .{ .type = token.TokenType.ident, .literal = "x" },
        .{ .type = token.TokenType.declaration, .literal = ":" },
        .{ .type = token.TokenType.ident, .literal = "usize" },
        .{ .type = token.TokenType.comma, .literal = "," },
        .{ .type = token.TokenType.ident, .literal = "y" },
        .{ .type = token.TokenType.declaration, .literal = ":" },
        .{ .type = token.TokenType.ident, .literal = "usize" },
        .{ .type = token.TokenType.r_paren, .literal = ")" },
        .{ .type = token.TokenType.fn_return, .literal = "=>" },
        .{ .type = token.TokenType.l_brace, .literal = "{" },
        .{ .type = token.TokenType.block_return, .literal = "return" },
        .{ .type = token.TokenType.ident, .literal = "x" },
        .{ .type = token.TokenType.plus, .literal = "+" },
        .{ .type = token.TokenType.ident, .literal = "y" },
        .{ .type = token.TokenType.semi_colon, .literal = ";" },
        .{ .type = token.TokenType.r_brace, .literal = "}" },
        .{ .type = token.TokenType.semi_colon, .literal = ";" },
        .{ .type = token.TokenType.ident, .literal = "result" },
        .{ .type = token.TokenType.declare_assign, .literal = ":=" },
        .{ .type = token.TokenType.ident, .literal = "add_stuff" },
        .{ .type = token.TokenType.l_paren, .literal = "(" },
        .{ .type = token.TokenType.ident, .literal = "five" },
        .{ .type = token.TokenType.comma, .literal = "," },
        .{ .type = token.TokenType.ident, .literal = "ten" },
        .{ .type = token.TokenType.r_paren, .literal = ")" },
    };

    var lexer = Lexer.init(input);

    // std.debug.print("\n-\n", .{}); // TODO: this adds a new line for testing output. How to do this better?, TODO: add this print without the test failing
    inline for (tests) |test_item| {
        const tok = lexer.nextToken();
        // std.debug.print("{}.token: Type: {}, Literal: {s}\n", .{ i, tok.type, tok.literal });

        try testing.expectEqual(test_item.type, tok.type);
        try testing.expectEqualStrings(test_item.literal, tok.literal);
    }
}
