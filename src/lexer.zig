const std = @import("std");
const testing = std.testing;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;

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
        lexer.eatChar();

        return lexer;
    }

    pub fn nextToken(self: *Lexer) Token {
        var tok = Token{ .type = TokenType.illegal, .literal = &[_]u8{self.char} };

        self.skipWhiteSpace();

        switch (self.char) {
            0 => {
                tok.type = TokenType.eof;
                tok.literal = "";
            },
            '=' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '>' => {
                        tok.type = TokenType.fn_return;
                        tok.literal = "=>";
                        self.eatChar();
                    },
                    '=' => {
                        tok.type = TokenType.equal;
                        tok.literal = "==";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.assign;
                        tok.literal = "=";
                    },
                }
            },
            ':' => {
                const peeked = self.peek(0);
                if (peeked == '=') {
                    tok.type = TokenType.declare_assign;
                    tok.literal = ":=";
                    self.eatChar();
                } else {
                    tok.type = TokenType.declaration;
                    tok.literal = ":";
                }
            },
            '+' => {
                tok.type = TokenType.plus;
                tok.literal = "+";
            },
            '-' => {
                tok.type = TokenType.minus;
                tok.literal = "-";
            },
            '!' => {
                const peeked = self.peek(0);

                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.not_equal;
                        tok.literal = "!=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.bang;
                        tok.literal = "!";
                    },
                }
            },
            '*' => {
                tok.type = TokenType.asterisk;
                tok.literal = "*";
            },
            '/' => {
                tok.type = TokenType.slash;
                tok.literal = "/";
            },
            '<' => {
                tok.type = TokenType.lt;
                tok.literal = "<";
            },
            '>' => {
                tok.type = TokenType.gt;
                tok.literal = ">";
            },
            ',' => {
                tok.type = TokenType.comma;
                tok.literal = ",";
            },
            ';' => {
                tok.type = TokenType.semi_colon;
                tok.literal = ";";
            },
            '(' => {
                tok.type = TokenType.l_paren;
                tok.literal = "(";
            },
            ')' => {
                tok.type = TokenType.r_paren;
                tok.literal = ")";
            },
            '{' => {
                tok.type = TokenType.l_brace;
                tok.literal = "{";
            },
            '}' => {
                tok.type = TokenType.r_brace;
                tok.literal = "}";
            },
            else => {
                if (Lexer.isLetter(self.char)) {
                    const ident = self.eatIdentifier();
                    tok.literal = ident;
                    tok.type = Token.lookupIdent(ident);
                    return tok; // INFO: readIdentifier advances the next char, so we have to return here!
                } else if (Lexer.isDigit(self.char)) {
                    tok.type = TokenType.int;
                    tok.literal = self.eatNumber();
                    return tok; // INFO: readNumber advances the next char, so we have to return here!
                }
            },
        }

        self.eatChar();
        return tok;
    }

    /// give us the next ch (character) and advance the position in the input string
    fn eatChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.char = 0;
        } else {
            self.char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    // read a whole TokenType.ident if we found a letter
    fn eatIdentifier(self: *Lexer) []const u8 {
        const position = self.position;

        while (Lexer.isLetter(self.char)) {
            self.eatChar();
        }

        return self.input[position..self.position];
    }

    fn eatNumber(self: *Lexer) []const u8 {
        const position = self.position;

        while (Lexer.isDigit(self.char)) {
            self.eatChar();
        }

        return self.input[position..self.position];
    }

    /// peeks from the current read_position
    fn peek(self: Lexer, look_ahead: usize) u8 {
        const index = self.read_position + look_ahead;

        if (index < self.input.len - 1 and index > 0) {
            return self.input[index];
        }

        return 0;
    }

    fn skipWhiteSpace(self: *Lexer) void {
        while (self.char == ' ' or self.char == '\t' or self.char == '\n' or self.char == '\r') {
            self.eatChar();
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
        \\!-/*5;
        \\if ten < 11 and ten > 9 or false {
        \\return true;
        \\}
        \\10 == 10;
        \\10 != 901;
    ;

    const tests = .{
        .{ .type = TokenType.ident, .literal = "five" },
        .{ .type = TokenType.declare_assign, .literal = ":=" },
        .{ .type = TokenType.int, .literal = "5" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.mutable, .literal = "mut" },
        .{ .type = TokenType.ident, .literal = "ten" },
        .{ .type = TokenType.declaration, .literal = ":" },
        .{ .type = TokenType.ident, .literal = "usize" },
        .{ .type = TokenType.assign, .literal = "=" },
        .{ .type = TokenType.int, .literal = "10" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.ident, .literal = "add_stuff" },
        .{ .type = TokenType.declare_assign, .literal = ":=" },
        .{ .type = TokenType.l_paren, .literal = "(" },
        .{ .type = TokenType.ident, .literal = "x" },
        .{ .type = TokenType.declaration, .literal = ":" },
        .{ .type = TokenType.ident, .literal = "usize" },
        .{ .type = TokenType.comma, .literal = "," },
        .{ .type = TokenType.ident, .literal = "y" },
        .{ .type = TokenType.declaration, .literal = ":" },
        .{ .type = TokenType.ident, .literal = "usize" },
        .{ .type = TokenType.r_paren, .literal = ")" },
        .{ .type = TokenType.fn_return, .literal = "=>" },
        .{ .type = TokenType.l_brace, .literal = "{" },
        .{ .type = TokenType.block_return, .literal = "return" },
        .{ .type = TokenType.ident, .literal = "x" },
        .{ .type = TokenType.plus, .literal = "+" },
        .{ .type = TokenType.ident, .literal = "y" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.r_brace, .literal = "}" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.ident, .literal = "result" },
        .{ .type = TokenType.declare_assign, .literal = ":=" },
        .{ .type = TokenType.ident, .literal = "add_stuff" },
        .{ .type = TokenType.l_paren, .literal = "(" },
        .{ .type = TokenType.ident, .literal = "five" },
        .{ .type = TokenType.comma, .literal = "," },
        .{ .type = TokenType.ident, .literal = "ten" },
        .{ .type = TokenType.r_paren, .literal = ")" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.bang, .literal = "!" },
        .{ .type = TokenType.minus, .literal = "-" },
        .{ .type = TokenType.slash, .literal = "/" },
        .{ .type = TokenType.asterisk, .literal = "*" },
        .{ .type = TokenType.int, .literal = "5" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.if_, .literal = "if" },
        .{ .type = TokenType.ident, .literal = "ten" },
        .{ .type = TokenType.lt, .literal = "<" },
        .{ .type = TokenType.int, .literal = "11" },
        .{ .type = TokenType.and_, .literal = "and" },
        .{ .type = TokenType.ident, .literal = "ten" },
        .{ .type = TokenType.gt, .literal = ">" },
        .{ .type = TokenType.int, .literal = "9" },
        .{ .type = TokenType.or_, .literal = "or" },
        .{ .type = TokenType.false, .literal = "false" },
        .{ .type = TokenType.l_brace, .literal = "{" },
        .{ .type = TokenType.block_return, .literal = "return" },
        .{ .type = TokenType.true, .literal = "true" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.r_brace, .literal = "}" },
        .{ .type = TokenType.int, .literal = "10" },
        .{ .type = TokenType.equal, .literal = "==" },
        .{ .type = TokenType.int, .literal = "10" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
        .{ .type = TokenType.int, .literal = "10" },
        .{ .type = TokenType.not_equal, .literal = "!=" },
        .{ .type = TokenType.int, .literal = "901" },
        .{ .type = TokenType.semi_colon, .literal = ";" },
    };

    var lexer = Lexer.init(input);

    std.debug.print("\n-\n", .{}); // TODO: this adds a new line for testing output. How to do this better?, TODO: add this print without the test failing
    inline for (0.., tests) |i, test_item| {
        const tok = lexer.nextToken();
        std.debug.print("{}.token: Type: {}, Literal: {s}\n", .{ i, tok.type, tok.literal });

        try testing.expectEqual(test_item.type, tok.type);
        try testing.expectEqualStrings(test_item.literal, tok.literal);
    }
}
