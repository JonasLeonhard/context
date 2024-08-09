const std = @import("std");
const testing = std.testing;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TokenLiteral = @import("token.zig").TokenLiteral;

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
        var tok = Token{ .type = TokenType.Unknown, .literal = &[_]u8{self.char} };

        self.skipWhiteSpace();

        switch (self.char) {
            0 => {
                tok.type = TokenType.Eof;
                tok.literal = "";
            },
            '=' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.EqEq;
                        tok.literal = "==";
                        self.eatChar();
                    },
                    '>' => {
                        tok.type = TokenType.Arrow;
                        tok.literal = "=>";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Eq;
                        tok.literal = "=";
                    },
                }
            },
            ':' => {
                const peeked = self.peek(0);
                if (peeked == '=') {
                    tok.type = TokenType.DeclareAssign;
                    tok.literal = ":=";
                    self.eatChar();
                } else {
                    tok.type = TokenType.Colon;
                    tok.literal = ":";
                }
            },
            '+' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.AddAssign;
                        tok.literal = "+=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Plus;
                        tok.literal = "+";
                    },
                }
            },
            '-' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.SubAssign;
                        tok.literal = "-=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Minus;
                        tok.literal = "-";
                    },
                }
            },
            '!' => {
                const peeked = self.peek(0);

                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.NotEq;
                        tok.literal = "!=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Bang;
                        tok.literal = "!";
                    },
                }
            },
            '*' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.MulAssign;
                        tok.literal = "*=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Star;
                        tok.literal = "*";
                    },
                }
            },
            '/' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '/' => {
                        tok.type = TokenType.LineComment;
                        tok.literal = "//";
                        self.eatChar();
                    },
                    '*' => {
                        // TODO: compiler error if end of comment not found!
                        const start_pos = self.position;
                        var end_pos = self.position;
                        self.eatChar(); // Eat the '*'
                        while (true) {
                            if (self.char == '*' and self.peek(0) == '/') {
                                self.eatChar(); // Eat the '*'
                                self.eatChar(); // Eat the '/'
                                end_pos = self.position;
                                break;
                            } else if (self.char == '0') {
                                // TODO: throw real error here!
                                std.debug.print("Error: Unterminated block comment starting at position {d}\n", .{start_pos});
                                break;
                            } else {
                                self.eatChar();
                            }
                        }

                        tok.type = TokenType.BlockComment;
                        tok.literal = self.input[start_pos..end_pos];
                    },
                    '=' => {
                        tok.type = TokenType.DivAssign;
                        tok.literal = "/=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Slash;
                        tok.literal = "/";
                    },
                }
            },
            '<' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '<' => {
                        tok.type = TokenType.LtLt;
                        tok.literal = "<<";
                        self.eatChar();

                        const peeked_2 = self.peek(0);
                        if (peeked_2 == '=') {
                            tok.type = TokenType.LtLtAssign;
                            tok.literal = "<<=";
                            self.eatChar();
                        }
                    },
                    else => {
                        tok.type = TokenType.Lt;
                        tok.literal = "<";
                    },
                }
            },
            '>' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '>' => {
                        tok.type = TokenType.GtGt;
                        tok.literal = ">>";
                        self.eatChar();

                        const peeked_2 = self.peek(0);
                        if (peeked_2 == '=') {
                            tok.type = TokenType.GtGtAssign;
                            tok.literal = ">>=";
                            self.eatChar();
                        }
                    },
                    else => {
                        tok.type = TokenType.Gt;
                        tok.literal = ">";
                    },
                }
            },
            ',' => {
                tok.type = TokenType.Comma;
                tok.literal = ",";
            },
            ';' => {
                tok.type = TokenType.Semi;
                tok.literal = ";";
            },
            '(' => {
                tok.type = TokenType.OpenParen;
                tok.literal = "(";
            },
            ')' => {
                tok.type = TokenType.CloseParen;
                tok.literal = ")";
            },
            '{' => {
                tok.type = TokenType.OpenBrace;
                tok.literal = "{";
            },
            '}' => {
                tok.type = TokenType.CloseBrace;
                tok.literal = "}";
            },
            '[' => {
                tok.type = TokenType.OpenBracket;
                tok.literal = "[";
            },
            ']' => {
                tok.type = TokenType.CloseBracket;
                tok.literal = "]";
            },
            '&' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '&' => {
                        tok.type = TokenType.AndAnd;
                        tok.literal = "&&";
                        self.eatChar();
                    },
                    '=' => {
                        tok.type = TokenType.BitwiseAssign;
                        tok.literal = "&=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.And;
                        tok.literal = "&";
                    },
                }
            },
            '.' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '.' => {
                        tok.type = TokenType.DotDot;
                        tok.literal = "..";
                        self.eatChar();

                        const peeked_2 = self.peek(0);
                        if (peeked_2 == '=') {
                            tok.type = TokenType.DotDotEq;
                            tok.literal = "..=";
                            self.eatChar();
                        }
                    },
                    else => {
                        tok.type = TokenType.Dot;
                        tok.literal = ".";
                    },
                }
            },
            '@' => {
                tok.type = TokenType.At;
                tok.literal = "@";
            },
            '#' => {
                tok.type = TokenType.Pound;
                tok.literal = "#";
            },
            '~' => {
                tok.type = TokenType.Tilde;
                tok.literal = "~";
            },
            '?' => {
                tok.type = TokenType.Question;
                tok.literal = "?";
            },
            '$' => {
                tok.type = TokenType.Dollar;
                tok.literal = "$";
            },
            '|' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '|' => {
                        tok.type = TokenType.OrOr;
                        tok.literal = "||";
                        self.eatChar();
                    },
                    '=' => {
                        tok.type = TokenType.OrAssign;
                        tok.literal = "|=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Or;
                        tok.literal = "|";
                    },
                }
            },
            '\\' => {
                tok.type = TokenType.BackSlash;
                tok.literal = "\\";
            },
            '^' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.CaretAssign;
                        tok.literal = "^=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Caret;
                        tok.literal = "^";
                    },
                }
            },
            '%' => {
                const peeked = self.peek(0);
                switch (peeked) {
                    '=' => {
                        tok.type = TokenType.ArithmeticAssign;
                        tok.literal = "%=";
                        self.eatChar();
                    },
                    else => {
                        tok.type = TokenType.Percent;
                        tok.literal = "%";
                    },
                }
            },
            else => {
                if (Lexer.isLetter(self.char)) {
                    const ident = self.eatIdentifier();
                    tok.literal = ident;
                    tok.type = Token.lookupIdent(ident);
                    return tok; // INFO: readIdentifier advances the next char, so we have to return here!
                } else if (Lexer.isDigit(self.char)) {
                    tok.type = TokenType{ .Literal = TokenLiteral.Int }; // TODO: implement other token literal types!
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

        if (index < self.input.len and index > 0) {
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
        // ------- Special -------
        \\xyz
        // ------- Literals -------
        \\10
        // ------- Keywords -------
        \\mut
        \\return
        \\if
        \\else
        \\false
        \\true
        // ------- One Char Tokens -------
        \\;
        \\,
        \\.
        \\(
        \\)
        \\{
        \\}
        \\[
        \\]
        \\@
        \\#
        \\~
        \\?
        \\:
        \\$
        \\=
        \\!
        \\<
        \\>
        \\-
        \\&
        \\|
        \\+
        \\*
        \\/
        \\\
        \\^
        \\%
        // ------- Two-char tokens: --------
        \\//
        \\/* block comment */
        \\:=
        \\=>
        \\&&
        \\||
        \\==
        \\!=
        \\%=
        \\&=
        \\*=
        \\+=
        \\-=
        \\..
        \\..=
        \\/=
        \\<<
        \\<<=
        \\>>
        \\>>=
        \\^=
        \\|=
    ;

    const tests = .{
        .{ TokenType.Ident, "xyz" },
        .{ TokenType{ .Literal = TokenLiteral.Int }, "10" },
        .{ TokenType.Mut, "mut" },
        .{ TokenType.Return, "return" },
        .{ TokenType.If, "if" },
        .{ TokenType.Else, "else" },
        .{ TokenType.False, "false" },
        .{ TokenType.True, "true" },
        .{ TokenType.Semi, ";" },
        .{ TokenType.Comma, "," },
        .{ TokenType.Dot, "." },
        .{ TokenType.OpenParen, "(" },
        .{ TokenType.CloseParen, ")" },
        .{ TokenType.OpenBrace, "{" },
        .{ TokenType.CloseBrace, "}" },
        .{ TokenType.OpenBracket, "[" },
        .{ TokenType.CloseBracket, "]" },
        .{ TokenType.At, "@" },
        .{ TokenType.Pound, "#" },
        .{ TokenType.Tilde, "~" },
        .{ TokenType.Question, "?" },
        .{ TokenType.Colon, ":" },
        .{ TokenType.Dollar, "$" },
        .{ TokenType.Eq, "=" },
        .{ TokenType.Bang, "!" },
        .{ TokenType.Lt, "<" },
        .{ TokenType.Gt, ">" },
        .{ TokenType.Minus, "-" },
        .{ TokenType.And, "&" },
        .{ TokenType.Or, "|" },
        .{ TokenType.Plus, "+" },
        .{ TokenType.Star, "*" },
        .{ TokenType.Slash, "/" },
        .{ TokenType.BackSlash, "\\" },
        .{ TokenType.Caret, "^" },
        .{ TokenType.Percent, "%" },
        .{ TokenType.LineComment, "//" },
        .{ TokenType.BlockComment, "/* block comment */" },
        .{ TokenType.DeclareAssign, ":=" },
        .{ TokenType.Arrow, "=>" },
        .{ TokenType.AndAnd, "&&" },
        .{ TokenType.OrOr, "||" },
        .{ TokenType.EqEq, "==" },
        .{ TokenType.NotEq, "!=" },
        .{ TokenType.ArithmeticAssign, "%=" },
        .{ TokenType.BitwiseAssign, "&=" },
        .{ TokenType.MulAssign, "*=" },
        .{ TokenType.AddAssign, "+=" },
        .{ TokenType.SubAssign, "-=" },
        .{ TokenType.DotDot, ".." },
        .{ TokenType.DotDotEq, "..=" },
        .{ TokenType.DivAssign, "/=" },
        .{ TokenType.LtLt, "<<" },
        .{ TokenType.LtLtAssign, "<<=" },
        .{ TokenType.GtGt, ">>" },
        .{ TokenType.GtGtAssign, ">>=" },
        .{ TokenType.CaretAssign, "^=" },
        .{ TokenType.OrAssign, "|=" },
    };

    var lexer = Lexer.init(input);

    std.debug.print("\n-\n", .{}); // TODO: this adds a new line for testing output. How to do this better?, TODO: add this print without the test failing
    inline for (0.., tests) |i, test_item| {
        const tok = lexer.nextToken();
        std.debug.print("{}.token: Type: {}, Literal: {s}\n", .{ i, tok.type, tok.literal });

        try testing.expectEqual(test_item[0], tok.type);
        try testing.expectEqualStrings(test_item[1], tok.literal);
    }
}
