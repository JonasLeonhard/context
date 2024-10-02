const std = @import("std");
const isDigit = std.ascii.isDigit;
const isAlphabetic = std.ascii.isAlphabetic;
const testing = std.testing;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TokenLiteral = @import("token.zig").TokenLiteral;

const Lexer = @This();

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
    var tok = Token{ .type = TokenType.Unknown, .literal = &[_]u8{self.char}, .location = null };

    self.skipWhiteSpace();

    switch (self.char) {
        0 => {
            tok.type = .Eof;
            tok.literal = "";
        },
        '=' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '=' => {
                    tok.type = .EqEq;
                    tok.literal = "==";
                    self.eatChar();
                },
                '>' => {
                    tok.type = .Arrow;
                    tok.literal = "=>";
                    self.eatChar();
                },
                else => {
                    tok.type = .Eq;
                    tok.literal = "=";
                },
            }
        },
        ':' => {
            const peeked = self.peek(0);
            if (peeked == '=') {
                tok.type = .DeclareAssign;
                tok.literal = ":=";
                self.eatChar();
            } else {
                tok.type = .Colon;
                tok.literal = ":";
            }
        },
        '+' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '=' => {
                    tok.type = .AddAssign;
                    tok.literal = "+=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Plus;
                    tok.literal = "+";
                },
            }
        },
        '-' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '=' => {
                    tok.type = .SubAssign;
                    tok.literal = "-=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Minus;
                    tok.literal = "-";
                },
            }
        },
        '!' => {
            const peeked = self.peek(0);

            switch (peeked) {
                '=' => {
                    tok.type = .NotEq;
                    tok.literal = "!=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Bang;
                    tok.literal = "!";
                },
            }
        },
        '*' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '=' => {
                    tok.type = .MulAssign;
                    tok.literal = "*=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Star;
                    tok.literal = "*";
                },
            }
        },
        '/' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '/' => {
                    tok.type = .LineComment;
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

                    tok.type = .BlockComment;
                    tok.literal = self.input[start_pos..end_pos];
                },
                '=' => {
                    tok.type = .DivAssign;
                    tok.literal = "/=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Slash;
                    tok.literal = "/";
                },
            }
        },
        '<' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '<' => {
                    tok.type = .LtLt;
                    tok.literal = "<<";
                    self.eatChar();

                    const peeked_2 = self.peek(0);
                    if (peeked_2 == '=') {
                        tok.type = .LtLtAssign;
                        tok.literal = "<<=";
                        self.eatChar();
                    }
                },
                else => {
                    tok.type = .Lt;
                    tok.literal = "<";
                },
            }
        },
        '>' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '>' => {
                    tok.type = .GtGt;
                    tok.literal = ">>";
                    self.eatChar();

                    const peeked_2 = self.peek(0);
                    if (peeked_2 == '=') {
                        tok.type = .GtGtAssign;
                        tok.literal = ">>=";
                        self.eatChar();
                    }
                },
                else => {
                    tok.type = .Gt;
                    tok.literal = ">";
                },
            }
        },
        ',' => {
            tok.type = .Comma;
            tok.literal = ",";
        },
        ';' => {
            tok.type = .Semi;
            tok.literal = ";";
        },
        '(' => {
            tok.type = .OpenParen;
            tok.literal = "(";
        },
        ')' => {
            tok.type = .CloseParen;
            tok.literal = ")";
        },
        '{' => {
            tok.type = .OpenBrace;
            tok.literal = "{";
        },
        '}' => {
            tok.type = .CloseBrace;
            tok.literal = "}";
        },
        '[' => {
            tok.type = .OpenBracket;
            tok.literal = "[";
        },
        ']' => {
            tok.type = .CloseBracket;
            tok.literal = "]";
        },
        '&' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '&' => {
                    tok.type = .AndAnd;
                    tok.literal = "&&";
                    self.eatChar();
                },
                '=' => {
                    tok.type = .BitwiseAssign;
                    tok.literal = "&=";
                    self.eatChar();
                },
                else => {
                    tok.type = .And;
                    tok.literal = "&";
                },
            }
        },
        '.' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '.' => {
                    tok.type = .DotDot;
                    tok.literal = "..";
                    self.eatChar();

                    const peeked_2 = self.peek(0);
                    if (peeked_2 == '=') {
                        tok.type = .DotDotEq;
                        tok.literal = "..=";
                        self.eatChar();
                    }
                },
                else => {
                    tok.type = .Dot;
                    tok.literal = ".";
                },
            }
        },
        '@' => {
            tok.type = .At;
            tok.literal = "@";
        },
        '#' => {
            tok.type = .Pound;
            tok.literal = "#";
        },
        '~' => {
            tok.type = .Tilde;
            tok.literal = "~";
        },
        '?' => {
            tok.type = .Question;
            tok.literal = "?";
        },
        '$' => {
            tok.type = .Dollar;
            tok.literal = "$";
        },
        '|' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '|' => {
                    tok.type = .OrOr;
                    tok.literal = "||";
                    self.eatChar();
                },
                '=' => {
                    tok.type = .OrAssign;
                    tok.literal = "|=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Or;
                    tok.literal = "|";
                },
            }
        },
        '\\' => {
            tok.type = .BackSlash;
            tok.literal = "\\";
        },
        '^' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '=' => {
                    tok.type = .CaretAssign;
                    tok.literal = "^=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Caret;
                    tok.literal = "^";
                },
            }
        },
        '%' => {
            const peeked = self.peek(0);
            switch (peeked) {
                '=' => {
                    tok.type = .ArithmeticAssign;
                    tok.literal = "%=";
                    self.eatChar();
                },
                else => {
                    tok.type = .Percent;
                    tok.literal = "%";
                },
            }
        },
        '"' => {
            tok.type = .{ .Literal = .Str };
            tok.literal = self.eatString();
        },
        else => {
            if (isAlphabetic(self.char)) {
                const ident = self.eatIdentifier();
                tok.literal = ident;
                tok.type = Token.lookupIdent(ident);
                return tok; // INFO: readIdentifier advances the next char, so we have to return here!
            } else if (isDigit(self.char)) {
                tok.type = .{ .Literal = TokenLiteral.Int }; // TODO: implement other token literal types!
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

    while (isAlphabetic(self.char)) {
        self.eatChar();
    }

    return self.input[position..self.position];
}

fn eatNumber(self: *Lexer) []const u8 {
    const position = self.position;

    while (isDigit(self.char)) {
        self.eatChar();
    }

    return self.input[position..self.position];
}

fn eatString(self: *Lexer) []const u8 {
    self.eatChar(); // remove the first '"'
    const position = self.position;

    while (true) {
        if (self.char == '"' or self.char == 0)
            break;
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

test "Next Token" {
    const input =
        // ------- Special -------
        \\xyz
        // ------- Literals -------
        \\10
        \\"foobar"
        \\"foo bar"
        // ------- Keywords -------
        \\mut
        \\return
        \\if
        \\else
        \\false
        \\true
        \\fn
        \\where
        \\pub
        \\enum
        \\struct
        \\impl
        \\switch
        \\for
        \\while
        \\break
        \\defer
        \\null
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
        .{ TokenType{ .Literal = .Int }, "10" },
        .{ TokenType{ .Literal = .Str }, "foobar" },
        .{ TokenType{ .Literal = .Str }, "foo bar" },
        .{ TokenType.Mut, "mut" },
        .{ TokenType.Return, "return" },
        .{ TokenType.If, "if" },
        .{ TokenType.Else, "else" },
        .{ TokenType.False, "false" },
        .{ TokenType.True, "true" },
        .{ TokenType.Fn, "fn" },
        .{ TokenType.Where, "where" },
        .{ TokenType.Pub, "pub" },
        .{ TokenType.Enum, "enum" },
        .{ TokenType.Struct, "struct" },
        .{ TokenType.Impl, "impl" },
        .{ TokenType.Switch, "switch" },
        .{ TokenType.For, "for" },
        .{ TokenType.While, "while" },
        .{ TokenType.Break, "break" },
        .{ TokenType.Defer, "defer" },
        .{ TokenType.Null, "null" },
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

    inline for (tests) |test_item| {
        const tok = lexer.nextToken();

        try testing.expectEqual(test_item[0], tok.type);
        try testing.expectEqualStrings(test_item[1], tok.literal);
    }
}
