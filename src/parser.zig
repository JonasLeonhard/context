const std = @import("std");
const fmt = std.fmt;
const testing = std.testing;

const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const DeclareAssignStatement = ast.DeclareAssignStatement;
const IdentifierExpression = ast.IdentifierExpression;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    lexer: Lexer,
    prev_token: Token = Token{ .type = .Unknown, .literal = "" },
    cur_token: Token = Token{ .type = .Unknown, .literal = "" },
    peek_token: Token = Token{ .type = .Unknown, .literal = "" },
    errors: ArrayList([]const u8),

    fn init(allocator: std.mem.Allocator, lex: Lexer) Parser {
        var parser = Parser{ .allocator = allocator, .lexer = lex, .errors = ArrayList([]const u8).init(allocator) };

        // read two tokens, so curToken and peekToken are both set
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    fn nextToken(self: *Parser) void {
        self.prev_token = self.cur_token;
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser) !Program {
        var program = Program.init(self.allocator);

        while (self.cur_token.type != .Eof) {
            const statement = try self.parseStatement();
            if (statement) |stmt| {
                try program.statements.append(stmt);
            }
            self.nextToken();
        }

        return program;
    }

    fn parseStatement(self: *Parser) !?Statement {
        switch (self.cur_token.type) {
            .DeclareAssign => return try self.parseDeclareAssignStatement(),
            else => {
                if (self.peek_token.type != .Unknown or self.peek_token.type != .Eof) {
                    self.nextToken();
                    return try self.parseStatement();
                }
                return null;
            },
        }
    }

    fn parseDeclareAssignStatement(self: *Parser) !?Statement {
        const ident_expr = IdentifierExpression{ .token = self.prev_token, .value = self.prev_token.literal };
        if (!self.expectPrevAndEat(.Ident)) {
            const err = try fmt.allocPrint(self.allocator, "Error parsing ':=' declare_assign. Expected identifier before declare_assign, but got: {s}\n", .{self.prev_token.literal});
            try self.errors.append(err);
            return null;
        }
        const statement = DeclareAssignStatement{ .token = self.prev_token, .name = ident_expr, .value = null };

        while (!self.curTokenIs(TokenType.Semi)) {
            self.nextToken();
        }

        return Statement{ .declare_assign = statement };
    }

    fn curTokenIs(self: Parser, token_type: TokenType) bool {
        return self.cur_token.type.compareEq(token_type);
    }

    fn peekTokenIs(self: Parser, token_type: TokenType) bool {
        return self.peek_token.type.compareEq(token_type);
    }

    fn prevTokenIs(self: Parser, token_type: TokenType) bool {
        return self.prev_token.type.compareEq(token_type);
    }

    fn expectPeekAndEat(self: *Parser, token_type: TokenType) bool {
        if (self.peekTokenIs(token_type)) {
            self.nextToken();
            return true;
        }

        return false;
    }

    fn expectPrevAndEat(self: *Parser, token_type: TokenType) bool {
        if (self.prevTokenIs(token_type)) {
            self.nextToken();
            return true;
        }
        return false;
    }

    fn checkParserErrors(self: Parser) !void {
        if (self.errors.items.len == 0) {
            return;
        }

        std.debug.print("parser has {d} errors\n", .{self.errors.items.len});
        for (self.errors.items) |err| {
            std.debug.print("parser error: {s}\n", .{err});
        }

        return error.ParserError;
    }
};

fn validDeclareAssignStatement(statement: Statement, name: []const u8) !bool {
    switch (statement) {
        .declare_assign => |declareAssign| {
            try testing.expectEqualStrings(declareAssign.tokenLiteral(), ":=");
            try testing.expectEqualStrings(declareAssign.name.value, name);
            try testing.expectEqualStrings(declareAssign.name.tokenLiteral(), name);
            return true;
        },
        else => return false,
    }
}

test "DeclareAssign statements" {
    const input =
        \\x := 5;
        \\y := 10;
        \\foobar := 838383;
    ;

    const lexer = Lexer.init(input);
    var parser = Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram();
    defer program.deinit();
    try parser.checkParserErrors();

    std.debug.print("\nDeclareAssignStatements: {}\n\n", .{program.statements});

    try testing.expectEqual(program.statements.items.len, 3);

    const tests = .{ .{ .expected_identifier = "x" }, .{ .expected_identifier = "y" }, .{ .expected_identifier = "foobar" } };

    inline for (0.., tests) |i, test_item| {
        const statement = program.statements.items[i];
        const is_valid = try validDeclareAssignStatement(statement, test_item.expected_identifier);
        try testing.expect(is_valid);
    }
}
