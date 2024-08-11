const std = @import("std");
const fmt = std.fmt;
const testing = std.testing;

const ast = @import("ast.zig");
const Program = ast.Program;
const Statement = ast.Statement;
const DeclareAssignStatement = ast.DeclareAssignStatement;
const ReturnStatement = ast.ReturnStatement;
const ExpressionStatement = ast.ExpressionStatement;
const Expression = ast.Expression;
const IdentifierExpression = ast.IdentifierExpression;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

const prefix_parse_fn_map = std.StaticStringMap(TokenType).initComptime(.{
    // TODO: TokenType to prefixParseFn
    .{ "token", TokenType.Mut },
});

const infix_parse_fn_map = std.StaticStringMap(TokenType).initComptime(.{
    // TODO: TokenType to infixParseFunction
    .{ "mut", TokenType.Mut },
});

/// A recursive descent pratt parser
pub const Parser = struct {
    lexer: Lexer,
    prev_token: Token = Token{ .type = .Unknown, .literal = "" },
    cur_token: Token = Token{ .type = .Unknown, .literal = "" },
    peek_token: Token = Token{ .type = .Unknown, .literal = "" },
    errors: ArrayList([]const u8),

    pub fn init(alloc: std.mem.Allocator, lex: Lexer) Parser {
        var parser = Parser{ .lexer = lex, .errors = ArrayList([]const u8).init(alloc) };

        // read two tokens, so curToken and peekToken are both set
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    fn nextToken(self: *Parser) void {
        self.prev_token = self.cur_token;
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser, alloc: std.mem.Allocator) !Program {
        var program = Program.init(alloc);

        while (self.cur_token.type != .Eof) {
            const statement = try self.parseStatement(alloc);
            if (statement) |stmt| {
                try program.statements.append(stmt);
            }
            self.nextToken();
        }

        return program;
    }

    fn parseStatement(self: *Parser, alloc: std.mem.Allocator) !?Statement {
        switch (self.cur_token.type) {
            .DeclareAssign => return try self.parseDeclareAssignStatement(alloc),
            .Return => return try self.parseReturnStatement(),
            else => {
                if (self.peek_token.type != .Unknown and self.peek_token.type != .Eof) {
                    self.nextToken();
                    return try self.parseStatement(alloc);
                }
                return null;
            },
        }
    }

    fn parseDeclareAssignStatement(self: *Parser, alloc: std.mem.Allocator) !?Statement {
        const declare_assign_token = self.cur_token;
        const ident_expr = IdentifierExpression{ .token = self.prev_token, .ident = self.prev_token.literal };

        if (!self.expectPrevAndEat(.Ident)) {
            const err = try fmt.allocPrint(alloc, "Error parsing ':=' declare_assign. Expected identifier before declare_assign, but got: {s}\n", .{self.prev_token.literal});
            try self.errors.append(err);
            return null;
        }
        const statement = DeclareAssignStatement{ .token = declare_assign_token, .ident_expr = ident_expr, .expr = null }; // TODO: parse val
        while (!self.curTokenIs(TokenType.Semi)) {
            self.nextToken();
        }

        return Statement{ .declare_assign = statement };
    }

    fn parseReturnStatement(self: *Parser) !?Statement {
        const return_token = self.cur_token;
        self.nextToken();

        // TODO: were skipping the expressions until we encounter a semi colon;
        while (!self.curTokenIs(TokenType.Semi)) {
            self.nextToken();
        }

        const statement = ReturnStatement{ .token = return_token, .expr = null }; // TODO: parse val
        return Statement{ .return_ = statement };
    }

    // TODO
    fn registerPrefix(self: Parser, token_type: TokenType, parse_func: fn () void) void {
        _ = self;
        _ = token_type;
        _ = parse_func;
    }

    // TODO
    fn registerInfix(self: Parser, token_type: TokenType, parse_func: fn () void) void {
        _ = self;
        _ = token_type;
        _ = parse_func;
    }

    // fn prefixParseFn(self: Parser) Expression {
    //     _ = self;
    //     @compileError("TODO");
    // }

    // fn infixParseFn(self: Parser, left_expr: Expression) Expression {
    //     _ = self;
    //     _ = left_expr;
    //     @compileError("TODO");
    // }

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

    pub fn checkParserErrors(self: Parser) !void {
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

test "DeclareAssign Statement" {
    const input =
        \\x := 5;
    ;

    const lexer = Lexer.init(input);
    var parser = Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(test_allocator);
    defer program.deinit();
    try parser.checkParserErrors();

    try testing.expectEqual(1, program.statements.items.len);

    const tests = [_]Statement{
        Statement{
            // x := 5;
            .declare_assign = DeclareAssignStatement{
                .ident_expr = IdentifierExpression{ .ident = "x", .token = Token{ .type = .Ident, .literal = "x" } },
                .token = Token{ .type = .DeclareAssign, .literal = ":=" },
                .expr = null, // TODO
            },
        },
    };

    for (0.., tests) |i, expected| {
        const actual = program.statements.items[i];
        try testing.expectEqual(expected.declare_assign.token.type, actual.declare_assign.token.type);
        try testing.expectEqualStrings(expected.declare_assign.token.literal, actual.declare_assign.token.literal);
        try testing.expectEqualStrings(expected.declare_assign.ident_expr.ident, actual.declare_assign.ident_expr.ident);
        try testing.expectEqual(expected.declare_assign.ident_expr.token.type, actual.declare_assign.ident_expr.token.type);
        try testing.expectEqualStrings(expected.declare_assign.ident_expr.token.literal, actual.declare_assign.ident_expr.token.literal);
    }
}

test "Return Statement" {
    const input =
        \\return 5;
    ;

    const lexer = Lexer.init(input);
    var parser = Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(test_allocator);
    defer program.deinit();
    try parser.checkParserErrors();

    try testing.expectEqual(1, program.statements.items.len);

    const tests = [_]Statement{
        // return 5;
        Statement{
            .return_ = ReturnStatement{
                .token = Token{ .type = .Return, .literal = "return" },
                .expr = null, // TODO
            },
        },
    };

    for (0.., tests) |i, expected| {
        const actual = program.statements.items[i];
        try testing.expectEqual(expected.return_.token.type, actual.return_.token.type);
        try testing.expectEqualStrings(expected.return_.token.literal, actual.return_.token.literal);
        try testing.expectEqual(expected.return_.expr, actual.return_.expr);
    }
}

test "Expression Statement" {
    const input = "foobar;";

    const lexer = Lexer.init(input);
    var parser = Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(test_allocator);
    defer program.deinit();
    try parser.checkParserErrors();

    try testing.expectEqual(1, program.statements.items.len);

    const tests = [_]Statement{
        // foobar;
        Statement{
            .expression = ExpressionStatement{
                .token = Token{ .type = .Ident, .literal = "foobar" },
                .expr = null, // TODO
            },
        },
    };

    for (0.., tests) |i, expected| {
        const actual = program.statements.items[i];
        try testing.expectEqual(expected.expression.token.type, actual.expression.token.type);
        try testing.expectEqualStrings(expected.expression.token.literal, actual.expression.token.literal);
        try testing.expectEqual(expected.expression.expr, actual.expression.expr);
    }
}
