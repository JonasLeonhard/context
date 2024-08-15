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
const TokenLiteral = @import("token.zig").TokenLiteral;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;

/// A recursive descent pratt parser
pub const Parser = struct {
    const Precedence = enum {
        Lowest,
        Equals, // ==
        LessGreater, // > or <
        Sum, // +
        Product, // *
        Prefix, // -X or !X
        Call, // myFunction(X)
    };
    const PrefixParseFunc = *const fn (parser: *Parser) Expression;
    const InfixParseFunc = *const fn (parser: *Parser, expr: Expression) Expression;

    lexer: Lexer,
    prev_token: Token = .{ .type = .Unknown, .literal = "" },
    cur_token: Token = .{ .type = .Unknown, .literal = "" },
    peek_token: Token = .{ .type = .Unknown, .literal = "" },
    errors: ArrayList([]const u8),
    prefix_parse_fn_map: std.AutoHashMap(TokenType, PrefixParseFunc),
    infix_parse_fn_map: std.AutoHashMap(TokenType, InfixParseFunc),

    pub fn init(alloc: std.mem.Allocator, lex: Lexer) !Parser {
        var parser = Parser{
            .lexer = lex,
            .errors = ArrayList([]const u8).init(alloc),
            .prefix_parse_fn_map = std.AutoHashMap(TokenType, PrefixParseFunc).init(alloc),
            .infix_parse_fn_map = std.AutoHashMap(TokenType, InfixParseFunc).init(alloc),
        };

        // registerPrefix
        try parser.prefix_parse_fn_map.put(.Ident, parseIdentifier);

        // read two tokens, so curToken and peekToken are both set
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
        self.prefix_parse_fn_map.deinit();
        self.infix_parse_fn_map.deinit();
    }

    fn nextToken(self: *Parser) void {
        self.prev_token = self.cur_token;
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
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
            .Ident => {
                if (self.peekTokenIs(.DeclareAssign)) {
                    self.nextToken();
                    return try self.parseDeclareAssignStatement(alloc);
                } else {
                    return self.parseExpressionStatement();
                }
            },
            .Return => return try self.parseReturnStatement(),
            else => {
                return self.parseExpressionStatement();
            },
        }
    }

    fn parseExpressionStatement(self: *Parser) Statement {
        const statement = .{ .expression = .{
            .token = self.cur_token,
            .expr = self.parseExpression(.Lowest),
        } };

        if (self.peekTokenIs(.Semi)) {
            self.nextToken();
        }

        return statement;
    }

    fn parseDeclareAssignStatement(self: *Parser, alloc: std.mem.Allocator) !?Statement {
        const declare_assign_token = self.cur_token;
        const ident_expr = .{ .token = self.prev_token, .ident = self.prev_token.literal };

        if (!self.expectPrevAndEat(.Ident)) {
            const err = try fmt.allocPrint(alloc, "Error parsing ':=' declare_assign. Expected identifier before declare_assign, but got: {s}\n", .{self.prev_token.literal});
            try self.errors.append(err);
            return null;
        }

        const statement = .{ .token = declare_assign_token, .ident_expr = ident_expr, .expr = null }; // TODO: parse val
        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        return .{ .declare_assign = statement };
    }

    fn parseReturnStatement(self: *Parser) !?Statement {
        const return_token = self.cur_token;
        self.nextToken();

        // TODO: were skipping the expressions until we encounter a semi colon;
        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        const statement = .{ .token = return_token, .expr = null }; // TODO: parse val
        return .{ .return_ = statement };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ?Expression {
        // TODO
        _ = precedence;

        const prefix = self.prefix_parse_fn_map.get(self.cur_token.type);

        if (prefix == null) {
            return null;
        }

        const left_expr = prefix.?(self);
        return left_expr;
    }

    fn parseIdentifier(self: *Parser) Expression {
        return .{ .ident_expr = .{ .token = self.cur_token, .ident = self.cur_token.literal } };
    }
};

test "DeclareAssign Statement" {
    const input =
        \\x := 5;
    ;

    const lexer = Lexer.init(input);
    var parser = try Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(test_allocator);
    defer program.deinit();
    try parser.checkParserErrors();

    const program_str = try program.toString(test_allocator);
    defer test_allocator.free(program_str);

    try testing.expectEqual(1, program.statements.items.len);

    const tests = [_]Statement{
        .{
            // x := 5;
            .declare_assign = .{
                .ident_expr = .{ .ident = "x", .token = .{ .type = .Ident, .literal = "x" } },
                .token = .{ .type = .DeclareAssign, .literal = ":=" },
                .expr = .{ .ident_expr = .{ .token = .{ .type = .{ .Literal = .Int }, .literal = "5" }, .ident = "5" } },
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
        // TODO: test expr?
    }
}

test "Return Statement" {
    const input =
        \\return 5;
    ;

    const lexer = Lexer.init(input);
    var parser = try Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(test_allocator);
    defer program.deinit();
    try parser.checkParserErrors();

    try testing.expectEqual(1, program.statements.items.len);

    const tests = [_]Statement{
        // return 5;
        .{
            .return_ = .{
                .token = .{ .type = .Return, .literal = "return" },
                .expr = .{ .ident_expr = .{ .token = .{ .type = .{ .Literal = .Int }, .literal = "5" }, .ident = "5" } },
            },
        },
    };

    for (0.., tests) |i, expected| {
        const actual = program.statements.items[i];
        try testing.expectEqual(expected.return_.token.type, actual.return_.token.type);
        try testing.expectEqualStrings(expected.return_.token.literal, actual.return_.token.literal);
        // TODO test expr?
    }
}

test "Expression Statement" {
    const input = "foobar;";

    const lexer = Lexer.init(input);
    var parser = try Parser.init(test_allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(test_allocator);
    defer program.deinit();
    try parser.checkParserErrors();

    try testing.expectEqual(1, program.statements.items.len);

    const tests = [_]Statement{
        // foobar;
        .{
            .expression = .{
                .token = .{ .type = .Ident, .literal = "foobar" },
                .expr = null, // TODO
            },
        },
    };

    for (0.., tests) |i, expected| {
        const actual = program.statements.items[i];
        try testing.expectEqual(expected.expression.token.type, actual.expression.token.type);
        try testing.expectEqualStrings(expected.expression.token.literal, actual.expression.token.literal);
        // TODO: parse expr?
        // try testing.expectEqual(expected.expression.expr, actual.expression.expr);
    }
}
