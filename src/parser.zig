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
const IntLiteralExpression = ast.IntLiteralExpression;
const PrefixExpression = ast.PrefixExpression;
const Lexer = @import("lexer.zig").Lexer;
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TokenLiteral = @import("token.zig").TokenLiteral;
const ArrayList = std.ArrayList;
const test_allocator = std.testing.allocator;
const assert = @import("std").debug.assert;

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
    const PrefixParseFunc = *const fn (parser: *Parser) anyerror!Expression;
    const InfixParseFunc = *const fn (parser: *Parser, expr: Expression) Expression;

    lexer: Lexer,
    prev_token: Token = .{ .type = .Unknown, .literal = "" },
    cur_token: Token = .{ .type = .Unknown, .literal = "" },
    peek_token: Token = .{ .type = .Unknown, .literal = "" },
    errors: ArrayList([]const u8),
    prefix_parse_fn_map: std.AutoHashMap(TokenType, PrefixParseFunc),
    infix_parse_fn_map: std.AutoHashMap(TokenType, InfixParseFunc),

    /// use this for allocations that are cleaned up in Parser.deinit anyways eg. appending to errors, and are not returned externally
    internal_alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, lex: Lexer) !Parser {
        var parser = Parser{
            .lexer = lex,
            .errors = ArrayList([]const u8).init(alloc),
            .prefix_parse_fn_map = std.AutoHashMap(TokenType, PrefixParseFunc).init(alloc),
            .infix_parse_fn_map = std.AutoHashMap(TokenType, InfixParseFunc).init(alloc),
            .internal_alloc = alloc,
        };

        // registerPrefix
        try parser.prefix_parse_fn_map.put(.Ident, parseIdentifier);
        try parser.prefix_parse_fn_map.put(.{ .Literal = .Int }, parseIntegerLiteral);
        try parser.prefix_parse_fn_map.put(.Bang, parsePrefixExpression);
        try parser.prefix_parse_fn_map.put(.Minus, parsePrefixExpression);

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
            .Ident => {
                if (self.peekTokenIs(.DeclareAssign)) {
                    self.nextToken();
                    return try self.parseDeclareAssignStatement();
                } else {
                    return try self.parseExpressionStatement();
                }
            },
            .Return => return try self.parseReturnStatement(),
            else => {
                return try self.parseExpressionStatement();
            },
        }
    }

    fn parseExpressionStatement(self: *Parser) !Statement {
        const statement = Statement{ .expression = .{
            .token = self.cur_token,
            .expr = try self.parseExpression(.Lowest),
        } };

        if (self.peekTokenIs(.Semi)) {
            self.nextToken();
        }

        return statement;
    }

    fn parseDeclareAssignStatement(self: *Parser) !?Statement {
        assert(self.cur_token.type == .DeclareAssign);

        const declare_assign_token = self.cur_token;
        const ident_expr = IdentifierExpression{ .token = self.prev_token, .value = self.prev_token.literal };

        if (!self.expectPrevAndEat(.Ident)) {
            const err = try fmt.allocPrint(self.internal_alloc, "Error parsing ':=' declare_assign. Expected identifier before declare_assign, but got: {s}\n", .{self.prev_token.literal});
            try self.errors.append(err);
            return null;
        }

        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        return Statement{
            .declare_assign = .{
                .token = declare_assign_token,
                .ident_expr = ident_expr,
                .expr = null,
            },
        };
    }

    fn parseReturnStatement(self: *Parser) !?Statement {
        assert(self.cur_token.type == .Return);

        const return_token = self.cur_token;
        self.nextToken();

        // TODO: were skipping the expressions until we encounter a semi colon;
        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        return Statement{ .return_ = .{ .token = return_token, .expr = null } };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) !?Expression {
        // TODO
        _ = precedence;

        const prefix = self.prefix_parse_fn_map.get(self.cur_token.type);

        if (prefix == null) {
            const err_msg = try std.fmt.allocPrint(self.internal_alloc, "no prefix parse function for {any} found.", .{self.cur_token.type});
            try self.errors.append(err_msg);
            return null;
        }

        const left_expr = try prefix.?(self);
        return left_expr;
    }

    fn parseIdentifier(self: *Parser) !Expression {
        assert(self.cur_token.type == .Ident);

        return Expression{ .ident_expr = .{ .token = self.cur_token, .value = self.cur_token.literal } };
    }

    fn parseIntegerLiteral(self: *Parser) !Expression {
        const value = std.fmt.parseInt(i32, self.cur_token.literal, 0) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.internal_alloc, "could not parse {s} as integer literal: {s}", .{ self.cur_token.literal, @errorName(err) });
            try self.errors.append(err_msg);
            return err;
        };

        return Expression{
            .int_literal_expr = .{
                .token = self.cur_token,
                .value = value,
            },
        };
    }

    fn parsePrefixExpression(self: *Parser) !Expression {
        const operator = self.cur_token.literal;
        const token = self.cur_token;

        self.nextToken();

        const right = try self.parseExpression(.Prefix);

        if (right == null) {
            const err_msg = try std.fmt.allocPrint(self.internal_alloc, "could not parse right for prefix expression: {s} at {any}", .{ operator, token });
            try self.errors.append(err_msg);
            return error.PrefixExpressionDoesNotExist;
        }

        return Expression{
            .prefix_expr = .{
                .operator = operator,
                .token = token,
                .right = &right.?,
            },
        };
    }
};

pub fn expectParsedEqStatements(input: []const u8, expected_statements: []const Statement) !void {
    const lexer = Lexer.init(input);
    var parser = try Parser.init(testing.allocator, lexer);
    defer parser.deinit();

    var program = try parser.parseProgram(testing.allocator);
    defer program.deinit();

    try parser.checkParserErrors();

    if (expected_statements.len != program.statements.items.len) {
        std.debug.print("\nMismatch in number of statements:\n", .{});
        std.debug.print("Expected {} statements, but got {}.\n\n", .{ expected_statements.len, program.statements.items.len });
        std.debug.print("Input: {s}\n\n", .{input});
        std.debug.print("Parsed statements:\n\n", .{});
        for (program.statements.items, 0..) |stmt, i| {
            std.debug.print("Statement {}:\n {}\n\n", .{ i, std.json.fmt(stmt, .{ .whitespace = .indent_2 }) });
        }
        return error.StatementCountMismatch;
    }

    for (expected_statements, 0..) |expected, i| {
        const actual = program.statements.items[i];
        expectStatementEq(expected, actual) catch |err| {
            std.debug.print("\nMismatch in statement {}:\n", .{i});
            std.debug.print("Expected: {}\n", .{expected});
            std.debug.print("Actual: {}\n", .{actual});
            return err;
        };
    }
}

fn expectStatementEq(expected: Statement, actual: Statement) !void {
    if (@as(std.meta.Tag(Statement), expected) != @as(std.meta.Tag(Statement), actual)) {
        std.debug.print("Statement type mismatch. Expected: {}, got: {}\n", .{ @as(std.meta.Tag(Statement), expected), @as(std.meta.Tag(Statement), actual) });
        return error.StatementTypeMismatch;
    }

    switch (expected) {
        .declare_assign => |declare_assign| {
            const actual_declare_assign = actual.declare_assign;

            try testing.expectEqual(declare_assign.token.type, actual_declare_assign.token.type);
            try testing.expectEqualStrings(declare_assign.token.literal, actual_declare_assign.token.literal);
            try expectIdentifierExpressionEq(declare_assign.ident_expr, actual_declare_assign.ident_expr);

            // TODO: dont test .expr until implemented
            // if (declare_assign.expr) |expr| {
            //     try testing.expectEqual(expr, actual_declare_assign.expr);
            //     try expectExpressionEq(expr, actual_declare_assign.expr.?);
            // }
        },
        .return_ => |return_| {
            const actual_return = actual.return_;

            try testing.expectEqual(return_.token.type, actual_return.token.type);
            try testing.expectEqualStrings(return_.token.literal, actual_return.token.literal);

            // TODO: dont test .expr until implemented
            // if (return_.expr) |expr| {
            //     try testing.expectEqual(expr, actual_return.expr);
            //     try expectExpressionEq(expr, actual_return.expr.?);
            // }
        },
        .expression => |expression| {
            const actual_expression = actual.expression;

            try testing.expectEqual(expression.token.type, actual_expression.token.type);
            try testing.expectEqualStrings(expression.token.literal, actual_expression.token.literal);

            // TODO: dont test .expr until implemented
            // if (expression.expr) |expr| {
            //     try testing.expectEqual(expr, actual_expression.expr);
            //     try expectExpressionEq(expr, actual_expression.expr.?);
            // }
        },
    }
}

fn expectExpressionEq(expected: Expression, actual: Expression) !void {
    if (@as(std.meta.Tag(Expression), expected) != @as(std.meta.Tag(Expression), actual)) {
        std.debug.print("Expression type mismatch. Expected: {}, got: {}\n", .{ @as(std.meta.Tag(Expression), expected), @as(std.meta.Tag(Expression), actual) });
        return error.ExpressionTypeMismatch;
    }

    switch (expected) {
        .ident_expr => |ident_expr| {
            try expectIdentifierExpressionEq(ident_expr, actual.ident_expr);
        },
        .int_literal_expr => |int_literal_expr| {
            try expectIntLiteralExpressionEq(int_literal_expr, actual.int_literal_expr);
        },
        .prefix_expr => |prefix_expr| {
            try expectPrefixExpressionEq(prefix_expr, actual.prefix_expr);
        },
    }
}

fn expectIdentifierExpressionEq(expected: IdentifierExpression, actual: IdentifierExpression) !void {
    try testing.expectEqual(expected.token.type, actual.token.type);
    try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
    try testing.expectEqualStrings(expected.value, actual.value);
}

fn expectIntLiteralExpressionEq(expected: IntLiteralExpression, actual: IntLiteralExpression) !void {
    try testing.expectEqual(expected.token.type, actual.token.type);
    try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
    try testing.expectEqual(expected.value, actual.value);
}

fn expectPrefixExpressionEq(expected: PrefixExpression, actual: PrefixExpression) anyerror!void {
    try testing.expectEqual(expected.token.type, actual.token.type);
    try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
    try testing.expectEqualStrings(expected.operator, actual.operator);
    try expectExpressionEq(expected.right.*, actual.right.*);
}

test "DeclareAssign Statement" {
    const input =
        \\x := 5;
    ;

    const tests = [_]Statement{
        .{
            // x := 5;
            .declare_assign = .{
                .ident_expr = .{
                    .value = "x",
                    .token = .{
                        .type = .Ident,
                        .literal = "x",
                    },
                },
                .token = .{
                    .type = .DeclareAssign,
                    .literal = ":=",
                },
                .expr = .{
                    .int_literal_expr = .{
                        .token = .{
                            .type = .{
                                .Literal = .Int,
                            },
                            .literal = "5",
                        },
                        .value = 5,
                    },
                },
            },
        },
    };

    try expectParsedEqStatements(input, &tests);
}

test "Return Statement" {
    const input =
        \\return 5;
    ;

    const tests = [_]Statement{
        // return 5;
        .{
            .return_ = .{
                .token = .{
                    .type = .Return,
                    .literal = "return",
                },
                .expr = .{
                    .int_literal_expr = .{
                        .token = .{
                            .type = .{
                                .Literal = .Int,
                            },
                            .literal = "5",
                        },
                        .value = 5,
                    },
                },
            },
        },
    };

    try expectParsedEqStatements(input, &tests);
}

test "Statement Expression" {
    const input = "foobar;";

    const tests = [_]Statement{
        // foobar;
        .{
            .expression = .{
                .token = .{
                    .type = .Ident,
                    .literal = "foobar",
                },
                .expr = null, // TODO
            },
        },
    };

    try expectParsedEqStatements(input, &tests);
}

test "Integer Literal Expression" {
    const input = "5;";

    const tests = [_]Statement{
        // foobar;
        .{
            .expression = .{
                .token = .{
                    .type = .{
                        .Literal = .Int,
                    },
                    .literal = "5",
                },
                .expr = .{
                    .int_literal_expr = .{
                        .token = .{ .type = .{
                            .Literal = .Int,
                        }, .literal = "5" },
                        .value = 5,
                    },
                },
            },
        },
    };

    try expectParsedEqStatements(input, &tests);
}

test "Prefix Expression" {
    const input =
        \\!5;
        \\-15;
    ;

    const tests = [_]Statement{
        // foobar;
        .{
            .expression = .{
                .token = .{
                    .type = .Bang,
                    .literal = "!",
                },
                .expr = .{
                    .prefix_expr = .{
                        .token = .{
                            .type = .Bang,
                            .literal = "!",
                        },
                        .operator = "!",
                        .right = &.{
                            .int_literal_expr = .{
                                .token = .{
                                    .type = .{
                                        .Literal = .Int,
                                    },
                                    .literal = "5",
                                },
                                .value = 5,
                            },
                        },
                    },
                },
            },
        },
        .{
            .expression = .{
                .token = .{
                    .type = .Minus,
                    .literal = "-",
                },
                .expr = .{
                    .prefix_expr = .{
                        .token = .{
                            .type = .Minus,
                            .literal = "-",
                        },
                        .operator = "-",
                        .right = &.{
                            .int_literal_expr = .{
                                .token = .{
                                    .type = .{
                                        .Literal = .Int,
                                    },
                                    .literal = "15",
                                },
                                .value = 55,
                            },
                        },
                    },
                },
            },
        },
    };

    try expectParsedEqStatements(input, &tests);
}
