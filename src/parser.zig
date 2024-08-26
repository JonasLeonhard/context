const std = @import("std");
const fmt = std.fmt;
const testing = std.testing;

const ast = @import("ast.zig");

const Lexer = @import("lexer.zig").Lexer;

const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const TokenLiteral = @import("token.zig").TokenLiteral;

const ArrayList = std.ArrayList;
const assert = std.debug.assert;
const ArenaAllocator = std.heap.ArenaAllocator;

/// A recursive descent pratt parser
pub const Parser = struct {
    const Precedence = enum {
        // Sorted by precedence:
        Lowest,
        Equals, // ==
        LessGreater, // > or <
        Sum, // +
        Product, // *
        Prefix, // -X or !X
        Call, // myFunction(X)

        pub fn fromTokenType(tok: TokenType) @This() {
            return switch (tok) {
                .EqEq => .Equals,
                .NotEq => .Equals,
                .Lt => .LessGreater,
                .Gt => .LessGreater,
                .Plus => .Sum,
                .Minus => .Sum,
                .Slash => .Product,
                .Star => .Product,
                else => .Lowest,
            };
        }
    };
    const PrefixParseFunc = *const fn (parser: *Parser, ast_tree: *ast.Tree) anyerror!ast.NodeIndex; // TODO: change anyerror to actual errors...
    const InfixParseFunc = *const fn (parser: *Parser, ast_tree: *ast.Tree, left: ast.NodeIndex) anyerror!ast.NodeIndex;

    lexer: Lexer,
    prev_token: Token = .{ .type = .Unknown, .literal = "", .location = null },
    cur_token: Token = .{ .type = .Unknown, .literal = "", .location = null },
    peek_token: Token = .{ .type = .Unknown, .literal = "", .location = null },
    errors: ArrayList([]const u8),
    prefix_parse_fn_map: std.AutoHashMap(TokenType, PrefixParseFunc),
    infix_parse_fn_map: std.AutoHashMap(TokenType, InfixParseFunc),

    /// use this for allocations that are cleaned up in Parser.deinit anyways eg. appending to errors, and are not returned externally
    arena: ArenaAllocator,

    pub fn init(alloc: std.mem.Allocator, lex: Lexer) !Parser {
        var arena = ArenaAllocator.init(alloc);
        errdefer arena.deinit();

        const errors = ArrayList([]const u8).init(alloc);
        var prefix_parse_fn_map = std.AutoHashMap(TokenType, PrefixParseFunc).init(alloc); // TODO: can we have a filled map initially or use a different approach?
        var infix_parse_fn_map = std.AutoHashMap(TokenType, InfixParseFunc).init(alloc);

        // registerPrefix
        try prefix_parse_fn_map.put(.Ident, parseIdentifier);
        try prefix_parse_fn_map.put(.{ .Literal = .Int }, parseIntegerLiteral);
        try prefix_parse_fn_map.put(.Bang, parsePrefixExpression);
        try prefix_parse_fn_map.put(.Minus, parsePrefixExpression);
        try prefix_parse_fn_map.put(.True, parseBooleanLiteral);
        try prefix_parse_fn_map.put(.False, parseBooleanLiteral);
        try prefix_parse_fn_map.put(.Null, parseNullLiteral);
        try prefix_parse_fn_map.put(.OpenParen, parseGroupedExpression);
        try prefix_parse_fn_map.put(.If, parseIfExpression);

        // registerInfix
        try infix_parse_fn_map.put(.Plus, parseInfixExpression);
        try infix_parse_fn_map.put(.Minus, parseInfixExpression);
        try infix_parse_fn_map.put(.Slash, parseInfixExpression);
        try infix_parse_fn_map.put(.Star, parseInfixExpression);
        try infix_parse_fn_map.put(.EqEq, parseInfixExpression);
        try infix_parse_fn_map.put(.NotEq, parseInfixExpression);
        try infix_parse_fn_map.put(.Lt, parseInfixExpression);
        try infix_parse_fn_map.put(.Gt, parseInfixExpression);

        var parser = Parser{
            .lexer = lex,
            .errors = errors,
            .prefix_parse_fn_map = prefix_parse_fn_map,
            .infix_parse_fn_map = infix_parse_fn_map,
            .arena = arena,
        };

        // read two tokens, so curToken and peekToken are both set
        parser.nextToken();
        parser.nextToken();

        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
        self.prefix_parse_fn_map.deinit();
        self.infix_parse_fn_map.deinit();
        self.arena.deinit();
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

    fn peekPrecedence(self: Parser) Precedence {
        return Precedence.fromTokenType(self.peek_token.type);
    }

    fn curPrecedence(self: Parser) Precedence {
        return Precedence.fromTokenType(self.cur_token.type);
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

    pub fn parseTree(self: *Parser, alloc: std.mem.Allocator) !ast.Tree {
        var ast_tree = ast.Tree.init(alloc);
        errdefer ast_tree.deinit();

        var statements = std.ArrayList(ast.NodeIndex).init(ast_tree.arena.allocator());

        while (self.cur_token.type != .Eof) {
            if (try self.parseStatement(&ast_tree)) |stmt| {
                try statements.append(stmt);
            }
            self.nextToken();
        }

        _ = try ast_tree.addNode(.{
            .root = .{
                .statements = try statements.toOwnedSlice(),
            },
        });

        return ast_tree;
    }

    fn parseStatement(self: *Parser, ast_tree: *ast.Tree) !?ast.NodeIndex {
        switch (self.cur_token.type) {
            .Ident => {
                if (self.peekTokenIs(.DeclareAssign)) {
                    self.nextToken();
                    return try self.parseDeclareAssignStatement(ast_tree);
                } else {
                    return try self.parseExpressionStatement(ast_tree);
                }
            },
            .Return => return try self.parseReturnStatement(ast_tree),
            else => {
                return try self.parseExpressionStatement(ast_tree);
            },
        }
    }

    fn parseExpressionStatement(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const statement = ast_tree.addNode(.{
            .expression = .{
                .token = self.cur_token,
                .expr = try self.parseExpression(ast_tree, .Lowest),
            },
        });

        if (self.peekTokenIs(.Semi)) {
            self.nextToken();
        }

        return statement;
    }

    fn parseDeclareAssignStatement(self: *Parser, ast_tree: *ast.Tree) !?ast.NodeIndex {
        assert(self.cur_token.type == .DeclareAssign);

        const declare_assign_token = self.cur_token;
        const ident = try ast_tree.addNode(.{ .ident = .{
            .token = self.prev_token,
            .value = self.prev_token.literal,
        } });

        if (!self.expectPrevAndEat(.Ident)) {
            const err = try fmt.allocPrint(self.arena.allocator(), "Error parsing ':=' declare_assign. Expected identifier before declare_assign, but got: {s}\n", .{self.prev_token.literal});
            try self.errors.append(err);
            return null;
        }
        const expr = try self.parseExpression(ast_tree, .Lowest);

        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        return try ast_tree.addNode(.{
            .declare_assign = .{
                .token = declare_assign_token,
                .ident = ident,
                .expr = expr,
            },
        });
    }

    fn parseReturnStatement(self: *Parser, ast_tree: *ast.Tree) !?ast.NodeIndex {
        assert(self.cur_token.type == .Return);

        const return_token = self.cur_token;
        self.nextToken();

        // TODO: were skipping the expressions until we encounter a semi colon;
        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        return try ast_tree.addNode(.{
            .return_ = .{
                .token = return_token,
                .expr = null, // TODO!
            },
        });
    }

    fn parseExpression(self: *Parser, ast_tree: *ast.Tree, precedence: Precedence) !ast.NodeIndex {
        const prefix = self.prefix_parse_fn_map.get(self.cur_token.type);

        if (prefix == null) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "no prefix parse function for {any} found.", .{self.cur_token.type});
            try self.errors.append(err_msg);
            return error.PrefixExpressionDoesNotExist;
        }

        var left_expr = try prefix.?(self, ast_tree);

        while (!self.peekTokenIs(.Semi) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix = self.infix_parse_fn_map.get(self.peek_token.type);
            if (infix == null) {
                return left_expr;
            }

            self.nextToken();

            left_expr = try infix.?(self, ast_tree, left_expr);
        }
        return left_expr;
    }

    fn parseIdentifier(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        assert(self.cur_token.type == .Ident);

        return ast_tree.addNode(.{
            .ident = .{
                .token = self.cur_token,
                .value = self.cur_token.literal,
            },
        });
    }

    fn parseIntegerLiteral(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const value = std.fmt.parseInt(i32, self.cur_token.literal, 0) catch |err| {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "could not parse {s} as integer literal: {s}", .{ self.cur_token.literal, @errorName(err) });
            try self.errors.append(err_msg);
            return err;
        };

        return ast_tree.addNode(.{
            .literal = .{
                .token = self.cur_token,
                .value = .{
                    .int = value,
                },
            },
        });
    }

    fn parseBooleanLiteral(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        assert(self.cur_token.type == .False or self.cur_token.type == .True);

        return ast_tree.addNode(.{
            .literal = .{
                .token = self.cur_token,
                .value = .{
                    .boolean = self.curTokenIs(.True),
                },
            },
        });
    }

    fn parseNullLiteral(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        assert(self.cur_token.type == .Null);

        return ast_tree.addNode(.{
            .literal = .{
                .token = self.cur_token,
                .value = .null,
            },
        });
    }

    fn parseGroupedExpression(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const start_token = self.cur_token;

        self.nextToken();

        const expr = try self.parseExpression(ast_tree, .Lowest);

        if (!self.expectPeekAndEat(.CloseParen)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any CloseParen for grouped expression starting at: {any}.", .{start_token.type});
            try self.errors.append(err_msg);
            return error.GroupedExpressionNotClosed;
        }

        return expr;
    }

    fn parseIfExpression(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const cur_token = self.cur_token;

        if (!self.expectPeekAndEat(.OpenParen)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any OpenParen after if expression, found: {any}.", .{cur_token.type});
            try self.errors.append(err_msg);
            return error.ParseIfExpressionNotOpened;
        }

        self.nextToken();

        const condition = try self.parseExpression(ast_tree, .Lowest);

        if (!self.expectPeekAndEat(.CloseParen)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any CloseParen after if expression, found: {any}.", .{self.cur_token.type});
            try self.errors.append(err_msg);
            return error.ParseIfExpressionNotClosed;
        }

        if (!self.expectPeekAndEat(.OpenBrace)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any OpenBrace after if expression, found: {any}.", .{self.cur_token.type});
            try self.errors.append(err_msg);
            return error.ParseIfExpressionNoBlockOpened;
        }

        const consequence = try self.parseBlockStatement(ast_tree);
        var alternative: ?ast.NodeIndex = null;

        if (self.peekTokenIs(.Else)) {
            self.nextToken();

            if (!self.expectPeekAndEat(.OpenBrace)) {
                const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any OpenBrace after else expression, found: {any}", .{self.cur_token});
                try self.errors.append(err_msg);
                return error.ParseIfExpressionNoBlockOpened;
            }
            alternative = try self.parseBlockStatement(ast_tree);
        }

        return try ast_tree.addNode(.{
            .if_ = .{
                .token = cur_token,
                .condition = condition,
                .consequence = consequence,
                .alternative = alternative,
            },
        });
    }

    fn parseBlockStatement(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const start_token = self.cur_token;

        self.nextToken();

        var statements = std.ArrayList(ast.NodeIndex).init(ast_tree.arena.allocator());

        while (!self.curTokenIs(.CloseBrace) and !self.curTokenIs(.Eof)) {
            const statement = try self.parseStatement(ast_tree);

            if (statement) |stmt| {
                try statements.append(stmt);
            }
            self.nextToken();
        }

        return ast_tree.addNode(.{
            .block = .{
                .token = start_token,
                .statements = try statements.toOwnedSlice(),
            },
        });
    }

    fn parsePrefixExpression(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const operator = self.cur_token.literal;
        const token = self.cur_token;

        self.nextToken();

        const right = try self.parseExpression(ast_tree, .Prefix);

        return ast_tree.addNode(.{
            .prefix = .{
                .operator = operator,
                .token = token,
                .right = right,
            },
        });
    }

    fn parseInfixExpression(self: *Parser, ast_tree: *ast.Tree, left: ast.NodeIndex) !ast.NodeIndex {
        const token = self.cur_token;
        const operator = self.cur_token.literal;

        const precedence = self.curPrecedence();
        self.nextToken();

        const parsed_right = try self.parseExpression(ast_tree, precedence);

        return ast_tree.addNode(.{
            .infix = .{
                .token = token,
                .operator = operator,
                .left = left,
                .right = parsed_right,
            },
        });
    }
};

// ------------- Testing ---------------
fn parseTree(alloc: std.mem.Allocator, input: []const u8) !ast.Tree {
    const lexer = Lexer.init(input);
    var parser = try Parser.init(alloc, lexer);
    defer parser.deinit();

    const ast_tree = parser.parseTree(alloc) catch |err| {
        try parser.checkParserErrors();
        return err;
    };

    return ast_tree;
}

test "DeclareAssign Statement" {
    const input =
        \\x := 5;
    ;

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const literal_5 = try test_tree.addNode(
        .{
            .literal = .{
                .token = Token{
                    .type = .{
                        .Literal = .Int,
                    },
                    .literal = "5",
                    .location = null,
                },
                .value = .{
                    .int = 5,
                },
            },
        },
    );

    const ident_x = try test_tree.addNode(.{
        .ident = .{
            .token = Token{
                .type = .Ident,
                .literal = "x",
                .location = null,
            },
            .value = "x",
        },
    });

    const declare_assign = try test_tree.addNode(.{
        .declare_assign = .{
            .ident = ident_x,
            .expr = literal_5, // TODO: literal_5
            .token = Token{
                .type = .Plus,
                .literal = "+",
                .location = null,
            },
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{ .statements = &.{declare_assign} },
    });

    const tree_string = try ast_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "Return Statement" {
    const input =
        \\return 5;
    ;

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    // const literal_5 = try test_tree.addNode(.{
    //     .literal = .{
    //         .token = Token{
    //             .type = .{ .Literal = .Int },
    //             .literal = "5",
    //             .location = null, // TODO
    //         },
    //         .value = .{ .int = 5 },
    //     },
    // });

    const return_stmt = try test_tree.addNode(.{
        .return_ = .{
            .token = Token{
                .type = .Return,
                .location = null,
                .literal = "return",
            },
            .expr = null, // TODO: literal_5
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{return_stmt},
        },
    });

    const tree_string = try ast_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "Statement Expression" {
    const input = "foobar;";

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const foobar_ident = try test_tree.addNode(.{
        .ident = .{
            .token = Token{
                .type = .Ident,
                .literal = "foobar",
                .location = null, // TODO
            },
            .value = "foobar",
        },
    });

    const expression_statement = try test_tree.addNode(.{
        .expression = .{
            .token = .{
                .type = .Ident,
                .literal = "foobar",
                .location = null,
            },
            .expr = foobar_ident, // TODO
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{expression_statement},
        },
    });

    const tree_string = try ast_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "Integer Literal Expression" {
    const input = "5;";

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const foobar_ident = try test_tree.addNode(.{
        .literal = .{
            .token = .{
                .type = .{ .Literal = .Int },
                .literal = "5",
                .location = null,
            },
            .value = .{ .int = 5 },
        },
    });

    const expression_statement = try test_tree.addNode(.{
        .expression = .{
            .token = .{
                .type = .{ .Literal = .Int },
                .literal = "5",
                .location = null,
            },
            .expr = foobar_ident,
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{expression_statement},
        },
    });

    const tree_string = try ast_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "Prefix Expression" {
    const input =
        \\!5;
        \\-5;
    ;

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const literal_5 = try test_tree.addNode(.{
        .literal = .{
            .token = Token{
                .type = .{ .Literal = .Int },
                .literal = "5",
                .location = null, // TODO
            },
            .value = .{ .int = 5 },
        },
    });
    const prefix_expr = try test_tree.addNode(.{
        .prefix = .{
            .token = .{ .literal = "!", .type = .Bang, .location = null },
            .operator = "!",
            .right = literal_5,
        },
    });
    const prefix_expr2 = try test_tree.addNode(.{
        .prefix = .{
            .token = .{ .literal = "-", .type = .Minus, .location = null },
            .operator = "-",
            .right = literal_5,
        },
    });

    const expression_statement = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "!", .type = .Bang, .location = null },
            .expr = prefix_expr,
        },
    });

    const expression_statement2 = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "-", .type = .Minus, .location = null },
            .expr = prefix_expr2,
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{ expression_statement, expression_statement2 },
        },
    });

    const tree_string = try ast_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "Infix Expression" {
    const input =
        \\5 + 5;
        \\5 - 5;
        \\5 * 5;
        \\5 / 5;
        \\5 > 5;
        \\5 < 5;
        \\5 == 5;
        \\5 != 5;
        \\true == true;
        \\false == false;
        \\true != false;
    ;

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const literal_5 = try test_tree.addNode(.{
        .literal = .{
            .token = .{
                .type = .{ .Literal = .Int },
                .literal = "5",
                .location = null, // TODO
            },
            .value = .{ .int = 5 },
        },
    });

    const literal_true = try test_tree.addNode(.{
        .literal = .{
            .token = .{
                .type = .True,
                .literal = "true",
                .location = null, // TODO
            },
            .value = .{ .boolean = true },
        },
    });

    const literal_false = try test_tree.addNode(.{
        .literal = .{
            .token = .{
                .type = .False,
                .literal = "false",
                .location = null, // TODO
            },
            .value = .{ .boolean = false },
        },
    });

    const infixes = .{
        .{ .literal = "+", .type = .Plus, .location = null },
        .{ .literal = "-", .type = .Minus, .location = null },
        .{ .literal = "*", .type = .Star, .location = null },
        .{ .literal = "/", .type = .Slash, .location = null },
        .{ .literal = ">", .type = .Gt, .location = null },
        .{ .literal = "<", .type = .Lt, .location = null },
        .{ .literal = "==", .type = .EqEq, .location = null },
        .{ .literal = "!=", .type = .NotEq, .location = null },
    };

    const infix_expr_plus = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[0],
            .operator = "+",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_plus = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "+", .type = .Plus, .location = null },
            .expr = infix_expr_plus,
        },
    });

    const infix_expr_minus = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[1],
            .operator = "-",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_minus = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "-", .type = .Minus, .location = null },
            .expr = infix_expr_minus,
        },
    });

    const infix_expr_star = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[2],
            .operator = "*",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_star = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "*", .type = .Star, .location = null },
            .expr = infix_expr_star,
        },
    });

    const infix_expr_slash = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[3],
            .operator = "/",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_slash = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "/", .type = .Slash, .location = null },
            .expr = infix_expr_slash,
        },
    });

    const infix_expr_lt = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[4],
            .operator = "<",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_lt = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "<", .type = .Lt, .location = null },
            .expr = infix_expr_lt,
        },
    });

    const infix_expr_gt = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[5],
            .operator = ">",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_gt = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = ">", .type = .Gt, .location = null },
            .expr = infix_expr_gt,
        },
    });

    const infix_expr_eqeq = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[6],
            .operator = "==",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_eqeq = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "==", .type = .EqEq, .location = null },
            .expr = infix_expr_eqeq,
        },
    });

    const infix_expr_noteq = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[7],
            .operator = "!=",
            .right = literal_5,
            .left = literal_5,
        },
    });

    const expression_statement_noteq = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "!=", .type = .NotEq, .location = null },
            .expr = infix_expr_noteq,
        },
    });

    const infix_expr_true = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[6],
            .operator = "==",
            .right = literal_true,
            .left = literal_true,
        },
    });
    const expression_statement_true = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "==", .type = .EqEq, .location = null },
            .expr = infix_expr_true,
        },
    });

    const infix_expr_false = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[6],
            .operator = "==",
            .right = literal_false,
            .left = literal_false,
        },
    });
    const expression_statement_false = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "==", .type = .EqEq, .location = null },
            .expr = infix_expr_false,
        },
    });

    const infix_expr_truefalse = try test_tree.addNode(.{
        .infix = .{
            .token = infixes[7],
            .operator = "!=",
            .right = literal_false,
            .left = literal_true,
        },
    });
    const expression_statement_truefalse = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .literal = "!=", .type = .NotEq, .location = null },
            .expr = infix_expr_truefalse,
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{
                expression_statement_plus,
                expression_statement_minus,
                expression_statement_star,
                expression_statement_slash,
                expression_statement_gt,
                expression_statement_lt,
                expression_statement_eqeq,
                expression_statement_noteq,
                expression_statement_true,
                expression_statement_false,
                expression_statement_truefalse,
            },
        },
    });

    const tree_string = try ast_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "Operator Precedence" {
    const tests = .{
        .{
            "-a * b", // ((-a) * b)
            \\root
            \\  expression
            \\    infix
            \\      operator: *
            \\      prefix
            \\        op: -
            \\        ident
            \\          ident: a
            \\      ident
            \\        ident: b
            \\
        },
        .{
            "!-a", // (!(-a))
            \\root
            \\  expression
            \\    prefix
            \\      op: !
            \\      prefix
            \\        op: -
            \\        ident
            \\          ident: a
            \\
        },
        .{
            "a + b + c", // ((a + b) + c)
            \\root
            \\  expression
            \\    infix
            \\      operator: +
            \\      infix
            \\        operator: +
            \\        ident
            \\          ident: a
            \\        ident
            \\          ident: b
            \\      ident
            \\        ident: c
            \\
        },
        .{
            "a * b * c", // ((a * b) * c)
            \\root
            \\  expression
            \\    infix
            \\      operator: *
            \\      infix
            \\        operator: *
            \\        ident
            \\          ident: a
            \\        ident
            \\          ident: b
            \\      ident
            \\        ident: c
            \\
        },
        .{
            "a * b / c", // ((a * b) / c)
            \\root
            \\  expression
            \\    infix
            \\      operator: /
            \\      infix
            \\        operator: *
            \\        ident
            \\          ident: a
            \\        ident
            \\          ident: b
            \\      ident
            \\        ident: c
            \\
        },
        .{
            "a + b / c", // (a + (b / c))
            \\root
            \\  expression
            \\    infix
            \\      operator: +
            \\      ident
            \\        ident: a
            \\      infix
            \\        operator: /
            \\        ident
            \\          ident: b
            \\        ident
            \\          ident: c
            \\
        },
        .{
            "a + b * c + d / e - f", // (((a + (b * c)) + (d / e)) - f)
            \\root
            \\  expression
            \\    infix
            \\      operator: -
            \\      infix
            \\        operator: +
            \\        infix
            \\          operator: +
            \\          ident
            \\            ident: a
            \\          infix
            \\            operator: *
            \\            ident
            \\              ident: b
            \\            ident
            \\              ident: c
            \\        infix
            \\          operator: /
            \\          ident
            \\            ident: d
            \\          ident
            \\            ident: e
            \\      ident
            \\        ident: f
            \\
        },
        .{
            "3 + 4; -5 * 5", // (3 + 4)((-5) * 5)
            \\root
            \\  expression
            \\    infix
            \\      operator: +
            \\      literal
            \\        int: 3
            \\      literal
            \\        int: 4
            \\  expression
            \\    infix
            \\      operator: *
            \\      prefix
            \\        op: -
            \\        literal
            \\          int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 > 4 == 3 < 4", // ((5 > 4) == (3 < 4))
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      infix
            \\        operator: >
            \\        literal
            \\          int: 5
            \\        literal
            \\          int: 4
            \\      infix
            \\        operator: <
            \\        literal
            \\          int: 3
            \\        literal
            \\          int: 4
            \\
        },
        .{
            "5 < 4 != 3 > 4", // ((5 < 4) != (3 > 4))
            \\root
            \\  expression
            \\    infix
            \\      operator: !=
            \\      infix
            \\        operator: <
            \\        literal
            \\          int: 5
            \\        literal
            \\          int: 4
            \\      infix
            \\        operator: >
            \\        literal
            \\          int: 3
            \\        literal
            \\          int: 4
            \\
        },
        .{
            "3 + 4 * 5 == 3 * 1 + 4 * 5", // (( 3 + (4 * 5)) == ((3 * 1) + (4 * 5)))
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 3
            \\        infix
            \\          operator: *
            \\          literal
            \\            int: 4
            \\          literal
            \\            int: 5
            \\      infix
            \\        operator: +
            \\        infix
            \\          operator: *
            \\          literal
            \\            int: 3
            \\          literal
            \\            int: 1
            \\        infix
            \\          operator: *
            \\          literal
            \\            int: 4
            \\          literal
            \\            int: 5
            \\
        },
        .{
            "true", // true
            \\root
            \\  expression
            \\    literal
            \\      boolean: true
            \\
        },
        .{
            "false", // false
            \\root
            \\  expression
            \\    literal
            \\      boolean: false
            \\
        },
        .{
            "3 > 5 == false", // ((3 > 5) == false)
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      infix
            \\        operator: >
            \\        literal
            \\          int: 3
            \\        literal
            \\          int: 5
            \\      literal
            \\        boolean: false
            \\
        },
        .{
            "3 < 5 == true", // ((3 < 5) == true)
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      infix
            \\        operator: <
            \\        literal
            \\          int: 3
            \\        literal
            \\          int: 5
            \\      literal
            \\        boolean: true
            \\
        },
        .{
            "1 + (2 + 3) + 4", // ((1 + (2 + 3)) + 4)
            \\root
            \\  expression
            \\    infix
            \\      operator: +
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 1
            \\        infix
            \\          operator: +
            \\          literal
            \\            int: 2
            \\          literal
            \\            int: 3
            \\      literal
            \\        int: 4
            \\
        },
        .{
            "(5 + 5) * 2", // ((5 + 5) * 2)
            \\root
            \\  expression
            \\    infix
            \\      operator: *
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 5
            \\        literal
            \\          int: 5
            \\      literal
            \\        int: 2
            \\
        },
        .{
            "2 / (5 + 5)", // (2 / (5 + 5))
            \\root
            \\  expression
            \\    infix
            \\      operator: /
            \\      literal
            \\        int: 2
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 5
            \\        literal
            \\          int: 5
            \\
        },
        .{
            "-(5 + 5)", // (-(5 + 5))
            \\root
            \\  expression
            \\    prefix
            \\      op: -
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 5
            \\        literal
            \\          int: 5
            \\
        },
        .{
            "!(true == true)", // (!(true == true))
            \\root
            \\  expression
            \\    prefix
            \\      op: !
            \\      infix
            \\        operator: ==
            \\        literal
            \\          boolean: true
            \\        literal
            \\          boolean: true
            \\
        },
    };

    inline for (tests) |case| {
        var ast_tree = try parseTree(testing.allocator, case[0]);
        defer ast_tree.deinit();

        const tree_string = try ast_tree.toString(testing.allocator);
        defer testing.allocator.free(tree_string);

        try testing.expectEqualStrings(case[1], tree_string);
    }
}

test "Boolean Literal Expression" {
    const input =
        \\true;
        \\false;
    ;

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const true_literal = try test_tree.addNode(.{
        .literal = .{
            .token = .{ .literal = "true", .location = null, .type = .True },
            .value = .{ .boolean = true },
        },
    });

    const false_literal = try test_tree.addNode(.{
        .literal = .{
            .token = .{ .literal = "false", .location = null, .type = .False },
            .value = .{ .boolean = false },
        },
    });

    const true_expr = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .type = .True, .location = null, .literal = "true" },
            .expr = true_literal,
        },
    });

    const false_expr = try test_tree.addNode(.{
        .expression = .{
            .token = .{ .type = .False, .location = null, .literal = "false" },
            .expr = false_literal,
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{ true_expr, false_expr },
        },
    });

    const tree_string = try ast_tree.toString(testing.allocator);
    defer testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(testing.allocator);
    defer testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}

test "If-Else Expression" {
    const input =
        \\input := if (x < y) { x } else { null };
    ;

    var ast_tree = try parseTree(testing.allocator, input);
    defer ast_tree.deinit();

    var test_tree = ast.Tree.init(testing.allocator);
    defer test_tree.deinit();

    const ident_input = try test_tree.addNode(.{
        .ident = .{
            .value = "input",
            .token = .{ .location = null, .type = .Ident, .literal = "input" },
        },
    });

    const ident_x = try test_tree.addNode(.{
        .ident = .{
            .value = "x",
            .token = .{ .location = null, .type = .Ident, .literal = "x" },
        },
    });

    const ident_y = try test_tree.addNode(.{
        .ident = .{
            .value = "y",
            .token = .{ .location = null, .type = .Ident, .literal = "y" },
        },
    });

    const null_literal = try test_tree.addNode(.{ .literal = .{
        .token = .{
            .literal = "null",
            .type = .Null,
            .location = null,
        },
        .value = .null,
    } });

    const condition_expr = try test_tree.addNode(.{
        .infix = .{
            .token = .{
                .location = null,
                .type = .Ident,
                .literal = "<",
            },
            .left = ident_x,
            .right = ident_y,
            .operator = "<",
        },
    });

    const null_expression = try test_tree.addNode(.{
        .expression = .{
            .token = .{
                .location = null,
                .type = .Null,
                .literal = "null",
            },
            .expr = null_literal,
        },
    });

    const ident_x_expression = try test_tree.addNode(.{
        .expression = .{
            .token = .{
                .location = null,
                .type = .Ident,
                .literal = "x",
            },
            .expr = ident_x,
        },
    });

    const consequence_block = try test_tree.addNode(.{
        .block = .{
            .token = .{
                .literal = "{",
                .location = null,
                .type = .OpenBrace,
            },
            .statements = &.{ident_x_expression},
        },
    });

    const alternative_block = try test_tree.addNode(.{
        .block = .{
            .token = .{
                .literal = "{",
                .location = null,
                .type = .OpenBrace,
            },
            .statements = &.{null_expression},
        },
    });

    const if_ = try test_tree.addNode(.{
        .if_ = .{
            .token = .{
                .literal = "if",
                .type = .If,
                .location = null,
            },
            .condition = condition_expr,
            .consequence = consequence_block,
            .alternative = alternative_block,
        },
    });

    const declare_assign = try test_tree.addNode(.{
        .declare_assign = .{
            .ident = ident_input,
            .token = .{
                .literal = ":=",
                .type = .DeclareAssign,
                .location = null,
            },
            .expr = if_,
        },
    });

    _ = try test_tree.addNode(.{
        .root = .{
            .statements = &.{declare_assign},
        },
    });

    const tree_string = try ast_tree.toString(testing.allocator);
    defer testing.allocator.free(tree_string);

    const test_tree_string = try test_tree.toString(testing.allocator);
    defer testing.allocator.free(test_tree_string);

    try testing.expectEqualStrings(test_tree_string, tree_string);
}
