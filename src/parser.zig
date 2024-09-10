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
                .OpenParen => .Call,
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

    pub fn init(gpa: std.mem.Allocator, lex: Lexer) !Parser {
        var arena = ArenaAllocator.init(gpa);
        errdefer arena.deinit();

        const errors = ArrayList([]const u8).init(gpa);
        var prefix_parse_fn_map = std.AutoHashMap(TokenType, PrefixParseFunc).init(gpa); // TODO: can we have a filled map initially or use a different approach?
        var infix_parse_fn_map = std.AutoHashMap(TokenType, InfixParseFunc).init(gpa);

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
        try prefix_parse_fn_map.put(.Fn, parseFunctionLiteral);

        // registerInfix
        try infix_parse_fn_map.put(.Plus, parseInfixExpression);
        try infix_parse_fn_map.put(.Minus, parseInfixExpression);
        try infix_parse_fn_map.put(.Slash, parseInfixExpression);
        try infix_parse_fn_map.put(.Star, parseInfixExpression);
        try infix_parse_fn_map.put(.EqEq, parseInfixExpression);
        try infix_parse_fn_map.put(.NotEq, parseInfixExpression);
        try infix_parse_fn_map.put(.Lt, parseInfixExpression);
        try infix_parse_fn_map.put(.Gt, parseInfixExpression);
        try infix_parse_fn_map.put(.OpenParen, parseCallExpression);

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

    pub fn parseTree(self: *Parser, gpa: std.mem.Allocator) !ast.Tree {
        var ast_tree = ast.Tree.init(gpa);
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

        if (self.peekTokenIs(.Semi)) {
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

        const expr = try self.parseExpression(ast_tree, .Lowest);

        if (self.peekTokenIs(.Semi)) {
            self.nextToken();
        }

        return try ast_tree.addNode(.{
            .return_ = .{
                .token = return_token,
                .expr = expr,
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

    fn parseFunctionLiteral(self: *Parser, ast_tree: *ast.Tree) !ast.NodeIndex {
        const start_token = self.cur_token;

        if (!self.expectPeekAndEat(.OpenParen)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any OpenParen for function literal starting at: {any}.", .{start_token.type});
            try self.errors.append(err_msg);
            return error.FunctionLiteralParamsNotOpened;
        }

        const parameters = try self.parseFunctionParameters(ast_tree);

        if (!self.expectPeekAndEat(.OpenBrace)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any OpenBrace for function literal body starting at: {any}.", .{start_token.type});
            try self.errors.append(err_msg);
            return error.FunctionLiteralParamsNotClosed;
        }

        return ast_tree.addNode(.{
            .function = .{
                .token = start_token,
                .parameters = parameters,
                .body = try self.parseBlockStatement(ast_tree),
            },
        });
    }

    // TODO: add typed parameters!
    fn parseFunctionParameters(self: *Parser, ast_tree: *ast.Tree) ![]ast.NodeIndex {
        var parameters = std.ArrayList(ast.NodeIndex).init(ast_tree.arena.allocator());

        if (self.peekTokenIs(.CloseParen)) {
            self.nextToken();
            return parameters.toOwnedSlice();
        }

        self.nextToken();

        {
            const ident = try ast_tree.addNode(.{
                .ident = .{
                    .token = self.cur_token,
                    .value = self.cur_token.literal,
                },
            });

            try parameters.append(ident);
        }

        while (self.peekTokenIs(.Comma)) {
            self.nextToken();
            self.nextToken();
            const ident = try ast_tree.addNode(.{
                .ident = .{
                    .token = self.cur_token,
                    .value = self.cur_token.literal,
                },
            });
            try parameters.append(ident);
        }

        if (!self.expectPeekAndEat(.CloseParen)) {
            try self.errors.append("Could not find any CloseParen for function parameters.");
            return error.FunctionLiteralParamsNotClosed;
        }

        return parameters.toOwnedSlice();
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

    fn parseCallExpression(self: *Parser, ast_tree: *ast.Tree, function: ast.NodeIndex) !ast.NodeIndex {
        return ast_tree.addNode(.{
            .call = .{
                .token = self.cur_token,
                .function = function,
                .arguments = try self.parseCallArguments(ast_tree),
            },
        });
    }

    fn parseCallArguments(self: *Parser, ast_tree: *ast.Tree) ![]ast.NodeIndex {
        var args = ArrayList(ast.NodeIndex).init(ast_tree.arena.allocator());

        if (self.peekTokenIs(.CloseParen)) {
            self.nextToken();
            return args.toOwnedSlice();
        }

        self.nextToken();
        try args.append(try self.parseExpression(ast_tree, .Lowest));
        while (self.peekTokenIs(.Comma)) {
            self.nextToken();
            self.nextToken();
            try args.append(try self.parseExpression(ast_tree, .Lowest));
        }

        if (!self.expectPeekAndEat(.CloseParen)) {
            const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "Could not find any CloseParen after comma seperated call arguments, found: {any}", .{self.cur_token});
            try self.errors.append(err_msg);
            return error.CallArgumentsNotClosed;
        }

        return args.toOwnedSlice();
    }
};

// ------------- Testing ---------------
fn testTreeString(gpa: std.mem.Allocator, input_to_parse: []const u8, expected_tree_str: []const u8) !void {
    const lexer = Lexer.init(input_to_parse);
    var parser = try Parser.init(gpa, lexer);
    defer parser.deinit();

    var ast_tree = parser.parseTree(gpa) catch |err| {
        try parser.checkParserErrors();
        return err;
    };
    defer ast_tree.deinit();

    const tree_string = try ast_tree.toString(testing.allocator);
    defer testing.allocator.free(tree_string);

    try testing.expectEqualStrings(expected_tree_str, tree_string);
}

test "DeclareAssign Statement" {
    const tests = .{
        .{
            "x := 5;",
            \\root
            \\  declare_assign
            \\    ident
            \\      ident: x
            \\    literal
            \\      int: 5
            \\
        },
        .{
            "y := true;",
            \\root
            \\  declare_assign
            \\    ident
            \\      ident: y
            \\    literal
            \\      boolean: true
            \\
        },
        .{
            "foobar := y;",
            \\root
            \\  declare_assign
            \\    ident
            \\      ident: foobar
            \\    ident
            \\      ident: y
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Return Statement" {
    const tests = .{
        .{
            "return 5;",
            \\root
            \\  return_
            \\    literal
            \\      int: 5
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Statement Expression" {
    const tests = .{
        .{
            "foobar;",
            \\root
            \\  expression
            \\    ident
            \\      ident: foobar
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Integer Literal Expression" {
    const tests = .{
        .{
            "5;",
            \\root
            \\  expression
            \\    literal
            \\      int: 5
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Prefix Expression" {
    const tests = .{
        .{
            "!5;",
            \\root
            \\  expression
            \\    prefix
            \\      op: !
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "-5;",
            \\root
            \\  expression
            \\    prefix
            \\      op: -
            \\      literal
            \\        int: 5
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Infix Expression" {
    const tests = .{
        .{
            "5 + 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: +
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
            ,
        },
        .{
            "5 - 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: -
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 * 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: *
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 / 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: /
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 > 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: >
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 < 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: <
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 == 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "5 != 5;",
            \\root
            \\  expression
            \\    infix
            \\      operator: !=
            \\      literal
            \\        int: 5
            \\      literal
            \\        int: 5
            \\
        },
        .{
            "true == true;",
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      literal
            \\        boolean: true
            \\      literal
            \\        boolean: true
            \\
        },
        .{
            "false == false;",
            \\root
            \\  expression
            \\    infix
            \\      operator: ==
            \\      literal
            \\        boolean: false
            \\      literal
            \\        boolean: false
            \\
        },
        .{
            "true != false;",
            \\root
            \\  expression
            \\    infix
            \\      operator: !=
            \\      literal
            \\        boolean: true
            \\      literal
            \\        boolean: false
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
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
        .{
            "a + add(b * c) + d", // (( a + add((b * c))) + d)
            \\root
            \\  expression
            \\    infix
            \\      operator: +
            \\      infix
            \\        operator: +
            \\        ident
            \\          ident: a
            \\        call
            \\          infix
            \\            operator: *
            \\            ident
            \\              ident: b
            \\            ident
            \\              ident: c
            \\          ident
            \\            ident: add
            \\      ident
            \\        ident: d
            \\
        },
        .{
            "add(a, b, 1, 2 * 3, 4 + 5, add (6, 7 * 8))", // add(a, b, 1, (2 * 3), (4 + 5), add(6, 7 * 8))
            \\root
            \\  expression
            \\    call
            \\      ident
            \\        ident: a
            \\      ident
            \\        ident: b
            \\      literal
            \\        int: 1
            \\      infix
            \\        operator: *
            \\        literal
            \\          int: 2
            \\        literal
            \\          int: 3
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 4
            \\        literal
            \\          int: 5
            \\      call
            \\        literal
            \\          int: 6
            \\        infix
            \\          operator: *
            \\          literal
            \\            int: 7
            \\          literal
            \\            int: 8
            \\        ident
            \\          ident: add
            \\      ident
            \\        ident: add
            \\
        },
        .{
            "add(a + b + c * d / f + g)", // add((((a + b) + ((c + d) / f)) + g))
            \\root
            \\  expression
            \\    call
            \\      infix
            \\        operator: +
            \\        infix
            \\          operator: +
            \\          infix
            \\            operator: +
            \\            ident
            \\              ident: a
            \\            ident
            \\              ident: b
            \\          infix
            \\            operator: /
            \\            infix
            \\              operator: *
            \\              ident
            \\                ident: c
            \\              ident
            \\                ident: d
            \\            ident
            \\              ident: f
            \\        ident
            \\          ident: g
            \\      ident
            \\        ident: add
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Boolean Literal Expression" {
    const tests = .{
        .{
            "true;",
            \\root
            \\  expression
            \\    literal
            \\      boolean: true
            \\
            ,
            "false;",
            \\root
            \\  expression
            \\    literal
            \\      boolean: false
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "If Expression" {
    const tests = .{
        .{
            "input := if (x < y) { x };",
            \\root
            \\  declare_assign
            \\    ident
            \\      ident: input
            \\    if_
            \\      infix
            \\        operator: <
            \\        ident
            \\          ident: x
            \\        ident
            \\          ident: y
            \\      block
            \\        expression
            \\          ident
            \\            ident: x
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}
test "If-Else Expression" {
    const tests = .{
        .{
            "input := if (x < y) { x } else { null };",
            \\root
            \\  declare_assign
            \\    ident
            \\      ident: input
            \\    if_
            \\      infix
            \\        operator: <
            \\        ident
            \\          ident: x
            \\        ident
            \\          ident: y
            \\      block
            \\        expression
            \\          ident
            \\            ident: x
            \\      block
            \\        expression
            \\          literal
            \\            null: null
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Function Literal + parameters" {
    const tests = .{
        .{
            "fn(x, y) { x + y; };",
            \\root
            \\  expression
            \\    function
            \\      ident
            \\        ident: x
            \\      ident
            \\        ident: y
            \\      block
            \\        expression
            \\          infix
            \\            operator: +
            \\            ident
            \\              ident: x
            \\            ident
            \\              ident: y
            \\
        },
        .{
            "fn() {};",
            \\root
            \\  expression
            \\    function
            \\      block
            \\
        },
        .{
            "fn(x) {};",
            \\root
            \\  expression
            \\    function
            \\      ident
            \\        ident: x
            \\      block
            \\
        },
        .{
            "fn(x, y, z) {};",
            \\root
            \\  expression
            \\    function
            \\      ident
            \\        ident: x
            \\      ident
            \\        ident: y
            \\      ident
            \\        ident: z
            \\      block
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}

test "Call Expression" {
    const tests = .{
        .{
            "add(1, 2 * 3, 4 + 5);",
            \\root
            \\  expression
            \\    call
            \\      literal
            \\        int: 1
            \\      infix
            \\        operator: *
            \\        literal
            \\          int: 2
            \\        literal
            \\          int: 3
            \\      infix
            \\        operator: +
            \\        literal
            \\          int: 4
            \\        literal
            \\          int: 5
            \\      ident
            \\        ident: add
            \\
        },
    };

    inline for (tests) |case| {
        try testTreeString(testing.allocator, case[0], case[1]);
    }
}
