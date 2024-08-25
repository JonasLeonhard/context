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
        var prefix_parse_fn_map = std.AutoHashMap(TokenType, PrefixParseFunc).init(alloc);
        var infix_parse_fn_map = std.AutoHashMap(TokenType, InfixParseFunc).init(alloc);

        // registerPrefix
        try prefix_parse_fn_map.put(.Ident, parseIdentifier);
        try prefix_parse_fn_map.put(.{ .Literal = .Int }, parseIntegerLiteral);
        try prefix_parse_fn_map.put(.Bang, parsePrefixExpression);
        try prefix_parse_fn_map.put(.Minus, parsePrefixExpression);

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
        // TODO: missing type...
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

        while (!self.curTokenIs(.Semi)) {
            self.nextToken();
        }

        return try ast_tree.addNode(.{
            .declare_assign = .{
                .token = declare_assign_token,
                .ident = ident,
                .expr = null, // TODO
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

    const ast_tree = try parser.parseTree(alloc);
    try parser.checkParserErrors();

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

    // const literal_5 = try test_tree.addNode(
    //     .{
    //         .literal = .{
    //             .token = Token{
    //                 .type = .{
    //                     .Literal = .Int,
    //                 },
    //                 .literal = "5",
    //                 .location = null,
    //             },
    //             .value = .{
    //                 .int = 5,
    //             },
    //         },
    //     },
    // );

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
            .expr = null, // TODO: literal_5
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

    try testing.expectEqualStrings(tree_string, test_tree_string);
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

    try testing.expectEqualStrings(tree_string, test_tree_string);
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

    try testing.expectEqualStrings(tree_string, test_tree_string);
}

// test "Integer Literal Expression" {
//     const input = "5;";
//
//     const tests = [_]Statement{
//         // foobar;
//         .{
//             .expression = .{
//                 .token = .{
//                     .type = .{
//                         .Literal = .Int,
//                     },
//                     .literal = "5",
//                 },
//                 .expr = .{
//                     .int_literal_expr = .{
//                         .token = .{
//                             .type = .{
//                                 .Literal = .Int,
//                             },
//                             .literal = "5",
//                         },
//                         .value = 5,
//                     },
//                 },
//             },
//         },
//     };
//
//     try expectParsedEqStatements(input, &tests);
// }
//
// test "Prefix Expression" {
//     const input =
//         \\!5;
//         \\-5;
//     ;
//
//     const right_five = try testing.allocator.create(Expression);
//     right_five.* = .{
//         .int_literal_expr = .{
//             .token = five_token,
//             .value = 5,
//         },
//     };
//
//     const right_fifteen = try testing.allocator.create(Expression);
//     right_fifteen.* = .{
//         .int_literal_expr = .{
//             .token = .{
//                 .type = .{
//                     .Literal = .Int,
//                 },
//                 .literal = "15",
//             },
//             .value = 15,
//         },
//     };
//
//     const tests = [_]Statement{
//         // foobar;
//         .{
//             .expression = .{
//                 .token = .{
//                     .type = .Bang,
//                     .literal = "!",
//                 },
//                 .expr = .{
//                     .prefix_expr = .{
//                         .token = .{
//                             .type = .Bang,
//                             .literal = "!",
//                         },
//                         .operator = "!",
//                         .right = right_five,
//                     },
//                 },
//             },
//         },
//         .{
//             .expression = .{
//                 .token = .{
//                     .type = .Minus,
//                     .literal = "-",
//                 },
//                 .expr = .{
//                     .prefix_expr = .{
//                         .token = .{
//                             .type = .Minus,
//                             .literal = "-",
//                         },
//                         .operator = "-",
//                         .right = right_fifteen,
//                     },
//                 },
//             },
//         },
//     };
//
//     defer for (tests) |test_stmt| {
//         test_stmt.deinit(testing.allocator);
//     };
//
//     try expectParsedEqStatements(input, &tests);
// }
//
// test "Infix Expression" {
//     const input =
//         \\5 + 5;
//         \\5 - 5;
//         \\5 * 5;
//         \\5 / 5;
//         \\5 > 5;
//         \\5 < 5;
//         \\5 == 5;
//         \\5 != 5;
//     ;
//
//     const tests = [_]Statement{
//         try gen_infix_statement(testing.allocator, "+"),
//         try gen_infix_statement(testing.allocator, "-"),
//         try gen_infix_statement(testing.allocator, "*"),
//         try gen_infix_statement(testing.allocator, "/"),
//         try gen_infix_statement(testing.allocator, ">"),
//         try gen_infix_statement(testing.allocator, "<"),
//         try gen_infix_statement(testing.allocator, "=="),
//         try gen_infix_statement(testing.allocator, "!="),
//     };
//
//     defer for (tests) |test_stmt| {
//         test_stmt.deinit(testing.allocator);
//     };
//
//     try expectParsedEqStatements(input, &tests);
// }
//
// test "Operator Precedence" {
//     const tests = .{
//         .{
//             "-a * b",
//             "((-a) * b)",
//         },
//         .{
//             "!-a",
//             "(!(-a))",
//         },
//         .{
//             "a + b + c",
//             "((a + b) + c)",
//         },
//         .{
//             "a * b * c",
//             "((a * b) * c)",
//         },
//         .{
//             "a * b / c",
//             "((a * b) / c)",
//         },
//         .{
//             "a + b / c",
//             "(a + (b / c))",
//         },
//         .{
//             "a + b * c + d / e - f",
//             "(((a + (b * c)) + (d / e)) - f)",
//         },
//         .{
//             "3 + 4; -5 * 5",
//             "(3 + 4)((-5) * 5)",
//         },
//         .{
//             "5 > 4 == 3 < 4",
//             "((5 > 4) == (3 < 4))",
//         },
//         .{
//             "5 < 4 != 3 > 4",
//             "((5 < 4) != (3 > 4))",
//         },
//         .{
//             "3 + 4 * 5 == 3 * 1 + 4 * 5",
//             "(( 3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
//         },
//     };
//
//     // TODO: seems like .operator of the parsed program has an invalid pointer or something?
//     inline for (tests) |expected| {
//         var program = try parseTree(test_allocator, expected[0]);
//         defer program.deinit();
//         std.debug.print("{any}", .{std.json.fmt(program.statements.items, .{})});
//         std.debug.print("to str {s}", .{expected[0]});
//         // const actual = try program.toString(test_allocator);
//         // try testing.expectEqualStrings(expected[1], actual);
//     }
// }
