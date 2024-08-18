// const std = @import("std");
// const fmt = std.fmt;
// const testing = std.testing;
//
// const ast = @import("ast.zig");
// const Program = ast.Program;
// const Statement = ast.Statement;
// const Expression = ast.Expression;
// const Integer = ast.Integer;
// const Lexer = @import("lexer.zig").Lexer;
// const Token = @import("token.zig").Token;
// const TokenType = @import("token.zig").TokenType;
// const TokenLiteral = @import("token.zig").TokenLiteral;
// const ArrayList = std.ArrayList;
// const test_allocator = std.testing.allocator;
// const assert = @import("std").debug.assert;
// const ArenaAllocator = std.heap.ArenaAllocator;
//
// /// A recursive descent pratt parser
// pub const Parser = struct {
//     const Precedence = enum {
//         // Sorted by precedence:
//         Lowest,
//         Equals, // ==
//         LessGreater, // > or <
//         Sum, // +
//         Product, // *
//         Prefix, // -X or !X
//         Call, // myFunction(X)
//
//         pub fn fromTokenType(tok: TokenType) @This() {
//             return switch (tok) {
//                 .EqEq => .Equals,
//                 .NotEq => .Equals,
//                 .Lt => .LessGreater,
//                 .Gt => .LessGreater,
//                 .Plus => .Sum,
//                 .Minus => .Sum,
//                 .Slash => .Product,
//                 .Star => .Product,
//                 else => .Lowest,
//             };
//         }
//     };
//     const PrefixParseFunc = *const fn (parser: *Parser) anyerror!Expression;
//     const InfixParseFunc = *const fn (parser: *Parser, left: *Expression) anyerror!Expression;
//
//     lexer: Lexer,
//     prev_token: Token = .{ .type = .Unknown, .literal = "", .location = null },
//     cur_token: Token = .{ .type = .Unknown, .literal = "", .location = null },
//     peek_token: Token = .{ .type = .Unknown, .literal = "", .location = null },
//     errors: ArrayList([]const u8),
//     prefix_parse_fn_map: std.AutoHashMap(TokenType, PrefixParseFunc),
//     infix_parse_fn_map: std.AutoHashMap(TokenType, InfixParseFunc),
//
//     /// use this for allocations that are cleaned up in Parser.deinit anyways eg. appending to errors, and are not returned externally
//     arena: ArenaAllocator,
//
//     pub fn init(alloc: std.mem.Allocator, lex: Lexer) !Parser {
//         var arena = ArenaAllocator.init(alloc);
//         errdefer arena.deinit();
//
//         const errors = ArrayList([]const u8).init(alloc);
//         var prefix_parse_fn_map = std.AutoHashMap(TokenType, PrefixParseFunc).init(alloc);
//         var infix_parse_fn_map = std.AutoHashMap(TokenType, InfixParseFunc).init(alloc);
//
//         // registerPrefix
//         try prefix_parse_fn_map.put(.Ident, parseIdentifier);
//         try prefix_parse_fn_map.put(.{ .Literal = .Int }, parseIntegerLiteral);
//         try prefix_parse_fn_map.put(.Bang, parsePrefixExpression);
//         try prefix_parse_fn_map.put(.Minus, parsePrefixExpression);
//
//         // registerInfix
//         try infix_parse_fn_map.put(.Plus, parseInfixExpression);
//         try infix_parse_fn_map.put(.Minus, parseInfixExpression);
//         try infix_parse_fn_map.put(.Slash, parseInfixExpression);
//         try infix_parse_fn_map.put(.Star, parseInfixExpression);
//         try infix_parse_fn_map.put(.EqEq, parseInfixExpression);
//         try infix_parse_fn_map.put(.NotEq, parseInfixExpression);
//         try infix_parse_fn_map.put(.Lt, parseInfixExpression);
//         try infix_parse_fn_map.put(.Gt, parseInfixExpression);
//
//         var parser = Parser{
//             .lexer = lex,
//             .errors = errors,
//             .prefix_parse_fn_map = prefix_parse_fn_map,
//             .infix_parse_fn_map = infix_parse_fn_map,
//             .arena = arena,
//         };
//
//         // read two tokens, so curToken and peekToken are both set
//         parser.nextToken();
//         parser.nextToken();
//
//         return parser;
//     }
//
//     pub fn deinit(self: *Parser) void {
//         self.errors.deinit();
//         self.prefix_parse_fn_map.deinit();
//         self.infix_parse_fn_map.deinit();
//         self.arena.deinit();
//     }
//
//     fn nextToken(self: *Parser) void {
//         self.prev_token = self.cur_token;
//         self.cur_token = self.peek_token;
//         self.peek_token = self.lexer.nextToken();
//     }
//
//     fn curTokenIs(self: Parser, token_type: TokenType) bool {
//         return self.cur_token.type.compareEq(token_type);
//     }
//
//     fn peekTokenIs(self: Parser, token_type: TokenType) bool {
//         return self.peek_token.type.compareEq(token_type);
//     }
//
//     fn prevTokenIs(self: Parser, token_type: TokenType) bool {
//         return self.prev_token.type.compareEq(token_type);
//     }
//
//     fn expectPeekAndEat(self: *Parser, token_type: TokenType) bool {
//         if (self.peekTokenIs(token_type)) {
//             self.nextToken();
//             return true;
//         }
//
//         return false;
//     }
//
//     fn expectPrevAndEat(self: *Parser, token_type: TokenType) bool {
//         if (self.prevTokenIs(token_type)) {
//             self.nextToken();
//             return true;
//         }
//         return false;
//     }
//
//     fn peekPrecedence(self: Parser) Precedence {
//         return Precedence.fromTokenType(self.peek_token.type);
//     }
//
//     fn curPrecedence(self: Parser) Precedence {
//         return Precedence.fromTokenType(self.cur_token.type);
//     }
//
//     pub fn checkParserErrors(self: Parser) !void {
//         if (self.errors.items.len == 0) {
//             return;
//         }
//
//         std.debug.print("parser has {d} errors\n", .{self.errors.items.len});
//         for (self.errors.items) |err| {
//             std.debug.print("parser error: {s}\n", .{err});
//         }
//
//         return error.ParserError;
//     }
//
//     pub fn parseProgram(self: *Parser, alloc: std.mem.Allocator) !Program {
//         var program = Program.init(alloc);
//
//         while (self.cur_token.type != .Eof) {
//             const statement = try self.parseStatement();
//             if (statement) |stmt| {
//                 try program.statements.append(stmt);
//             }
//             self.nextToken();
//         }
//
//         return program;
//     }
//
//     fn parseStatement(self: *Parser) !?Statement {
//         switch (self.cur_token.type) {
//             .Ident => {
//                 if (self.peekTokenIs(.DeclareAssign)) {
//                     self.nextToken();
//                     return try self.parseDeclareAssignStatement();
//                 } else {
//                     return try self.parseExpressionStatement();
//                 }
//             },
//             .Return => return try self.parseReturnStatement(),
//             else => {
//                 return try self.parseExpressionStatement();
//             },
//         }
//     }
//
//     fn parseExpressionStatement(self: *Parser) !Statement {
//         const statement = Statement{ .expression = .{
//             .token = self.cur_token,
//             .expr = try self.parseExpression(.Lowest),
//         } };
//
//         if (self.peekTokenIs(.Semi)) {
//             self.nextToken();
//         }
//
//         return statement;
//     }
//
//     fn parseDeclareAssignStatement(self: *Parser) !?Statement {
//         assert(self.cur_token.type == .DeclareAssign);
//
//         const declare_assign_token = self.cur_token;
//         const ident_expr = Expression{
//             .Ident = .{
//                 .token = self.prev_token,
//                 .value = self.prev_token.literal,
//                 .location = null,
//             },
//         };
//
//         if (!self.expectPrevAndEat(.Ident)) {
//             const err = try fmt.allocPrint(self.arena.allocator(), "Error parsing ':=' declare_assign. Expected identifier before declare_assign, but got: {s}\n", .{self.prev_token.literal});
//             try self.errors.append(err);
//             return null;
//         }
//
//         while (!self.curTokenIs(.Semi)) {
//             self.nextToken();
//         }
//
//         return Statement{
//             .DeclareAssign = .{
//                 .token = declare_assign_token,
//                 .ident_expr = ident_expr,
//                 .expr = null,
//             },
//         };
//     }
//
//     fn parseReturnStatement(self: *Parser) !?Statement {
//         assert(self.cur_token.type == .Return);
//
//         const return_token = self.cur_token;
//         self.nextToken();
//
//         // TODO: were skipping the expressions until we encounter a semi colon;
//         while (!self.curTokenIs(.Semi)) {
//             self.nextToken();
//         }
//
//         return Statement.Return{ .token = return_token, .expr = null };
//     }
//
//     fn parseExpression(self: *Parser, precedence: Precedence) !?Expression {
//         const prefix = self.prefix_parse_fn_map.get(self.cur_token.type);
//
//         if (prefix == null) {
//             const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "no prefix parse function for {any} found.", .{self.cur_token.type});
//             try self.errors.append(err_msg);
//             return null;
//         }
//
//         std.debug.print("prefix is currently!? {any}\n", .{prefix});
//         var left_expr = try prefix.?(self);
//
//         while (!self.peekTokenIs(.Semi) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
//             const infix = self.infix_parse_fn_map.get(self.peek_token.type);
//             if (infix == null) {
//                 return left_expr;
//             }
//
//             self.nextToken();
//
//             left_expr = try infix.?(self, &left_expr);
//         }
//         return left_expr;
//     }
//
//     fn parseIdentifier(self: *Parser) !Expression {
//         assert(self.cur_token.type == .Ident);
//
//         return Expression.Ident{ .token = self.cur_token, .value = self.cur_token.literal };
//     }
//
//     fn parseIntegerLiteral(self: *Parser) !Expression {
//         const value = std.fmt.parseInt(i32, self.cur_token.literal, 0) catch |err| {
//             const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "could not parse {s} as integer literal: {s}", .{ self.cur_token.literal, @errorName(err) });
//             try self.errors.append(err_msg);
//             return err;
//         };
//
//         return Expression.Literal{
//             .Integer = .{
//                 .token = self.cur_token,
//                 .value = value,
//             },
//         };
//     }
//
//     fn parsePrefixExpression(self: *Parser) !Expression {
//         const operator = self.cur_token.literal;
//         const token = self.cur_token;
//
//         self.nextToken();
//
//         const right = try self.parseExpression(.Prefix);
//
//         if (right == null) {
//             const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "could not parse right for prefix expression: at {any}", .{token});
//             try self.errors.append(err_msg);
//             return error.PrefixExpressionDoesNotExist;
//         }
//
//         const allocated = try self.arena.allocator().create(Expression);
//         allocated.* = right.?;
//
//         return Expression.Prefix{
//             .operator = operator,
//             .token = token,
//             .right = allocated,
//         };
//     }
//
//     fn parseInfixExpression(self: *Parser, left: *Expression) !Expression {
//         const token = self.cur_token;
//         const operator = self.cur_token.literal;
//
//         const precedence = self.curPrecedence();
//         self.nextToken();
//
//         const parsed_right = try self.parseExpression(precedence);
//         if (parsed_right == null) {
//             const err_msg = try std.fmt.allocPrint(self.arena.allocator(), "could not parse right for prefix expression: at {any}", .{token});
//             try self.errors.append(err_msg);
//             return error.InfixExpressionDoesNotExist;
//         }
//
//         const allocated = try self.arena.allocator().create(Expression);
//         allocated.* = parsed_right.?;
//
//         return Expression.Infix{
//             .token = token,
//             .operator = operator,
//             .left = left,
//             .right = allocated,
//         };
//     }
// };

// // ------------- Testing ---------------
// fn parseProgram(alloc: std.mem.Allocator, input: []const u8) !Program {
//     const lexer = Lexer.init(input);
//     var parser = try Parser.init(alloc, lexer);
//     defer parser.deinit();
//
//     const program = try parser.parseProgram(alloc);
//     try parser.checkParserErrors();
//
//     return program;
// }
//
// fn expectParsedEqStatements(input: []const u8, expected_statements: []const Statement) !void {
//     var program = try parseProgram(test_allocator, input);
//     defer program.deinit();
//
//     if (expected_statements.len != program.statements.items.len) {
//         std.debug.print("\nMismatch in number of statements:\n", .{});
//         std.debug.print("Expected {} statements, but got {}.\n\n", .{ expected_statements.len, program.statements.items.len });
//         std.debug.print("Input: {s}\n\n", .{input});
//         std.debug.print("Parsed statements:\n\n", .{});
//         for (program.statements.items, 0..) |stmt, i| {
//             std.debug.print("Statement {}:\n {}\n\n", .{ i, std.json.fmt(stmt, .{ .whitespace = .indent_2 }) });
//         }
//         return error.StatementCountMismatch;
//     }
//
//     for (expected_statements, 0..) |expected, i| {
//         const actual = program.statements.items[i];
//         expectStatementEq(expected, actual) catch |err| {
//             std.debug.print("\nMismatch in statement {}:\n", .{i});
//             std.debug.print("Expected: {any}\n", .{expected});
//             std.debug.print("Actual: {any}\n", .{actual});
//             return err;
//         };
//     }
// }
//
// fn expectStatementEq(expected: Statement, actual: Statement) !void {
//     if (@as(std.meta.Tag(Statement), expected) != @as(std.meta.Tag(Statement), actual)) {
//         std.debug.print("Statement type mismatch. Expected: {}, got: {}\n", .{ @as(std.meta.Tag(Statement), expected), @as(std.meta.Tag(Statement), actual) });
//         return error.StatementTypeMismatch;
//     }
//
//     switch (expected) {
//         .declare_assign => |declare_assign| {
//             const actual_declare_assign = actual.declare_assign;
//
//             try testing.expectEqual(declare_assign.token.type, actual_declare_assign.token.type);
//             try testing.expectEqualStrings(declare_assign.token.literal, actual_declare_assign.token.literal);
//             try expectIdentifierExpressionEq(declare_assign.ident_expr, actual_declare_assign.ident_expr);
//
//             // TODO: dont test .expr until implemented
//             // if (declare_assign.expr) |expr| {
//             //     try testing.expectEqual(expr, actual_declare_assign.expr);
//             //     try expectExpressionEq(expr, actual_declare_assign.expr.?);
//             // }
//         },
//         .return_ => |return_| {
//             const actual_return = actual.return_;
//
//             try testing.expectEqual(return_.token.type, actual_return.token.type);
//             try testing.expectEqualStrings(return_.token.literal, actual_return.token.literal);
//
//             // TODO: dont test .expr until implemented
//             // if (return_.expr) |expr| {
//             //     try testing.expectEqual(expr, actual_return.expr);
//             //     try expectExpressionEq(expr, actual_return.expr.?);
//             // }
//         },
//         .expression => |expression| {
//             const actual_expression = actual.expression;
//
//             try testing.expectEqual(expression.token.type, actual_expression.token.type);
//             try testing.expectEqualStrings(expression.token.literal, actual_expression.token.literal);
//
//             // TODO: dont test .expr until implemented
//             // if (expression.expr) |expr| {
//             //     try testing.expectEqual(expr, actual_expression.expr);
//             //     try expectExpressionEq(expr, actual_expression.expr.?);
//             // }
//         },
//     }
// }
//
// fn expectExpressionEq(expected: Expression, actual: Expression) !void {
//     if (@as(std.meta.Tag(Expression), expected) != @as(std.meta.Tag(Expression), actual)) {
//         std.debug.print("Expression type mismatch. Expected: {}, got: {}\n", .{ @as(std.meta.Tag(Expression), expected), @as(std.meta.Tag(Expression), actual) });
//         return error.ExpressionTypeMismatch;
//     }
//
//     switch (expected) {
//         .ident_expr => |ident_expr| {
//             try expectIdentifierExpressionEq(ident_expr, actual.ident_expr);
//         },
//         .int_literal_expr => |int_literal_expr| {
//             try expectIntLiteralExpressionEq(int_literal_expr, actual.int_literal_expr);
//         },
//         .prefix_expr => |prefix_expr| {
//             try expectPrefixExpressionEq(prefix_expr, actual.prefix_expr);
//         },
//         .infix_expr => |infix_expr| {
//             try expectInfixExpressionEq(infix_expr, actual.infix_expr);
//         },
//     }
// }
//
// fn expectIdentifierExpressionEq(expected: Expression.Ident, actual: Expression.Ident) !void {
//     try testing.expectEqual(expected.token.type, actual.token.type);
//     try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
//     try testing.expectEqualStrings(expected.value, actual.value);
// }
//
// fn expectIntLiteralExpressionEq(expected: Expression.Literal, actual: Expression.Literal) !void {
//     try testing.expectEqual(expected.token.type, actual.token.type);
//     try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
//     try testing.expectEqual(expected.value, actual.value);
// }
//
// fn expectPrefixExpressionEq(expected: Expression.Prefix, actual: Expression.Prefix) anyerror!void {
//     try testing.expectEqual(expected.token.type, actual.token.type);
//     try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
//     try testing.expectEqualStrings(expected.operator, actual.operator);
//     try expectExpressionEq(expected.right.*, actual.right.*);
// }
//
// fn expectInfixExpressionEq(expected: Expression.Infix, actual: Expression.Infix) anyerror!void {
//     try testing.expectEqual(expected.token.type, actual.token.type);
//     try testing.expectEqualStrings(expected.token.literal, actual.token.literal);
//     try testing.expectEqualStrings(expected.operator, actual.operator);
//     try expectExpressionEq(expected.right.*, actual.right.*);
//     try expectExpressionEq(expected.left.*, actual.left.*);
// }
//
// const five_token = Token{
//     .type = .{ .Literal = .Int },
//     .literal = "5",
// };
//
// const five_expr = Expression{ .Literal = .{
//     .Integer = .{
//         .value = 5,
//         .location = null,
//     },
// } };
//
// fn gen_infix_statement(alloc: std.mem.Allocator, operator: []const u8) !Statement {
//     const left = try alloc.create(Expression);
//     left.* = five_expr;
//
//     const right = try alloc.create(Expression);
//     right.* = five_expr;
//
//     return .{
//         .expression = .{
//             .token = five_token,
//             .expr = .{
//                 .infix_expr = .{
//                     .token = five_token,
//                     .left = left,
//                     .operator = operator,
//                     .right = right,
//                 },
//             },
//         },
//     };
// }
//
// test "DeclareAssign Statement" {
//     const input =
//         \\x := 5;
//     ;
//
//     const tests = [_]Statement{
//         .{
//             // x := 5;
//             .declare_assign = .{
//                 .ident_expr = .{
//                     .value = "x",
//                     .token = .{
//                         .type = .Ident,
//                         .literal = "x",
//                     },
//                 },
//                 .token = .{
//                     .type = .DeclareAssign,
//                     .literal = ":=",
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
// test "Return Statement" {
//     const input =
//         \\return 5;
//     ;
//
//     const tests = [_]Statement{
//         // return 5;
//         .{
//             .return_ = .{
//                 .token = .{
//                     .type = .Return,
//                     .literal = "return",
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
// test "Statement Expression" {
//     const input = "foobar;";
//
//     const tests = [_]Statement{
//         // foobar;
//         .{
//             .expression = .{
//                 .token = .{
//                     .type = .Ident,
//                     .literal = "foobar",
//                 },
//                 .expr = null, // TODO
//             },
//         },
//     };
//
//     try expectParsedEqStatements(input, &tests);
// }
//
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
//         var program = try parseProgram(test_allocator, expected[0]);
//         defer program.deinit();
//         std.debug.print("{any}", .{std.json.fmt(program.statements.items, .{})});
//         std.debug.print("to str {s}", .{expected[0]});
//         // const actual = try program.toString(test_allocator);
//         // try testing.expectEqualStrings(expected[1], actual);
//     }
// }
