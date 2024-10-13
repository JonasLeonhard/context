const std = @import("std");
const testing = std.testing;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const obj = @import("object.zig");
const Object = obj.Object;
const Environment = @import("Environment.zig");
const ast = @import("ast.zig");
const Tree = ast.Tree;
const Expression = ast.Expression;
const Statement = ast.Statement;
const ArrayList = std.ArrayList;

const Evaluator = @This();

arena: std.heap.ArenaAllocator,

pub fn init(alloc: std.mem.Allocator) Evaluator {
    return Evaluator{
        .arena = std.heap.ArenaAllocator.init(alloc),
    };
}

pub fn deinit(self: Evaluator) void {
    self.arena.deinit();
}

pub fn reset(self: *Evaluator) void {
    _ = self.arena.reset(.free_all);
}

pub fn evalTree(self: *Evaluator, ast_tree: *Tree, env: *Environment) !Object {
    const result = try self.evalStatements(ast_tree.nodes, env);
    if (result == .return_) {
        return result.return_.value.*;
    }
    return result;
}

pub fn evalStatements(self: *Evaluator, statements: std.ArrayList(Statement), env: *Environment) anyerror!Object {
    var last_result = Object{ .null = .{} };

    for (statements.items) |statement| {
        const result = try self.evalStatement(statement, env);

        if (result == .return_ or result == .error_) {
            return result;
        }

        last_result = result;
    }

    return last_result;
}

pub fn evalStatement(self: *Evaluator, statement: Statement, env: *Environment) !Object {
    const result = switch (statement) {
        .expression => |expr| try self.evalExpressionStatement(expr, env),
        .block => |block| try self.evalBlockStatement(block, env),
        .return_ => |return_| try self.evalReturnStatement(return_, env),
        .declare_assign => |declare_assign| try self.evalDeclareAssignStatement(declare_assign, env),
    };
    return result;
}

fn evalExpressionStatement(self: *Evaluator, statement: Statement.ExpressionStatement, env: *Environment) !Object {
    if (statement.expr) |expr| {
        return try self.evalExpression(expr, env);
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "Missing Expression in ExpressionStatment: {any}", .{statement}),
        },
    };
}

fn evalBlockStatement(self: *Evaluator, block: Statement.BlockStatement, env: *Environment) !Object {
    return self.evalStatements(block.statements, env);
}

fn evalReturnStatement(self: *Evaluator, statement: Statement.ReturnStatement, env: *Environment) !Object {
    if (statement.expr) |expr| {
        const value = try self.evalExpression(expr, env);
        const val_obj = try self.arena.allocator().create(Object);
        val_obj.* = value;

        return Object{ .return_ = .{ .value = val_obj } };
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "Empty expression in ReturnStatement: {any}", .{statement}),
        },
    };
}

fn evalDeclareAssignStatement(self: *Evaluator, statement: Statement.DeclareAssignStatement, env: *Environment) !Object {
    if (statement.expr) |expr| {
        var expr_obj = try self.evalExpression(expr, env);
        _ = try env.set(statement.ident.value, &expr_obj);
        return expr_obj;
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "Empty expression in DeclareAssignStatment: {any}", .{statement}),
        },
    };
}

pub fn evalExpressions(self: *Evaluator, expressions: []Expression, env: *Environment) anyerror!ArrayList(Object) {
    var results = ArrayList(Object).init(self.arena.allocator());
    for (expressions) |exp| {
        try results.append(try self.evalExpression(exp, env));
    }
    return results;
}

pub fn evalExpression(self: *Evaluator, expression: Expression, env: *Environment) !Object {
    return switch (expression) {
        .literal => |literal| evalLiteralExpression(literal),
        .prefix => |prefix| try self.evalPrefixExpression(prefix, env),
        .infix => |infix| try self.evalInfixExpression(infix, env),
        .if_ => |if_| try self.evalIfExpression(if_, env),
        .ident => |ident| try self.evalIdentExpression(ident, env),
        .call => |call| try self.evalCallExpression(call, env),
        .function => |function| try evalFunctionExpression(function, env),
        .index => |index| try evalIndexExpression(index, env),
    };
}

fn evalLiteralExpression(literal: Expression.Literal) !Object {
    return switch (literal.value) {
        .int => |int_val| {
            return Object{ .integer = .{ .value = int_val } };
        },
        .string => |str_val| {
            return Object{ .string = .{ .value = str_val } };
        },
        .boolean => |bool_val| {
            return Object{ .boolean = .{ .value = bool_val } };
        },
        .null => {
            return Object{ .null = .{} };
        },
        else => {
            std.debug.print("EvalLiteralValueNotImplementedYet for {any}\n", .{literal});
            return error.EvalLiteralValueNotImplementedYet;
        },
    };
}

fn evalPrefixExpression(self: *Evaluator, prefix: Expression.Prefix, env: *Environment) anyerror!Object {
    const right = try self.evalExpression(prefix.right.*, env);
    if (right == .error_) {
        return right;
    }

    if (std.mem.eql(u8, prefix.operator, "!")) {
        return try evalBangOperatorExpression(right);
    }

    if (std.mem.eql(u8, prefix.operator, "-")) {
        return try self.evalMinusPrefixOperatorExpression(right);
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: {s}{s}", .{ prefix.operator, @tagName(right) }),
        },
    };
}

fn evalInfixExpression(self: *Evaluator, infix: Expression.Infix, env: *Environment) anyerror!Object {
    const left = try self.evalExpression(infix.left.*, env);
    if (left == .error_)
        return left;

    const right = try self.evalExpression(infix.right.*, env);
    if (right == .error_)
        return right;

    if (left == .integer and right == .integer) {
        return self.evalIntegerInfixExpression(infix.operator, left.integer, right.integer);
    }

    if (left == .string and right == .string) {
        return self.evalStringInfixExpression(infix.operator, left.string, right.string);
    }

    if (std.mem.eql(u8, infix.operator, "==")) {
        return Object{ .boolean = .{ .value = std.meta.eql(left, right) } };
    }

    if (std.mem.eql(u8, infix.operator, "!=")) {
        return Object{ .boolean = .{ .value = !std.meta.eql(left, right) } };
    }

    if (!std.mem.eql(u8, @tagName(left), @tagName(right))) {
        return Object{
            .error_ = .{
                .message = try std.fmt.allocPrint(self.arena.allocator(), "type mismatch: {s} {s} {s}", .{ @tagName(left), infix.operator, @tagName(right) }),
            },
        };
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: {s} {s} {s}", .{ @tagName(left), infix.operator, @tagName(right) }),
        },
    };
}

fn evalIfExpression(self: *Evaluator, if_: Expression.If, env: *Environment) anyerror!Object {
    const condition = try self.evalExpression(if_.condition.*, env);

    if (condition == .error_)
        return condition;

    if (condition.isTruthy()) {
        return try self.evalStatements(if_.consequence.statements, env);
    } else if (if_.alternative) |alternative| {
        return try self.evalStatements(alternative.statements, env);
    }

    return Object{ .null = .{} };
}

fn evalIdentExpression(self: *Evaluator, ident: Expression.Ident, env: *Environment) !Object {
    const ident_obj_from_env = env.get(ident.value);

    if (ident_obj_from_env) |ident_obj| {
        return ident_obj;
    }

    const ident_obj_from_builtin = Object.Builtin.get(ident.value);
    if (ident_obj_from_builtin) |ident_obj| {
        return ident_obj;
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "identifier not found: {s}", .{ident.value}),
        },
    };
}

fn evalCallExpression(self: *Evaluator, call: Expression.Call, env: *Environment) !Object {
    const args = try self.evalExpressions(call.arguments.items, env);

    if (args.items.len == 1 and args.items[0] == .error_) {
        return args.items[0];
    }

    return try self.applyFunction(call.function.*, args, env);
}

fn applyFunction(self: *Evaluator, function: Expression, args: ArrayList(Object), env: *Environment) anyerror!Object {
    const func_obj = try self.evalExpression(function, env);

    switch (func_obj) {
        .function => |func| {
            var enclosed_env = Environment.newEnclosedEnvironment(env);
            for (func.parameters.items, 0..) |param, arg_idx| {
                var arg = args.items[arg_idx];
                _ = try enclosed_env.set(param.value, &arg);
            }
            const evaluated = try self.evalStatements(func.body.statements, &enclosed_env);
            switch (evaluated) {
                .return_ => |return_| {
                    return return_.value.*;
                },
                else => {
                    return evaluated;
                },
            }
        },
        .builtin => |builtin| {
            return builtin.func(args.items, self.arena.allocator());
        },
        else => {
            std.debug.print("Not A function in applyFunction {s}\n", .{@tagName(function)});
            return error.ApplyFunctionNotAFunction;
        },
    }
}

fn evalFunctionExpression(function: Expression.Function, env: *Environment) !Object {
    const cloned = try function.clone(env.arena.allocator());
    return Object{
        .function = .{ .body = cloned.body, .parameters = cloned.parameters },
    };
}

fn evalIndexExpression(index: Expression.Index, env: *Environment) !Object {
    _ = index;
    _ = env;
    @panic("TODO");
}

fn evalIntegerInfixExpression(self: *Evaluator, operator: []const u8, left: Object.Integer, right: Object.Integer) !Object {
    if (std.mem.eql(u8, "+", operator)) {
        return Object{ .integer = .{ .value = left.value + right.value } };
    }

    if (std.mem.eql(u8, "-", operator)) {
        return Object{ .integer = .{ .value = left.value - right.value } };
    }

    if (std.mem.eql(u8, "*", operator)) {
        return Object{ .integer = .{ .value = left.value * right.value } };
    }

    if (std.mem.eql(u8, "/", operator)) {
        return Object{ .integer = .{ .value = @divExact(left.value, right.value) } }; // TODO: is divExact correct here? or @divFloor / @divTrunc
    }

    if (std.mem.eql(u8, "<", operator)) {
        return Object{ .boolean = .{ .value = left.value < right.value } };
    }

    if (std.mem.eql(u8, ">", operator)) {
        return Object{ .boolean = .{ .value = left.value > right.value } };
    }

    if (std.mem.eql(u8, "==", operator)) {
        return Object{ .boolean = .{ .value = left.value == right.value } };
    }

    if (std.mem.eql(u8, "!=", operator)) {
        return Object{ .boolean = .{ .value = left.value != right.value } };
    }

    return Object{
        .error_ = .{ .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: integer {s} integer", .{operator}) },
    };
}

fn evalStringInfixExpression(self: *Evaluator, operator: []const u8, left: Object.String, right: Object.String) !Object {
    if (std.mem.eql(u8, "+", operator)) {
        return Object{ .string = .{ .value = try std.mem.concat(self.arena.allocator(), u8, &[_][]const u8{ left.value, right.value }) } };
    }

    return Object{
        .error_ = .{ .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: integer {s} integer", .{operator}) },
    };
}

//
fn evalBangOperatorExpression(right: Object) !Object {
    switch (right) {
        .boolean => |boolean| {
            if (boolean.value) {
                return Object{ .boolean = .{ .value = false } };
            }
            return Object{ .boolean = .{ .value = true } };
        },
        .null => {
            return Object{ .boolean = .{ .value = true } };
        },
        .integer => {
            return Object{ .boolean = .{ .value = false } };
        },
        else => {
            return error.EvalBangOperatorExpressionNotImplementedYet;
        },
    }
}

fn evalMinusPrefixOperatorExpression(self: *Evaluator, right: Object) !Object {
    switch (right) {
        .integer => |integer| {
            return Object{ .integer = .{ .value = -integer.value } };
        },
        else => {
            return Object{
                .error_ = .{
                    .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: -{s}", .{@tagName(right)}),
                },
            };
        },
    }
}

fn testEvalToObject(alloc: std.mem.Allocator, evaluator: *Evaluator, input: []const u8) !Object {
    const lexer = Lexer.init(input);
    var parser = try Parser.init(alloc, lexer);
    defer parser.deinit();
    var ast_tree = try parser.parseTree(alloc);
    defer ast_tree.deinit();
    var env = Environment.init(alloc);
    defer env.deinit();

    return evaluator.evalTree(&ast_tree, &env);
}

fn testIntegerObject(expected: i64, actual: Object) !void {
    switch (actual) {
        .integer => |int_obj| {
            try testing.expectEqual(expected, int_obj.value);
        },
        else => {
            return error.NotAnIntegerObject;
        },
    }
}

fn testStringObject(expected: []const u8, actual: Object) !void {
    switch (actual) {
        .string => |str_obj| {
            try testing.expectEqualStrings(expected, str_obj.value);
        },
        else => {
            return error.NotAStringObject;
        },
    }
}

fn testNullObject(actual: Object) !void {
    switch (actual) {
        .null => {
            return; // We are a null object!
        },
        else => {
            return error.NotANullObject;
        },
    }
}

fn testBooleanObject(expected: bool, actual: Object) !void {
    switch (actual) {
        .boolean => |bool_obj| {
            try testing.expectEqual(expected, bool_obj.value);
        },
        else => {
            return error.NotAnBooleanObject;
        },
    }
}

fn testFunctionObject(alloc: std.mem.Allocator, evaluator: *Evaluator, expected: []const u8, actual: []const u8) !void {
    const lexer = Lexer.init(actual);
    var parser = try Parser.init(alloc, lexer);
    defer parser.deinit();
    var ast_tree = try parser.parseTree(alloc);
    defer ast_tree.deinit();
    var env = Environment.init(alloc);
    defer env.deinit();

    const evaluated = try evaluator.evalTree(&ast_tree, &env);

    switch (evaluated) {
        .function => {
            const func_str = try std.json.stringifyAlloc(alloc, evaluated, .{ .whitespace = .indent_2 });
            defer alloc.free(func_str);
            try testing.expectEqualStrings(expected, func_str);
        },
        else => {
            return error.NotAFunctionObject;
        },
    }
}

test "Eval Integer Expression" {
    const tests = .{
        .{
            "5",
            5,
        },
        .{
            "10",
            10,
        },
        .{
            "-5",
            -5,
        },
        .{
            "-10",
            -10,
        },
        .{
            "5 + 5 + 5 + 5 - 10",
            10,
        },
        .{
            "2 * 2 * 2 * 2 * 2",
            32,
        },
        .{
            "-50 + 100 + -50",
            0,
        },
        .{
            "5 * 2 + 10",
            20,
        },
        .{
            "5 + 2 * 10",
            25,
        },
        .{
            "20 + 2 * -10",
            0,
        },
        .{
            "50 / 2 * 2 + 10",
            60,
        },
        .{
            "2 * (5 + 10)",
            30,
        },
        .{
            "3 * 3 * 3 + 10",
            37,
        },
        .{
            "3 * (3 * 3) + 10",
            37,
        },
        .{
            "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            50,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        try testIntegerObject(test_item[1], evaluated_ast);
    }
}

test "Eval String Expression" {
    const tests = .{
        .{
            "\"foo\"",
            "foo",
        },
        .{
            "\"hello world\"",
            "hello world",
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        try testStringObject(test_item[1], evaluated_ast);
    }
}

test "Eval Boolean Expression" {
    const tests = .{
        .{
            "true",
            true,
        },
        .{
            "false",
            false,
        },
        .{
            "1 < 2",
            true,
        },
        .{
            "1 > 2",
            false,
        },
        .{
            "1 < 1",
            false,
        },
        .{
            "1 > 1",
            false,
        },
        .{
            "1 == 1",
            true,
        },
        .{
            "1 != 1",
            false,
        },
        .{
            "1 == 2",
            false,
        },
        .{
            "1 != 2",
            true,
        },
        .{
            "true == true",
            true,
        },
        .{
            "false == false",
            true,
        },
        .{
            "true == false",
            false,
        },
        .{
            "true != false",
            true,
        },
        .{
            "false != true",
            true,
        },
        .{
            "(1 < 2) == true",
            true,
        },
        .{
            "(1 < 2) == false",
            false,
        },
        .{
            "(1 > 2) == true",
            false,
        },
        .{
            "(1 > 2) == false",
            true,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();
        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        testBooleanObject(test_item[1], evaluated_ast) catch |err| {
            std.debug.print("\nfailed at comparing: {s} with {any}\n", .{ test_item[0], evaluated_ast });
            return err;
        };
    }
}

test "Bang Operator" {
    const tests = .{
        .{
            "!true",
            false,
        },
        .{
            "!false",
            true,
        },
        .{
            "!5",
            false,
        },
        .{
            "!!true",
            true,
        },
        .{
            "!!false",
            false,
        },
        .{
            "!!5",
            true,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        try testBooleanObject(test_item[1], evaluated_ast);
    }
}

test "Eval If Else Expression" {
    const tests = .{
        .{
            "if (true) { 10 }",
            10,
        },
        .{
            "if (false) { 10 }",
            null,
        },
        .{
            "if (1) { 10 }",
            10,
        },
        .{
            "if (1 < 2) { 10 }",
            10,
        },
        .{
            "if (1 > 2) { 10 }",
            null,
        },
        .{
            "if (1 > 2) { 10 } else { 20 }",
            20,
        },
        .{
            "if (1 < 2) { 10 } else { 20 }",
            10,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);

        switch (@TypeOf(test_item[1])) {
            comptime_int => {
                try testIntegerObject(test_item[1], evaluated_ast);
            },
            @TypeOf(null) => {
                try testNullObject(evaluated_ast);
            },
            else => {
                std.debug.print("{any}", .{@TypeOf(test_item[1])});
                return error.TestEvalIfExpressionTypeNotImplemented;
            },
        }
    }
}

test "Return Statements" {
    const tests = .{
        .{
            "return 10;",
            10,
        },
        .{
            "return 10; 9;",
            10,
        },
        .{
            "return 2 * 5; 9;",
            10,
        },
        .{
            "9; return 2 * 5; 9;",
            10,
        },
        .{
            \\if (10 > 1) {
            \\  if (10 > 1) {
            \\    return 10;
            \\  }
            \\  return 1;
            \\}
            ,
            10,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);

        try testIntegerObject(test_item[1], evaluated_ast);
    }
}

test "Error Handling" {
    const tests = .{
        .{
            "5 + true;",
            "type mismatch: integer + boolean",
        },
        .{
            "5 + true; 5;",
            "type mismatch: integer + boolean",
        },
        .{
            "-true",
            "unknown operator: -boolean",
        },
        .{
            "true + false",
            "unknown operator: boolean + boolean",
        },
        .{
            "5; true + false; 5",
            "unknown operator: boolean + boolean",
        },
        .{
            "if (10 > 1) { true + false; }",
            "unknown operator: boolean + boolean",
        },
        .{
            "foobar",
            "identifier not found: foobar",
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);

        if (evaluated_ast != .error_) {
            std.debug.print("DEBUG {any}", .{evaluated_ast});
            return error.TestErrorHandlingNoErrorObjectReturned;
        }

        try testing.expectEqualStrings(test_item[1], evaluated_ast.error_.message);
    }
}

test "DeclareAssign Statments" {
    const tests = .{
        .{
            "a := 5; a;",
            5,
        },
        .{
            "a := 5 * 5; a;",
            25,
        },
        .{
            "a := 5; b := a; b;",
            5,
        },
        .{
            "a := 5; b := a; c := a + b + 5; c;",
            15,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);

        try testIntegerObject(test_item[1], evaluated_ast);
    }
}

test "Function Object" {
    const tests = .{
        .{
            "fn(x) { x + 2; };",
            \\{
            \\  "type": "function",
            \\  "parameters": [
            \\    "x"
            \\  ],
            \\  "body": {
            \\    "type": "block",
            \\    "statements": [
            \\      {
            \\        "type": "expression_statement",
            \\        "expression": {
            \\          "type": "infix",
            \\          "operator": "+",
            \\          "left": {
            \\            "type": "ident",
            \\            "value": "x"
            \\          },
            \\          "right": {
            \\            "type": "literal",
            \\            "value": 2
            \\          }
            \\        }
            \\      }
            \\    ]
            \\  }
            \\}
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        try testFunctionObject(testing.allocator, &evaluator, test_item[1], test_item[0]);
    }
}

test "Function Application" {
    const tests = .{
        .{
            "identity := fn(x) { x; }; identity(5);",
            5,
        },
        .{
            "identity := fn(x) { return x; }; identity(5);",
            5,
        },
        .{
            "double := fn(x) { x * 2; }; double(5);",
            10,
        },
        .{
            "add := fn(x, y) { x + y; }; add(5, 5);",
            10,
        },
        .{
            "add := fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            20,
        },
        .{
            "fn(x) { x; }(5)",
            5,
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        try testIntegerObject(test_item[1], evaluated_ast);
    }
}

test "String Concatenation" {
    const tests = .{
        .{
            \\"Hello" + " " + "World";
            ,
            "Hello World",
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        try testStringObject(test_item[1], evaluated_ast);
    }
}

test "Builtin Function" {
    const tests = .{
        .{
            \\len("")
            ,
            0,
        },
        .{
            \\len("four")
            ,
            4,
        },
        .{
            \\len("hello world")
            ,
            11,
        },
        .{
            \\len(1)
            ,
            @as([]const u8, "argument to 'len' not supported, got integer"),
        },
    };

    inline for (tests) |test_item| {
        var evaluator = Evaluator.init(testing.allocator);
        defer evaluator.deinit();

        const evaluated_ast = try testEvalToObject(testing.allocator, &evaluator, test_item[0]);
        switch (@TypeOf(test_item[1])) {
            comptime_int => {
                try testIntegerObject(test_item[1], evaluated_ast);
            },
            []const u8 => {
                try testing.expectEqualStrings(test_item[1], evaluated_ast.error_.message);
            },
            else => @panic(std.fmt.comptimePrint("Builtin Function test type unsupported: {s}", .{@typeName(@TypeOf(test_item[1]))})),
        }
    }
}
