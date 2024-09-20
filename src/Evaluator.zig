const std = @import("std");
const testing = std.testing;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Object = @import("object.zig").Object;
const Environment = @import("Environment.zig");
const ast = @import("ast.zig");

// To avoid copies of true and false values, we create them once.
const True = &Object.Boolean{ .value = true };
const False = &Object.Boolean{ .value = false };
const Null_Global = &Object.Null{};

const Evaluator = @This();

arena: std.heap.ArenaAllocator,

const errors = error{
    EvalNodeTypeNotImplementedYet,
    EvalLiteralValueNotImplementedYet,
    EvalStatmentsNoStatmentsGiven,
    EvalPrefixExpressionOperatorNotImlementedYet,
    EvalBangOperatorExpressionNotImplementedYet,
    EvalMinusPrefixOperatorExpressionNotImplementedYet,
    EvalInfixExpressionNotImplemented,
    EvalIntegerInfixExpressionUndefined,
    GotWrongNodeTypeFromAstTree,
} || std.fmt.AllocPrintError;

pub fn init(alloc: std.mem.Allocator) Evaluator {
    return Evaluator{
        .arena = std.heap.ArenaAllocator.init(alloc),
    };
}

pub fn deinit(self: Evaluator) void {
    self.arena.deinit();
}

pub fn eval(self: *Evaluator, ast_tree: *ast.Tree, env: *Environment, node: ast.Node) errors!Object {
    switch (node) {
        .root => |root| {
            return try self.evalProgram(ast_tree, env, root.statements);
        },
        .expression => |expression| {
            if (expression.expr) |expr| {
                return try self.eval(ast_tree, env, ast_tree.nodes.items[expr]);
            }
            return Object{ .null = Null_Global };
        },
        .prefix => |prefix| {
            const right = try self.eval(ast_tree, env, ast_tree.nodes.items[prefix.right]);

            if (right == .error_) {
                return right;
            }

            return self.evalPrefixExpression(prefix.operator, right);
        },
        .infix => |infix| {
            const left = try self.eval(ast_tree, env, ast_tree.nodes.items[infix.left]);
            if (left == .error_) {
                return left;
            }

            const right = try self.eval(ast_tree, env, ast_tree.nodes.items[infix.right]);
            if (right == .error_) {
                return right;
            }

            return try evalInfixExpression(self, infix.operator, left, right);
        },
        .block => |block| {
            return try self.evalBlockStatment(ast_tree, env, block);
        },
        .if_ => |if_| {
            return try self.evalIfExpression(ast_tree, env, if_);
        },
        .return_ => |return_| {
            if (return_.expr) |expr| {
                const value = try self.eval(ast_tree, env, ast_tree.nodes.items[expr]);

                if (value == .error_) {
                    return value;
                }

                return Object{ .return_ = .{ .value = &value } };
            }
            return Object{ .null = Null_Global };
        },
        .declare_assign => |declare_assign| {
            if (declare_assign.expr) |expr| {
                const value = try self.eval(ast_tree, env, ast_tree.nodes.items[expr]);
                if (value == .error_) {
                    return value;
                }

                const ident = ast_tree.nodes.items[declare_assign.ident];
                _ = try env.set(ident.ident.value, value);
            }

            return Object{ .null = Null_Global };
        },
        .literal => |literal| {
            switch (literal.value) {
                .int => |int_val| {
                    return Object{
                        .integer = .{ .value = int_val },
                    };
                },
                .boolean => |bool_val| {
                    return Object{ .boolean = nativeBoolToBooleanObject(bool_val) };
                },
                .null => {
                    return Object{ .null = Null_Global };
                },
                else => {
                    std.debug.print("EvalLiteralValueNotImplementedYet for {any}\n", .{literal});
                    return errors.EvalLiteralValueNotImplementedYet;
                },
            }
        },
        .ident => |ident| {
            return try self.evalIdentifier(env, ident);
        },
        else => {
            std.debug.print("EvalNodeTypeNotImplementedYet for {any}\n", .{node});
            return errors.EvalNodeTypeNotImplementedYet;
        },
    }
}

fn evalProgram(self: *Evaluator, ast_tree: *ast.Tree, env: *Environment, statements: []const ast.NodeIndex) !Object {
    var result: Object = Object{ .null = Null_Global };
    for (statements) |statement| {
        result = try self.eval(ast_tree, env, ast_tree.nodes.items[statement]);

        if (result == .return_) {
            return result.return_.value.*;
        }
        if (result == .error_) {
            return result;
        }
    }

    return result;
}

fn evalBlockStatment(self: *Evaluator, ast_tree: *ast.Tree, env: *Environment, block: ast.Node.Block) !Object {
    var result: Object = Object{ .null = Null_Global };

    for (block.statements) |statement| {
        result = try self.eval(ast_tree, env, ast_tree.nodes.items[statement]);

        if (result == .return_ or result == .error_) {
            return result;
        }
    }

    return result;
}

fn evalIfExpression(self: *Evaluator, ast_tree: *ast.Tree, env: *Environment, if_: ast.Node.If) !Object {
    const condition = try self.eval(ast_tree, env, ast_tree.nodes.items[if_.condition]);
    if (condition == .error_) {
        return condition;
    }

    if (isTruthy(condition)) {
        return self.eval(ast_tree, env, ast_tree.nodes.items[if_.consequence]);
    } else if (if_.alternative) |alternative| {
        return self.eval(ast_tree, env, ast_tree.nodes.items[alternative]);
    } else {
        return Object{ .null = Null_Global };
    }
}

fn isTruthy(obj: Object) bool {
    switch (obj) {
        .null => {
            return false;
        },
        .boolean => |boolean| {
            return boolean.value;
        },
        else => {
            return true;
        },
    }
}

fn evalPrefixExpression(self: *Evaluator, operator: []const u8, right: Object) !Object {
    if (std.mem.eql(u8, operator, "!")) {
        return try evalBangOperatorExpression(right);
    }

    if (std.mem.eql(u8, operator, "-")) {
        return try self.evalMinusPrefixOperatorExpression(right);
    }

    return Object{
        .error_ = .{
            .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: {s}{s}", .{ operator, @tagName(right) }),
        },
    };
}

fn evalInfixExpression(self: *Evaluator, operator: []const u8, left: Object, right: Object) !Object {
    if (left == .integer and right == .integer) {
        return self.evalIntegerInfixExpression(operator, left.integer, right.integer);
    }

    if (std.mem.eql(u8, operator, "==")) {
        return Object{ .boolean = nativeBoolToBooleanObject(std.meta.eql(left, right)) };
    }

    if (std.mem.eql(u8, operator, "!=")) {
        return Object{ .boolean = nativeBoolToBooleanObject(!std.meta.eql(left, right)) };
    }

    if (!std.mem.eql(u8, @tagName(left), @tagName(right))) {
        return Object{
            .error_ = .{
                .message = try std.fmt.allocPrint(self.arena.allocator(), "type mismatch: {s} {s} {s}", .{ @tagName(left), operator, @tagName(right) }),
            },
        };
    }

    return Object{ .error_ = .{
        .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: {s} {s} {s}", .{ @tagName(left), operator, @tagName(right) }),
    } };
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
        return Object{ .boolean = nativeBoolToBooleanObject(left.value < right.value) };
    }

    if (std.mem.eql(u8, ">", operator)) {
        return Object{ .boolean = nativeBoolToBooleanObject(left.value > right.value) };
    }

    if (std.mem.eql(u8, "==", operator)) {
        return Object{ .boolean = nativeBoolToBooleanObject(left.value == right.value) };
    }

    if (std.mem.eql(u8, "!=", operator)) {
        return Object{ .boolean = nativeBoolToBooleanObject(left.value != right.value) };
    }

    return Object{
        .error_ = .{ .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: integer {s} integer", .{operator}) },
    };
}

fn evalBangOperatorExpression(right: Object) !Object {
    switch (right) {
        .boolean => |boolean| {
            if (boolean.value) {
                return Object{ .boolean = False };
            }
            return Object{ .boolean = True };
        },
        .null => {
            return Object{ .boolean = True };
        },
        .integer => {
            return Object{ .boolean = False };
        },
        else => {
            return errors.EvalBangOperatorExpressionNotImplementedYet;
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
                .error_ = .{ .message = try std.fmt.allocPrint(self.arena.allocator(), "unknown operator: -{s}", .{@tagName(right)}) },
            };
        },
    }
}

fn evalIdentifier(self: *Evaluator, env: *Environment, node: ast.Node.Ident) !Object {
    const value = env.get(node.value);
    if (value == null) {
        return Object{
            .error_ = .{ .message = try std.fmt.allocPrint(self.arena.allocator(), "identifier not found: {s}", .{node.value}) },
        };
    }
    return value.?;
}

fn nativeBoolToBooleanObject(value: bool) *const Object.Boolean {
    return if (value) True else False;
}

fn testEvalToObject(alloc: std.mem.Allocator, evaluator: *Evaluator, input: []const u8) !Object {
    const lexer = Lexer.init(input);
    var parser = try Parser.init(alloc, lexer);
    defer parser.deinit();
    var ast_tree = try parser.parseTree(alloc);
    defer ast_tree.deinit();
    var env = Environment.init(alloc);
    defer env.deinit();

    if (ast_tree.root) |root_idx| {
        return evaluator.eval(&ast_tree, &env, ast_tree.nodes.items[root_idx]);
    } else {
        return error.NoAstRootNode;
    }
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
