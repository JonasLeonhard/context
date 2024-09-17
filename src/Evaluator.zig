const std = @import("std");
const testing = std.testing;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");

pub const Object = union(enum) {
    integer: Integer,
    boolean: *const Boolean,
    null: *const Null,

    testing: Integer, // TODO: remove

    const Integer = struct {
        value: i64,
    };

    const Boolean = struct {
        value: bool,
    };

    const Null = struct {};

    pub fn toString(self: Object, alloc: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .integer => |int| {
                return try std.fmt.allocPrint(alloc, "{d}", .{int.value});
            },
            .boolean => |boolean| {
                return try std.fmt.allocPrint(alloc, "{any}", .{boolean.value});
            },
            .null => {
                return try std.fmt.allocPrint(alloc, "{any}", .{null});
            },
            else => {
                return "toString not implemented yet";
            },
        }
    }
};

// To avoid copies of true and false values, we create them once.
const True = &Object.Boolean{ .value = true };
const False = &Object.Boolean{ .value = false };
const Null_Global = &Object.Null{};

const Evaluator = @This();

const errors = error{
    EvalNodeTypeNotImplementedYet,
    EvalLiteralValueNotImplementedYet,
    EvalStatmentsNoStatmentsGiven,
    EvalPrefixExpressionOperatorNotImlementedYet,
    EvalBangOperatorExpressionNotImplementedYet,
    EvalMinusPrefixOperatorExpressionNotImplementedYet,
    EvalInfixExpressionNotImplemented,
    EvalIntegerInfixExpressionUndefined,
};

pub fn eval(ast_tree: *ast.Tree, node: ast.Node) errors!Object {
    switch (node) {
        .root => |root| {
            return evalStatements(ast_tree, root.statements);
        },
        .expression => |expression| {
            if (expression.expr) |expr| {
                return eval(ast_tree, ast_tree.nodes.items[expr]);
            }
            return Object{ .null = Null_Global };
        },
        .prefix => |prefix| {
            const right = try eval(ast_tree, ast_tree.nodes.items[prefix.right]);
            return evalPrefixExpression(prefix.operator, right);
        },
        .infix => |infix| {
            const left = try eval(ast_tree, ast_tree.nodes.items[infix.left]);
            const right = try eval(ast_tree, ast_tree.nodes.items[infix.right]);
            return try evalInfixExpression(infix.operator, left, right);
        },
        .block => |block| {
            return try evalStatements(ast_tree, block.statements);
        },
        .if_ => |if_| {
            return try evalIfExpression(ast_tree, if_);
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
        else => {
            std.debug.print("EvalNodeTypeNotImplementedYet for {any}\n", .{node});
            return errors.EvalNodeTypeNotImplementedYet;
        },
    }
}

fn evalStatements(ast_tree: *ast.Tree, statements: []const ast.NodeIndex) !Object {
    for (statements) |statement| {
        return try eval(ast_tree, ast_tree.nodes.items[statement]);
    }

    return errors.EvalStatmentsNoStatmentsGiven;
}

fn evalIfExpression(ast_tree: *ast.Tree, if_: ast.Node.If) !Object {
    const condition = try eval(ast_tree, ast_tree.nodes.items[if_.condition]);

    if (isTruthy(condition)) {
        return eval(ast_tree, ast_tree.nodes.items[if_.consequence]);
    } else if (if_.alternative) |alternative| {
        return eval(ast_tree, ast_tree.nodes.items[alternative]);
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

fn evalPrefixExpression(operator: []const u8, right: Object) !Object {
    if (std.mem.eql(u8, operator, "!")) {
        return try evalBangOperatorExpression(right);
    }

    if (std.mem.eql(u8, operator, "-")) {
        return try evalMinusPrefixOperatorExpression(right);
    }

    return error.EvalPrefixExpressionOperatorNotImlementedYet;
}

fn evalInfixExpression(operator: []const u8, left: Object, right: Object) !Object {
    if (left == .integer and right == .integer) {
        return evalIntegerInfixExpression(operator, left.integer, right.integer);
    }

    if (std.mem.eql(u8, operator, "==")) {
        return Object{ .boolean = nativeBoolToBooleanObject(std.meta.eql(left, right)) };
    }

    if (std.mem.eql(u8, operator, "!=")) {
        return Object{ .boolean = nativeBoolToBooleanObject(!std.meta.eql(left, right)) };
    }

    return errors.EvalInfixExpressionNotImplemented;
}

fn evalIntegerInfixExpression(operator: []const u8, left: Object.Integer, right: Object.Integer) !Object {
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

    return errors.EvalIntegerInfixExpressionUndefined;
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

fn evalMinusPrefixOperatorExpression(right: Object) !Object {
    switch (right) {
        .integer => |integer| {
            return Object{ .integer = .{ .value = -integer.value } };
        },
        else => {
            return errors.EvalMinusPrefixOperatorExpressionNotImplementedYet;
        },
    }
}

fn nativeBoolToBooleanObject(value: bool) *const Object.Boolean {
    return if (value) True else False;
}

fn testEvalToObject(alloc: std.mem.Allocator, input: []const u8) !Object {
    const lexer = Lexer.init(input);
    var parser = try Parser.init(alloc, lexer);
    defer parser.deinit();
    var ast_tree = try parser.parseTree(alloc);
    defer ast_tree.deinit();

    if (ast_tree.root) |root_idx| {
        return eval(&ast_tree, ast_tree.nodes.items[root_idx]);
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
        const evaluated_ast = try testEvalToObject(testing.allocator, test_item[0]);
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
        const evaluated_ast = try testEvalToObject(testing.allocator, test_item[0]);
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
        const evaluated_ast = try testEvalToObject(testing.allocator, test_item[0]);
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
        const evaluated_ast = try testEvalToObject(testing.allocator, test_item[0]);

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
