const std = @import("std");
const testing = std.testing;
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const ast = @import("ast.zig");

pub const Object = union(enum) {
    integer: Integer,
    booleal: Boolean,
    null: Null,

    const Integer = struct {
        value: i64,
    };

    const Boolean = struct {
        value: bool,
    };

    const Null = struct {};
};

const Evaluator = @This();

const errors = error{
    EvalNodeTypeNotImplementedYet,
    EvalLiteralValueNotImplementedYet,
    EvalStatmentsNoStatmentsGiven,
};

fn eval(ast_tree: *ast.Tree, node: ast.Node) errors!Object {
    switch (node) {
        .root => |root| {
            return evalStatements(ast_tree, root.statements);
        },
        .expression => |expression| {
            if (expression.expr) |expr| {
                return eval(ast_tree, ast_tree.nodes.items[expr]);
            }
            return Object{ .null = .{} };
        },
        .literal => |literal| {
            switch (literal.value) {
                .int => |int_val| {
                    return Object{
                        .integer = .{ .value = int_val },
                    };
                },
                else => {
                    return errors.EvalLiteralValueNotImplementedYet;
                },
            }
        },
        else => {
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

fn testEvalToTree(alloc: std.mem.Allocator, input: []const u8) !Object {
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
    };

    inline for (tests) |test_item| {
        const evaluated_ast = try testEvalToTree(testing.allocator, test_item[0]);
        try testIntegerObject(test_item[1], evaluated_ast);
    }
}
