const std = @import("std");

pub const Tree = struct {
    alloc: std.mem.Allocator,
    nodes: std.MultiArrayList(Node),

    pub fn init(allocator: std.mem.Allocator) Tree {
        return .{
            .alloc = allocator,
            .nodes = std.MultiArrayList(Node){},
        };
    }

    pub fn deinit(self: *Tree) void {
        self.nodes.deinit(self.alloc);
    }

    pub fn addNode(self: *Tree, node: Node) !NodeIndex {
        try self.nodes.append(self.alloc, node);
        return @intCast(self.nodes.len - 1);
    }

    pub fn toString(self: *const Tree, alloc: std.mem.Allocator, node_index: NodeIndex) ![]u8 {
        var list = std.ArrayList(u8).init(alloc);
        errdefer list.deinit();

        try self.printNode(node_index, 0, list.writer());
        return list.toOwnedSlice();
    }

    fn printNode(self: *const Tree, node_index: NodeIndex, indent: u32, writer: anytype) !void {
        const node = self.nodes.get(node_index);

        try writer.writeByteNTimes(' ', indent * 2);
        try writer.print("{s}\n", .{@tagName(node)});

        switch (node) {
            .root => {
                for (node.root.statements) |stmt| {
                    try printNode(self, stmt, indent + 1, writer);
                }
            },
            .function_declaration => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("name: {s}\n", .{node.function_declaration.name});
                try printNode(self, node.function_declaration.body, indent + 1, writer);
            },
            .declare_assign => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("name: {s}\n", .{node.declare_assign.name});
                try printNode(self, node.declare_assign.expr, indent + 1, writer);
            },
            .binary_expression => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("op: {s}\n", .{@tagName(node.binary_expression.op)});
                try printNode(self, node.binary_expression.left, indent + 1, writer);
                try printNode(self, node.binary_expression.right, indent + 1, writer);
            },
            .unary_expression => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("op: {s}\n", .{@tagName(node.unary_expression.op)});
                try printNode(self, node.unary_expression.operand, indent + 1, writer);
            },
            .literal => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                switch (node.literal) {
                    .int => |value| try writer.print("int: {d}\n", .{value}),
                    .float => |value| try writer.print("float: {d}\n", .{value}),
                    .string => |value| try writer.print("string: {s}\n", .{value}),
                }
            },
        }
    }
};

pub const NodeIndex = u32;

pub const Node = union(enum) {
    root: Root,
    function_declaration: FunctionDeclaration,
    declare_assign: DeclareAssign,
    binary_expression: BinaryExpression,
    unary_expression: UnaryExpression,
    literal: Literal,

    pub const Root = struct {
        statements: []const NodeIndex,
    };

    pub const FunctionDeclaration = struct {
        name: []const u8,
        body: NodeIndex,
    };

    pub const DeclareAssign = struct {
        name: []const u8,
        expr: NodeIndex,
    };

    pub const BinaryExpression = struct {
        left: NodeIndex,
        right: NodeIndex,
        op: BinaryOp,
    };

    pub const UnaryExpression = struct {
        operand: NodeIndex,
        op: UnaryOp,
    };

    pub const Literal = union(enum) {
        int: i64,
        float: f64,
        string: []const u8,
    };

    pub const BinaryOp = enum { add, subtract, multiply, divide };
    pub const UnaryOp = enum { negate, not };
};

test "AstTree" {
    var tree = Tree.init(std.testing.allocator);
    defer tree.deinit();

    // Create a simple AST: x := 5 + 3;
    const literal_5 = try tree.addNode(.{ .literal = .{ .int = 5 } });
    const literal_3 = try tree.addNode(.{ .literal = .{ .int = 3 } });

    const binary_expr = try tree.addNode(.{
        .binary_expression = .{
            .left = literal_5,
            .right = literal_3,
            .op = .add,
        },
    });

    const declare_assign = try tree.addNode(.{
        .declare_assign = .{
            .name = "x",
            .expr = binary_expr,
        },
    });

    const root = try tree.addNode(.{
        .root = .{ .statements = &.{declare_assign} },
    });

    // Print the tree structure
    const tree_string = try tree.toString(std.testing.allocator, root);
    defer std.testing.allocator.free(tree_string);

    // std.debug.print("tree_string {s}", .{tree_string});

    const expected_output =
        \\root
        \\  declare_assign
        \\    name: x
        \\    binary_expression
        \\      op: add
        \\      literal
        \\        int: 5
        \\      literal
        \\        int: 3
        \\
    ;

    try std.testing.expectEqualStrings(expected_output, tree_string);
}
