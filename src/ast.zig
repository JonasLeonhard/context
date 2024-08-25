const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

pub const Tree = struct {
    arena: ArenaAllocator,
    /// The root node is expected to be the last node added to the nodes array list! => tree.nodes.len - 1
    nodes: ArrayList(Node), // @Performance: zigs compiler uses a MultiArrayList(Node) here
    root: ?NodeIndex,

    pub fn init(alloc: std.mem.Allocator) Tree {
        const arena = ArenaAllocator.init(alloc);

        return .{
            .arena = arena,
            .nodes = std.ArrayList(Node).init(alloc),
            .root = null,
        };
    }

    pub fn deinit(self: *Tree) void {
        self.nodes.deinit();
        self.arena.deinit();
    }

    pub fn addNode(self: *Tree, node: Node) !NodeIndex {
        try self.nodes.append(node);

        const node_index: NodeIndex = @intCast(self.nodes.items.len - 1);

        if (node == .root) {
            self.root = node_index;
        }

        return node_index;
    }

    pub fn toString(self: *const Tree, alloc: std.mem.Allocator) ![]u8 {
        var list = std.ArrayList(u8).init(alloc);
        errdefer list.deinit();

        if (self.root) |root| {
            try self.printNode(root, 0, list.writer());
        }

        return list.toOwnedSlice();
    }

    fn printNode(self: *const Tree, node_index: NodeIndex, indent: u32, writer: anytype) !void {
        const node = self.nodes.items[node_index];

        try writer.writeByteNTimes(' ', indent * 2);
        try writer.print("{s}\n", .{@tagName(node)});

        switch (node) {
            .root => {
                for (node.root.statements) |stmt| {
                    try self.printNode(stmt, indent + 1, writer);
                }
            },
            // ---------- Statements ---------
            .return_ => {
                if (node.return_.expr) |expr| {
                    try self.printNode(expr, indent + 1, writer);
                }
            },
            .declare_assign => {
                try self.printNode(node.declare_assign.ident, indent + 1, writer);
                if (node.declare_assign.expr) |expr| {
                    try self.printNode(expr, indent + 1, writer);
                }
            },
            .expression => {
                if (node.expression.expr) |expr| {
                    try self.printNode(expr, indent + 1, writer);
                }
            },
            // ---------- Expressions --------
            .ident => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("ident: {s}\n", .{node.ident.value});
            },
            .infix => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("operator: {s}\n", .{node.infix.operator});
                try self.printNode(node.infix.left, indent + 1, writer);
                try self.printNode(node.infix.right, indent + 1, writer);
            },
            .prefix => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                try writer.print("op: {s}\n", .{node.prefix.operator});
                try self.printNode(node.prefix.right, indent + 1, writer);
            },
            .literal => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                switch (node.literal.value) {
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

    // ---------- Statements ---------
    return_: Return,
    declare_assign: DeclareAssign,
    expression: Expression,

    // ---------- Expressions --------
    ident: Ident,
    infix: Infix,
    prefix: Prefix,
    literal: Literal,

    pub const Root = struct {
        statements: []const NodeIndex,
    };

    // ---------- Statements ---------
    pub const Return = struct {
        token: Token,
        expr: ?NodeIndex,
    };

    pub const DeclareAssign = struct {
        token: Token,
        ident: NodeIndex,
        expr: ?NodeIndex,
    };

    pub const Expression = struct {
        token: Token,
        expr: ?NodeIndex,
    };

    // ---------- Expressions --------
    pub const Ident = struct {
        token: Token,
        value: []const u8,
    };

    pub const Infix = struct {
        token: Token,
        left: NodeIndex,
        operator: []const u8,
        right: NodeIndex,
    };

    pub const Prefix = struct {
        token: Token,
        operator: []const u8,
        right: NodeIndex,
    };

    pub const Literal = struct {
        token: Token,
        value: LiteralValue,
    };

    const LiteralValue = union(enum) {
        int: i64,
        float: f64,
        string: []const u8,
    };
};

test "AstTree" {
    var tree = Tree.init(std.testing.allocator);
    defer tree.deinit();

    // Create a simple AST: x := 5 + 3;
    const literal_5 = try tree.addNode(
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
    const literal_3 = try tree.addNode(
        .{
            .literal = .{
                .token = Token{
                    .type = .{
                        .Literal = .Int,
                    },
                    .literal = "3",
                    .location = null,
                },
                .value = .{
                    .int = 3,
                },
            },
        },
    );

    const infix = try tree.addNode(.{
        .infix = .{
            .token = Token{
                .type = .Plus,
                .literal = "+",
                .location = null,
            },
            .left = literal_5,
            .operator = "+",
            .right = literal_3,
        },
    });

    const ident = try tree.addNode(.{
        .ident = .{
            .token = Token{
                .type = .Ident,
                .literal = "x",
                .location = null,
            },
            .value = "x",
        },
    });

    const declare_assign = try tree.addNode(.{
        .declare_assign = .{
            .token = Token{
                .type = .Ident,
                .literal = "x",
                .location = null,
            },
            .ident = ident,
            .expr = infix,
        },
    });

    _ = try tree.addNode(.{
        .root = .{ .statements = &.{declare_assign} },
    });

    // Print the tree structure
    const tree_string = try tree.toString(std.testing.allocator);
    defer std.testing.allocator.free(tree_string);

    // std.debug.print("tree_string {s}", .{tree_string});

    const expected_output =
        \\root
        \\  declare_assign
        \\    ident
        \\      ident: x
        \\    infix
        \\      operator: +
        \\      literal
        \\        int: 5
        \\      literal
        \\        int: 3
        \\
    ;

    try std.testing.expectEqualStrings(expected_output, tree_string);
}
