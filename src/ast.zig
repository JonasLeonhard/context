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

    pub fn init(gpa: std.mem.Allocator) Tree {
        const arena = ArenaAllocator.init(gpa);

        return .{
            .arena = arena,
            .nodes = std.ArrayList(Node).init(gpa),
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

    pub fn toString(self: *const Tree, gpa: std.mem.Allocator) ![]u8 {
        var list = std.ArrayList(u8).init(gpa);
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
            .block => {
                for (node.block.statements) |stmt| {
                    try self.printNode(stmt, indent + 1, writer);
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
            .function => {
                for (node.function.parameters) |param| {
                    try self.printNode(param, indent + 1, writer);
                }
                try self.printNode(node.function.body, indent + 1, writer);
            },
            .call => {
                for (node.call.arguments) |argument| {
                    try self.printNode(argument, indent + 1, writer);
                }
                try self.printNode(node.call.function, indent + 1, writer);
            },
            .literal => {
                try writer.writeByteNTimes(' ', (indent + 1) * 2);
                switch (node.literal.value) {
                    .int => |value| try writer.print("int: {d}\n", .{value}),
                    .boolean => |value| try writer.print("boolean: {any}\n", .{value}),
                    .null => |_| try writer.print("null: null\n", .{}),
                    .float => |value| try writer.print("float: {d}\n", .{value}),
                    .string => |value| try writer.print("string: {s}\n", .{value}),
                }
            },
            .if_ => {
                try self.printNode(node.if_.condition, indent + 1, writer);
                try self.printNode(node.if_.consequence, indent + 1, writer);
                if (node.if_.alternative) |alternative| {
                    try self.printNode(alternative, indent + 1, writer);
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
    block: Block,

    // ---------- Expressions --------
    ident: Ident,
    infix: Infix,
    prefix: Prefix,
    literal: Literal,
    if_: If,
    function: Function,
    call: Call,

    pub const Root = struct {
        /// Statements
        statements: []const NodeIndex,
    };

    // ---------- Statements ---------
    pub const Return = struct {
        token: Token,
        /// Expression
        expr: ?NodeIndex,
    };

    pub const DeclareAssign = struct {
        token: Token,
        ident: NodeIndex,
        /// Expression
        expr: ?NodeIndex,
    };

    pub const Expression = struct {
        token: Token,
        /// Expression
        expr: ?NodeIndex,
    };

    pub const Block = struct {
        token: Token,
        /// Statements
        statements: []const NodeIndex,
    };

    // ---------- Expressions --------
    pub const Ident = struct {
        token: Token,
        value: []const u8,
    };

    pub const Infix = struct {
        token: Token,
        /// Expression
        left: NodeIndex,
        operator: []const u8,
        /// Expression
        right: NodeIndex,
    };

    pub const Prefix = struct {
        token: Token,
        operator: []const u8,
        /// Expression
        right: NodeIndex,
    };

    pub const If = struct {
        token: Token,
        /// Expression
        condition: NodeIndex,
        /// BlockStatement
        consequence: NodeIndex,
        /// BlockStatement
        alternative: ?NodeIndex,
    };

    pub const Function = struct {
        token: Token,
        /// Ident
        parameters: []NodeIndex,
        /// BlockStatement
        body: NodeIndex,
    };

    /// CallExpression
    pub const Call = struct {
        token: Token,
        /// Expression
        function: NodeIndex,
        /// []Expression
        arguments: []NodeIndex,
    };

    pub const Literal = struct {
        token: Token,
        value: LiteralValue,
    };

    const LiteralValue = union(enum) {
        int: i64,
        boolean: bool,
        null,
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
