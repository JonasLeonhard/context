const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

/// Based on Zigs Data oriented design approach explained in these talks
/// https://www.youtube.com/watch?v=IroPQ150F6c
/// https://www.youtube.com/watch?v=KOZcJwGdQok&t=2005s
pub const Tree = struct {
    arena: ArenaAllocator,
    /// The root node is expected to be the last node added to the nodes array list! => tree.nodes.len - 1
    nodes: ArrayList(Node), // TODO: SPEED: zigs compiler uses a MultiArrayList(Node) here
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

        try self.toJson(alloc, list.writer());

        return list.toOwnedSlice();
    }

    pub fn toJson(self: *const Tree, alloc: std.mem.Allocator, writer: anytype) !void {
        var arena = std.heap.ArenaAllocator.init(alloc);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        var json_tree = std.json.Value{
            .object = std.json.ObjectMap.init(arena_alloc),
        };

        if (self.root) |root| {
            try json_tree.object.put("root", try self.nodeToJson(root, arena_alloc));
        }

        try std.json.stringify(json_tree, .{ .whitespace = .indent_2 }, writer);
    }

    fn nodeToJson(self: *const Tree, node_index: NodeIndex, alloc: std.mem.Allocator) !std.json.Value {
        const node = self.nodes.items[node_index];
        var json_node = std.json.Value{
            .object = std.json.ObjectMap.init(alloc),
        };
        errdefer json_node.object.deinit();

        try json_node.object.put("type", std.json.Value{ .string = @tagName(node) });

        switch (node) {
            .root => |root| {
                var statements = std.json.Value{ .array = std.json.Array.init(alloc) };
                for (root.statements) |stmt| {
                    try statements.array.append(try self.nodeToJson(stmt, alloc));
                }
                try json_node.object.put("statements", statements);
            },
            .return_ => |ret| {
                if (ret.expr) |expr| {
                    try json_node.object.put("expression", try self.nodeToJson(expr, alloc));
                }
            },
            .declare_assign => |decl| {
                try json_node.object.put("identifier", try self.nodeToJson(decl.ident, alloc));
                if (decl.expr) |expr| {
                    try json_node.object.put("expression", try self.nodeToJson(expr, alloc));
                }
            },
            .expression => |expr| {
                if (expr.expr) |e| {
                    try json_node.object.put("expression", try self.nodeToJson(e, alloc));
                }
            },
            .block => |block| {
                var statements = std.json.Value{ .array = std.json.Array.init(alloc) };
                for (block.statements) |stmt| {
                    try statements.array.append(try self.nodeToJson(stmt, alloc));
                }
                try json_node.object.put("statements", statements);
            },
            .ident => |ident| {
                try json_node.object.put("value", std.json.Value{ .string = ident.value });
            },
            .infix => |infix| {
                try json_node.object.put("operator", std.json.Value{ .string = infix.operator });
                try json_node.object.put("left", try self.nodeToJson(infix.left, alloc));
                try json_node.object.put("right", try self.nodeToJson(infix.right, alloc));
            },
            .prefix => |prefix| {
                try json_node.object.put("operator", std.json.Value{ .string = prefix.operator });
                try json_node.object.put("right", try self.nodeToJson(prefix.right, alloc));
            },
            .literal => |lit| {
                switch (lit.value) {
                    .int => |value| try json_node.object.put("value", std.json.Value{ .integer = value }),
                    .boolean => |value| try json_node.object.put("value", std.json.Value{ .bool = value }),
                    .null => try json_node.object.put("value", std.json.Value{ .null = {} }),
                    .float => |value| try json_node.object.put("value", std.json.Value{ .float = value }),
                    .string => |value| try json_node.object.put("value", std.json.Value{ .string = value }),
                }
            },
            .if_ => |if_node| {
                try json_node.object.put("condition", try self.nodeToJson(if_node.condition, alloc));
                try json_node.object.put("consequence", try self.nodeToJson(if_node.consequence, alloc));
                if (if_node.alternative) |alt| {
                    try json_node.object.put("alternative", try self.nodeToJson(alt, alloc));
                }
            },
            .function => |func| {
                var parameters = std.json.Value{ .array = std.json.Array.init(alloc) };
                for (func.parameters) |param| {
                    try parameters.array.append(try self.nodeToJson(param, alloc));
                }
                try json_node.object.put("parameters", parameters);
                try json_node.object.put("body", try self.nodeToJson(func.body, alloc));
            },
            .call => |call| {
                try json_node.object.put("function", try self.nodeToJson(call.function, alloc));
                var arguments = std.json.Value{ .array = std.json.Array.init(alloc) };
                for (call.arguments) |arg| {
                    try arguments.array.append(try self.nodeToJson(arg, alloc));
                }
                try json_node.object.put("arguments", arguments);
            },
        }

        return json_node;
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
        \\{
        \\  "root": {
        \\    "type": "root",
        \\    "statements": [
        \\      {
        \\        "type": "declare_assign",
        \\        "identifier": {
        \\          "type": "ident",
        \\          "value": "x"
        \\        },
        \\        "expression": {
        \\          "type": "infix",
        \\          "operator": "+",
        \\          "left": {
        \\            "type": "literal",
        \\            "value": 5
        \\          },
        \\          "right": {
        \\            "type": "literal",
        \\            "value": 3
        \\          }
        \\        }
        \\      }
        \\    ]
        \\  }
        \\}
    ;

    try std.testing.expectEqualStrings(expected_output, tree_string);
}
