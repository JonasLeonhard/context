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

    pub fn nodeToString(self: *const Tree, alloc: std.mem.Allocator, node: Node) ![]const u8 {
        switch (node) {
            .root => |root| {
                var result = std.ArrayList(u8).init(alloc);
                defer result.deinit();

                try result.appendSlice("Program {\n");
                for (root.statements) |stmt| {
                    const stmt_str = try self.nodeToString(alloc, self.nodes.items[stmt]);
                    defer alloc.free(stmt_str);
                    try result.appendSlice("  ");
                    try result.appendSlice(stmt_str);
                    try result.appendSlice("\n");
                }
                try result.appendSlice("}");

                return result.toOwnedSlice();
            },
            .return_ => |ret| {
                if (ret.expr) |expr| {
                    const expr_str = try self.nodeToString(alloc, self.nodes.items[expr]);
                    defer alloc.free(expr_str);
                    return try std.fmt.allocPrint(alloc, "return {s}", .{expr_str});
                } else {
                    return try alloc.dupe(u8, "return");
                }
            },
            .declare_assign => |decl| {
                const ident_str = try self.nodeToString(alloc, self.nodes.items[decl.ident]);
                defer alloc.free(ident_str);
                if (decl.expr) |expr| {
                    const expr_str = try self.nodeToString(alloc, self.nodes.items[expr]);
                    defer alloc.free(expr_str);
                    return try std.fmt.allocPrint(alloc, "{s} := {s}", .{ ident_str, expr_str });
                } else {
                    return try std.fmt.allocPrint(alloc, "{s} :=", .{ident_str});
                }
            },
            .expression => |expr| {
                if (expr.expr) |e| {
                    return try self.nodeToString(alloc, self.nodes.items[e]);
                } else {
                    return try alloc.dupe(u8, "");
                }
            },
            .block => |block| {
                var result = std.ArrayList(u8).init(alloc);
                defer result.deinit();

                try result.appendSlice("{\n");
                for (block.statements) |stmt| {
                    const stmt_str = try self.nodeToString(alloc, self.nodes.items[stmt]);
                    defer alloc.free(stmt_str);
                    try result.appendSlice("  ");
                    try result.appendSlice(stmt_str);
                    try result.appendSlice("\n");
                }
                try result.appendSlice("}");

                return result.toOwnedSlice();
            },
            .ident => |ident| {
                return try alloc.dupe(u8, ident.value);
            },
            .infix => |infix| {
                const left_str = try self.nodeToString(alloc, self.nodes.items[infix.left]);
                defer alloc.free(left_str);
                const right_str = try self.nodeToString(alloc, self.nodes.items[infix.right]);
                defer alloc.free(right_str);
                return try std.fmt.allocPrint(alloc, "({s} {s} {s})", .{ left_str, infix.operator, right_str });
            },
            .prefix => |prefix| {
                const right_str = try self.nodeToString(alloc, self.nodes.items[prefix.right]);
                defer alloc.free(right_str);
                return try std.fmt.allocPrint(alloc, "({s}{s})", .{ prefix.operator, right_str });
            },
            .literal => |lit| {
                return switch (lit.value) {
                    .int => |value| try std.fmt.allocPrint(alloc, "{d}", .{value}),
                    .boolean => |value| try std.fmt.allocPrint(alloc, "{}", .{value}),
                    .null => try alloc.dupe(u8, "null"),
                    .float => |value| try std.fmt.allocPrint(alloc, "{d}", .{value}),
                    .string => |value| try std.fmt.allocPrint(alloc, "\"{s}\"", .{value}),
                };
            },
            .if_ => |if_node| {
                const cond_str = try self.nodeToString(alloc, self.nodes.items[if_node.condition]);
                defer alloc.free(cond_str);
                const cons_str = try self.nodeToString(alloc, self.nodes.items[if_node.consequence]);
                defer alloc.free(cons_str);
                if (if_node.alternative) |alt| {
                    const alt_str = try self.nodeToString(alloc, self.nodes.items[alt]);
                    defer alloc.free(alt_str);
                    return try std.fmt.allocPrint(alloc, "if {s} {s} else {s}", .{ cond_str, cons_str, alt_str });
                } else {
                    return try std.fmt.allocPrint(alloc, "if {s} {s}", .{ cond_str, cons_str });
                }
            },
            .function => |func| {
                var result = std.ArrayList(u8).init(alloc);
                defer result.deinit();

                try result.appendSlice("fn(");
                for (func.parameters, 0..) |param, i| {
                    const param_str = try self.nodeToString(alloc, self.nodes.items[param]);
                    defer alloc.free(param_str);
                    try result.appendSlice(param_str);
                    if (i < func.parameters.len - 1) {
                        try result.appendSlice(", ");
                    }
                }
                try result.appendSlice(") ");

                const body_str = try self.nodeToString(alloc, self.nodes.items[func.body]);
                defer alloc.free(body_str);
                try result.appendSlice(body_str);

                return result.toOwnedSlice();
            },
            .call => |call| {
                var result = std.ArrayList(u8).init(alloc);
                defer result.deinit();

                const func_str = try self.nodeToString(alloc, self.nodes.items[call.function]);
                defer alloc.free(func_str);
                try result.appendSlice(func_str);
                try result.appendSlice("(");

                for (call.arguments, 0..) |arg, i| {
                    const arg_str = try self.nodeToString(alloc, self.nodes.items[arg]);
                    defer alloc.free(arg_str);
                    try result.appendSlice(arg_str);
                    if (i < call.arguments.len - 1) {
                        try result.appendSlice(", ");
                    }
                }
                try result.appendSlice(")");

                return result.toOwnedSlice();
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
