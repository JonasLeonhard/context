const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;

const Errors = error{};

pub const Tree = struct {
    arena: ArenaAllocator,
    /// Root node is expected to be the first statement
    nodes: ArrayList(Statement),

    pub fn init(alloc: std.mem.Allocator) Tree {
        const arena = ArenaAllocator.init(alloc);

        return .{
            .arena = arena,
            .nodes = std.ArrayList(Statement).init(alloc),
        };
    }

    pub fn deinit(self: *Tree) void {
        self.nodes.deinit();
        self.arena.deinit();
    }

    pub fn jsonStringify(self: Tree, jw: anytype) !void {
        try jw.beginObject();
        try jw.objectField("nodes");
        try jw.beginArray();
        for (self.nodes.items) |node| {
            try jw.write(node);
        }
        try jw.endArray();
        try jw.endObject();
    }
};

pub const Statement = union(enum) {
    return_: ReturnStatement,
    declare_assign: DeclareAssignStatement,
    expression: ExpressionStatement,
    block: BlockStatement,

    pub const ReturnStatement = struct {
        token: Token,
        expr: ?Expression,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("return");
            if (self.expr) |expr| {
                try jw.objectField("expression");
                try jw.write(expr);
            }
            try jw.endObject();
        }
    };

    pub const DeclareAssignStatement = struct {
        token: Token,
        ident: Expression.Ident,
        expr: ?Expression,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("declare_assign");
            try jw.objectField("identifier");
            try jw.write(self.ident);
            if (self.expr) |expr| {
                try jw.objectField("expression");
                try jw.write(expr);
            }
            try jw.endObject();
        }
    };

    pub const ExpressionStatement = struct {
        token: Token,
        expr: ?Expression,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("expression_statement");
            if (self.expr) |expr| {
                try jw.objectField("expression");
                try jw.write(expr);
            }
            try jw.endObject();
        }
    };

    pub const BlockStatement = struct {
        token: Token,
        statements: ArrayList(Statement),

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("block");
            try jw.objectField("statements");
            try jw.beginArray();
            for (self.statements.items) |stmt| {
                try jw.write(stmt);
            }
            try jw.endArray();
            try jw.endObject();
        }
    };

    pub fn jsonStringify(self: Statement, jw: anytype) !void {
        switch (self) {
            .return_ => |s| {
                try s.jsonStringify(jw);
            },
            .declare_assign => |s| {
                try s.jsonStringify(jw);
            },
            .expression => |s| {
                try s.jsonStringify(jw);
            },
            .block => |s| {
                try s.jsonStringify(jw);
            },
        }
    }
};

pub const Expression = union(enum) {
    ident: Ident,
    infix: Infix,
    prefix: Prefix,
    literal: Literal,
    if_: If,
    function: Function,
    call: Call,

    pub const Ident = struct {
        token: Token,
        value: []const u8,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("ident");
            try jw.objectField("value");
            try jw.write(self.value);
            try jw.endObject();
        }
    };

    pub const Infix = struct {
        token: Token,
        left: *Expression,
        operator: []const u8,
        right: *Expression,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("infix");
            try jw.objectField("operator");
            try jw.write(self.operator);
            try jw.objectField("left");
            try jw.write(self.left);
            try jw.objectField("right");
            try jw.write(self.right);
            try jw.endObject();
        }
    };

    pub const Prefix = struct {
        token: Token,
        operator: []const u8,
        right: *Expression,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("prefix");
            try jw.objectField("operator");
            try jw.write(self.operator);
            try jw.objectField("right");
            try jw.write(self.right);
            try jw.endObject();
        }
    };

    pub const If = struct {
        token: Token,
        condition: *Expression,
        consequence: Statement.BlockStatement,
        alternative: ?Statement.BlockStatement,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("if");
            try jw.objectField("condition");
            try jw.write(self.condition);
            try jw.objectField("consequence");
            try jw.write(self.consequence);
            if (self.alternative) |alt| {
                try jw.objectField("alternative");
                try jw.write(alt);
            }
            try jw.endObject();
        }
    };

    pub const Function = struct {
        token: Token,
        parameters: ArrayList(Ident),
        body: Statement.BlockStatement,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("function");
            try jw.objectField("parameters");
            try jw.beginArray();
            for (self.parameters.items) |param| {
                try jw.write(param);
            }
            try jw.endArray();
            try jw.objectField("body");
            try jw.write(self.body);
            try jw.endObject();
        }
    };

    pub const Call = struct {
        token: Token,
        function: *Expression,
        arguments: ArrayList(Expression),

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("call");
            try jw.objectField("function");
            try jw.write(self.function);
            try jw.objectField("arguments");
            try jw.beginArray();
            for (self.arguments.items) |arg| {
                try jw.write(arg);
            }
            try jw.endArray();
            try jw.endObject();
        }
    };

    pub const Literal = struct {
        token: Token,
        value: LiteralValue,

        pub fn jsonStringify(self: @This(), jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("literal");
            try jw.objectField("value");
            try self.value.jsonStringify(jw);
            try jw.endObject();
        }
    };

    const LiteralValue = union(enum) {
        int: i64,
        boolean: bool,
        null: void,
        float: f64,
        string: []const u8,

        pub fn jsonStringify(self: LiteralValue, jw: anytype) !void {
            switch (self) {
                .int => |v| {
                    try jw.write(v);
                },
                .boolean => |v| {
                    try jw.write(v);
                },
                .null => {
                    try jw.write(null);
                },
                .float => |v| {
                    try jw.write(v);
                },
                .string => |v| {
                    try jw.write(v);
                },
            }
        }
    };

    pub fn jsonStringify(self: Expression, jw: anytype) !void {
        switch (self) {
            .ident => |e| {
                try e.jsonStringify(jw);
            },
            .infix => |e| {
                try e.jsonStringify(jw);
            },
            .prefix => |e| {
                try e.jsonStringify(jw);
            },
            .literal => |e| {
                try e.jsonStringify(jw);
            },
            .if_ => |e| {
                try e.jsonStringify(jw);
            },
            .function => |e| {
                try e.jsonStringify(jw);
            },
            .call => |e| {
                try e.jsonStringify(jw);
            },
        }
    }
};

test "AstTree" {
    var tree = Tree.init(std.testing.allocator);
    defer tree.deinit();

    // Create a simple AST: x := 5 + 3;
    const literal_5 = try tree.arena.allocator().create(Expression);
    literal_5.* = Expression{
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
    };

    const literal_3 = try tree.arena.allocator().create(Expression);
    literal_3.* = Expression{
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
    };

    const infix = Expression{
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
    };

    const ident = Expression{
        .ident = .{
            .token = Token{
                .type = .Ident,
                .literal = "x",
                .location = null,
            },
            .value = "x",
        },
    };

    const declare_assign = Statement{
        .declare_assign = .{
            .token = Token{
                .type = .Ident,
                .literal = "x",
                .location = null,
            },
            .ident = ident.ident,
            .expr = infix,
        },
    };

    try tree.nodes.append(declare_assign);

    var tree_string = std.ArrayList(u8).init(std.testing.allocator);
    defer tree_string.deinit();
    try std.json.stringify(tree, .{ .whitespace = .indent_2 }, tree_string.writer());

    const expected_output =
        \\{
        \\  "nodes": [
        \\    {
        \\      "type": "declare_assign",
        \\      "identifier": {
        \\        "type": "ident",
        \\        "value": "x"
        \\      },
        \\      "expression": {
        \\        "type": "infix",
        \\        "operator": "+",
        \\        "left": {
        \\          "type": "literal",
        \\          "value": 5
        \\        },
        \\        "right": {
        \\          "type": "literal",
        \\          "value": 3
        \\        }
        \\      }
        \\    }
        \\  ]
        \\}
    ;

    try std.testing.expectEqualStrings(expected_output, tree_string.items);
}
