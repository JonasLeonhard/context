const std = @import("std");
const token = @import("token.zig");
const Token = token.Token;
const ArenaAllocator = std.heap.ArenaAllocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMap;

const Errors = error{
    OutOfMemory,
};

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

    pub fn jsonStringify(self: Tree, jw: anytype) Errors!void {
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

        pub fn jsonStringify(self: ReturnStatement, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("return");
            if (self.expr) |expr| {
                try jw.objectField("expression");
                try jw.write(expr);
            }
            try jw.endObject();
        }

        pub fn clone(self: ReturnStatement, alloc: std.mem.Allocator) !ReturnStatement {
            return ReturnStatement{
                .token = self.token,
                .expr = if (self.expr) |e| try e.clone(alloc) else null,
            };
        }
    };

    pub const DeclareAssignStatement = struct {
        token: Token,
        ident: Expression.Ident,
        expr: ?Expression,

        pub fn jsonStringify(self: DeclareAssignStatement, jw: anytype) Errors!void {
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

        pub fn clone(self: DeclareAssignStatement, alloc: std.mem.Allocator) !DeclareAssignStatement {
            return DeclareAssignStatement{
                .token = self.token,
                .ident = try self.ident.clone(alloc),
                .expr = if (self.expr) |e| try e.clone(alloc) else null,
            };
        }
    };

    pub const ExpressionStatement = struct {
        token: Token,
        expr: ?Expression,

        pub fn jsonStringify(self: ExpressionStatement, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("expression_statement");
            if (self.expr) |expr| {
                try jw.objectField("expression");
                try jw.write(expr);
            }
            try jw.endObject();
        }

        pub fn clone(self: ExpressionStatement, alloc: std.mem.Allocator) !ExpressionStatement {
            return ExpressionStatement{
                .token = self.token,
                .expr = if (self.expr) |e| try e.clone(alloc) else null,
            };
        }
    };

    pub const BlockStatement = struct {
        token: Token,
        statements: ArrayList(Statement),

        pub fn jsonStringify(self: BlockStatement, jw: anytype) Errors!void {
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

        pub fn clone(self: BlockStatement, alloc: std.mem.Allocator) !BlockStatement {
            var new_statements = ArrayList(Statement).init(alloc);
            errdefer new_statements.deinit();

            try new_statements.ensureTotalCapacity(self.statements.items.len);
            for (self.statements.items) |stmt| {
                const cloned_stmt = try stmt.clone(alloc);
                try new_statements.append(cloned_stmt);
            }

            return BlockStatement{
                .token = self.token,
                .statements = new_statements,
            };
        }
    };

    pub fn jsonStringify(self: Statement, jw: anytype) Errors!void {
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
    pub fn clone(self: Statement, alloc: std.mem.Allocator) Errors!Statement {
        return switch (self) {
            .return_ => |return_| Statement{ .return_ = try return_.clone(alloc) },
            .declare_assign => |declare_assign| Statement{ .declare_assign = try declare_assign.clone(alloc) },
            .expression => |expression| Statement{ .expression = try expression.clone(alloc) },
            .block => |block| Statement{ .block = try block.clone(alloc) },
        };
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
    index: Index,

    pub const Ident = struct {
        token: Token,
        value: []const u8,

        pub fn jsonStringify(self: Ident, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("ident");
            try jw.objectField("value");
            try jw.write(self.value);
            try jw.endObject();
        }
        pub fn clone(self: Ident, alloc: std.mem.Allocator) !Ident {
            return Ident{
                .token = self.token,
                .value = try alloc.dupe(u8, self.value),
            };
        }
    };

    pub const Infix = struct {
        token: Token,
        left: *Expression,
        operator: []const u8,
        right: *Expression,

        pub fn jsonStringify(self: Infix, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("infix");
            try jw.objectField("operator");
            try jw.write(self.operator);
            try jw.objectField("left");
            try jw.write(self.left.*);
            try jw.objectField("right");
            try jw.write(self.right.*);
            try jw.endObject();
        }
        pub fn clone(self: Infix, alloc: std.mem.Allocator) !Infix {
            const new_left = try alloc.create(Expression);
            errdefer alloc.destroy(new_left);
            new_left.* = try self.left.clone(alloc);

            const new_right = try alloc.create(Expression);
            errdefer alloc.destroy(new_right);
            new_right.* = try self.right.clone(alloc);

            return Infix{
                .token = self.token,
                .left = new_left,
                .operator = try alloc.dupe(u8, self.operator),
                .right = new_right,
            };
        }
    };

    pub const Prefix = struct {
        token: Token,
        operator: []const u8,
        right: *Expression,

        pub fn jsonStringify(self: Prefix, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("prefix");
            try jw.objectField("operator");
            try jw.write(self.operator);
            try jw.objectField("right");
            try jw.write(self.right.*);
            try jw.endObject();
        }
        pub fn clone(self: Prefix, alloc: std.mem.Allocator) !Prefix {
            const new_right = try alloc.create(Expression);
            errdefer alloc.destroy(new_right);
            new_right.* = try self.right.clone(alloc);

            return Prefix{
                .token = self.token,
                .operator = try alloc.dupe(u8, self.operator),
                .right = new_right,
            };
        }
    };

    pub const If = struct {
        token: Token,
        condition: *Expression,
        consequence: Statement.BlockStatement,
        alternative: ?Statement.BlockStatement,

        pub fn jsonStringify(self: If, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("if");
            try jw.objectField("condition");
            try jw.write(self.condition.*);
            try jw.objectField("consequence");
            try jw.write(self.consequence);
            if (self.alternative) |alt| {
                try jw.objectField("alternative");
                try jw.write(alt);
            }
            try jw.endObject();
        }
        pub fn clone(self: If, alloc: std.mem.Allocator) !If {
            const new_condition = try alloc.create(Expression);
            errdefer alloc.destroy(new_condition);
            new_condition.* = try self.condition.clone(alloc);

            return If{
                .token = self.token,
                .condition = new_condition,
                .consequence = try self.consequence.clone(alloc),
                .alternative = if (self.alternative) |alt| try alt.clone(alloc) else null,
            };
        }
    };

    pub const Function = struct {
        token: Token,
        parameters: ArrayList(Ident),
        body: Statement.BlockStatement,

        pub fn jsonStringify(self: Function, jw: anytype) Errors!void {
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

        pub fn clone(self: Function, alloc: std.mem.Allocator) !Function {
            var new_parameters = ArrayList(Ident).init(alloc);
            errdefer new_parameters.deinit();

            try new_parameters.ensureTotalCapacity(self.parameters.items.len);
            for (self.parameters.items) |param| {
                const cloned_param = try param.clone(alloc);
                try new_parameters.append(cloned_param);
            }

            return Function{
                .token = self.token,
                .parameters = new_parameters,
                .body = try self.body.clone(alloc),
            };
        }
    };

    pub const Call = struct {
        token: Token,
        function: *Expression,
        arguments: ArrayList(Expression),

        pub fn jsonStringify(self: Call, jw: anytype) Errors!void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("call");
            try jw.objectField("function");
            try jw.write(self.function.*);
            try jw.objectField("arguments");
            try jw.beginArray();
            for (self.arguments.items) |arg| {
                try jw.write(arg);
            }
            try jw.endArray();
            try jw.endObject();
        }

        pub fn clone(self: Call, alloc: std.mem.Allocator) !Call {
            const new_function = try alloc.create(Expression);
            errdefer alloc.destroy(new_function);
            new_function.* = try self.function.clone(alloc);

            var new_arguments = ArrayList(Expression).init(alloc);
            errdefer new_arguments.deinit();

            try new_arguments.ensureTotalCapacity(self.arguments.items.len);
            for (self.arguments.items) |arg| {
                const cloned_arg = try arg.clone(alloc);
                try new_arguments.append(cloned_arg);
            }

            return Call{
                .token = self.token,
                .function = new_function,
                .arguments = new_arguments,
            };
        }
    };

    pub const Index = struct {
        token: Token,
        left: *Expression,
        index: *Expression,

        pub fn jsonStringify(self: Index, jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("index");
            try jw.objectField("left");
            try jw.write(self.left.*);
            try jw.objectField("index");
            try jw.write(self.index.*);
            try jw.endObject();
        }

        pub fn clone(self: Index, alloc: std.mem.Allocator) !Index {
            const new_left = try alloc.create(Expression);
            errdefer alloc.destroy(new_left);
            new_left.* = try self.left.clone(alloc);

            const new_index = try alloc.create(Expression);
            errdefer alloc.destroy(new_index);
            new_index.* = try self.index.clone(alloc);

            return Index{
                .token = self.token,
                .left = new_left,
                .index = new_index,
            };
        }
    };

    pub const Literal = struct {
        token: Token,
        value: LiteralValue,

        pub fn jsonStringify(self: Literal, jw: anytype) !void {
            try jw.beginObject();
            try jw.objectField("type");
            try jw.write("literal");
            try jw.objectField("value");
            try self.value.jsonStringify(jw);
            try jw.endObject();
        }

        pub fn clone(self: Literal, alloc: std.mem.Allocator) !Literal {
            return Literal{
                .token = self.token,
                .value = try self.value.clone(alloc),
            };
        }
    };

    const LiteralValue = union(enum) {
        int: i64,
        boolean: bool,
        null: void,
        float: f64,
        string: []const u8,
        array: ArrayList(Expression),
        hashmap: StringHashMap(Expression),

        pub fn jsonStringify(self: LiteralValue, jw: anytype) Errors!void {
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
                .array => |array| {
                    try jw.beginArray();
                    for (array.items) |item| {
                        try jw.write(item);
                    }
                    try jw.endArray();
                },
                .hashmap => |hashmap| {
                    try jw.beginObject();
                    var it = hashmap.iterator();
                    var index: usize = 0;
                    while (it.next()) |entry| {
                        const key = entry.key_ptr.*;
                        const value = entry.value_ptr.*;

                        try jw.objectField(key);
                        try jw.write(value);
                        index += 1;
                    }
                    try jw.endObject();
                },
            }
        }

        pub fn clone(self: LiteralValue, alloc: std.mem.Allocator) !LiteralValue {
            return switch (self) {
                .int => |v| LiteralValue{ .int = v },
                .boolean => |v| LiteralValue{ .boolean = v },
                .null => LiteralValue{ .null = {} },
                .float => |v| LiteralValue{ .float = v },
                .string => |v| LiteralValue{ .string = try alloc.dupe(u8, v) },
                .array => |v| {
                    var new_array = try std.ArrayList(Expression).initCapacity(alloc, v.items.len);
                    errdefer new_array.deinit();

                    for (v.items) |item| {
                        try new_array.append(try item.clone(alloc));
                    }

                    return LiteralValue{ .array = new_array };
                },
                .hashmap => |v| {
                    const new_hashmap = try v.cloneWithAllocator(alloc);
                    return LiteralValue{ .hashmap = new_hashmap };
                },
            };
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
            .index => |e| {
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

    pub fn clone(self: Expression, alloc: std.mem.Allocator) Errors!Expression {
        return switch (self) {
            .ident => |ident| Expression{ .ident = try ident.clone(alloc) },
            .infix => |infix| Expression{ .infix = try infix.clone(alloc) },
            .prefix => |prefix| Expression{ .prefix = try prefix.clone(alloc) },
            .literal => |literal| Expression{ .literal = try literal.clone(alloc) },
            .if_ => |if_| Expression{ .if_ = try if_.clone(alloc) },
            .function => |function| Expression{ .function = try function.clone(alloc) },
            .index => |index| Expression{ .index = try index.clone(alloc) },
            .call => |call| Expression{ .call = try call.clone(alloc) },
        };
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
