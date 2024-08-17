const Token = @import("token.zig").Token;
const std = @import("std");
const ArrayList = std.ArrayList;
const testing = std.testing;

pub const Program = struct {
    statements: ArrayList(Statement),

    pub fn init(alloc: std.mem.Allocator) Program {
        return .{
            .statements = std.ArrayList(Statement).init(alloc),
        };
    }

    pub fn deinit(self: *Program) void {
        self.statements.deinit();
    }

    pub fn tokenLiteral(self: Program) []u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    /// Returns a string representation of the program. Requires alloc.free of the return value!
    /// this doesnt use the stored token literals, but instead uses the parsed Expression and Statement values.
    pub fn toString(self: Program, alloc: std.mem.Allocator) ![]const u8 {
        var buffer = std.ArrayList(u8).init(alloc);
        defer buffer.deinit();

        for (self.statements.items) |statement| {
            const statement_str = try statement.toString(alloc);
            defer alloc.free(statement_str);
            try buffer.appendSlice(statement_str);
        }

        return buffer.toOwnedSlice();
    }
};

// _____________________________________________

pub const Statement = union(enum) {
    declare_assign: DeclareAssignStatement,
    return_: ReturnStatement,
    expression: ExpressionStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
        }
    }

    pub fn toString(self: Statement, alloc: std.mem.Allocator) ![]const u8 {
        switch (self) {
            inline else => |case| return try case.toString(alloc),
        }
    }
};

pub const Expression = union(enum) {
    ident_expr: IdentifierExpression,
    int_literal_expr: IntLiteralExpression,
    prefix_expr: PrefixExpression,
    infix_expr: InfixExpression,

    pub fn tokenLiteral(self: Expression) []const u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
        }
    }

    pub fn toString(self: Expression, alloc: std.mem.Allocator) ![]const u8 {
        switch (self) {
            inline .int_literal_expr => |case| return try case.toString(alloc),
            inline .prefix_expr => |case| return try case.toString(alloc),
            inline .infix_expr => |case| return try case.toString(alloc),
            inline else => |case| return case.toString(),
        }
    }
};

// _____________________________________________

/// <ident> := <expr>; // Example: foo := 5;
pub const DeclareAssignStatement = struct {
    token: Token,
    ident_expr: IdentifierExpression,
    expr: ?Expression,

    pub fn tokenLiteral(self: DeclareAssignStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: DeclareAssignStatement, alloc: std.mem.Allocator) ![]const u8 {
        var expr_str: []const u8 = "";
        if (self.expr) |expression| {
            expr_str = try expression.toString(alloc);
        }
        return try std.fmt.allocPrint(alloc, "{s} {s} {s};", .{ self.ident_expr.toString(), self.token.literal, expr_str });
    }
};

/// return <expression>; // Example: return 5;
pub const ReturnStatement = struct {
    token: Token,
    expr: ?Expression,

    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ReturnStatement, alloc: std.mem.Allocator) ![]const u8 {
        var expr_str: []const u8 = "";
        if (self.expr) |expression| {
            expr_str = try expression.toString(alloc);
        }
        return try std.fmt.allocPrint(alloc, "{s} {s};", .{ self.tokenLiteral(), expr_str });
    }
};

/// <expression>; // Example: x + 10;
pub const ExpressionStatement = struct {
    /// the first token of the expression
    token: Token,
    expr: ?Expression,

    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ExpressionStatement, alloc: std.mem.Allocator) ![]const u8 {
        var expr_str: []const u8 = "";
        if (self.expr) |expression| {
            expr_str = try expression.toString(alloc);
            defer alloc.free(expr_str);
        }
        return try std.fmt.allocPrint(alloc, "{s};", .{expr_str});
    }
};

// _____________________________________________

/// <ident> // Example foo
pub const IdentifierExpression = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: IdentifierExpression) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: IdentifierExpression) []const u8 {
        return self.value;
    }
};

/// <INT>;
pub const IntLiteralExpression = struct {
    token: Token,
    value: i32,

    pub fn tokenLiteral(self: IntLiteralExpression) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: IntLiteralExpression, alloc: std.mem.Allocator) ![]const u8 {
        return try std.fmt.allocPrint(alloc, "{d}", .{self.value});
    }
};

/// <prefix operator> <expression> -> !5 or -10
pub const PrefixExpression = struct {
    /// first prefix_expr token. Eg !
    token: Token,
    operator: []const u8,
    right: *const Expression,

    pub fn tokenLiteral(self: PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: PrefixExpression, alloc: std.mem.Allocator) anyerror![]const u8 {
        const right_str = try self.right.toString(alloc);
        defer alloc.free(right_str);

        return try std.fmt.allocPrint(alloc, "({s}{s})", .{ self.operator, right_str });
    }
};

/// <expression> <infix operator> <expression> -> 5 - 5; or 5 * 10;
pub const InfixExpression = struct {
    /// first infix expression token, eg 5
    token: Token,
    left: *const Expression,
    operator: []const u8,
    right: *const Expression,

    pub fn tokenLiteral(self: InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: InfixExpression, alloc: std.mem.Allocator) anyerror![]const u8 {
        const left_str = try self.left.toString(alloc);
        const right_str = try self.right.toString(alloc);
        defer alloc.free(left_str);
        defer alloc.free(right_str);

        return try std.fmt.allocPrint(alloc, "({s} {s} {s})", .{ left_str, self.operator, right_str });
    }
};

test "Program toString()" {
    var program = Program.init(testing.allocator);
    defer program.deinit();

    const declare_assign_statement = Statement{
        .declare_assign = .{
            // foo
            .ident_expr = .{
                .token = .{ .type = .Ident, .literal = "foo" },
                .value = "foo",
            },
            // :=
            .token = .{
                .type = .DeclareAssign,
                .literal = ":=",
            },
            // bar
            .expr = .{ .ident_expr = .{
                .token = .{ .type = .Ident, .literal = "bar" },
                .value = "bar",
            } },
        },
    };
    try program.statements.append(declare_assign_statement);

    const program_str = try program.toString(testing.allocator);
    defer testing.allocator.free(program_str);

    try testing.expectEqualStrings(program_str, "foo := bar;");
}
