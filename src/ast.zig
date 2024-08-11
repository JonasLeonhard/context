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

    pub fn tokenLiteral(self: Expression) []const u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
        }
    }

    pub fn toString(self: Expression) []const u8 {
        switch (self) {
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
            expr_str = expression.toString();
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
            expr_str = expression.toString();
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
            expr_str = expression.toString();
        }
        return try std.fmt.allocPrint(alloc, "{s};", .{expr_str});
    }
};

// _____________________________________________

/// <ident> // Example foo
pub const IdentifierExpression = struct {
    token: Token,
    ident: []const u8,

    pub fn tokenLiteral(self: IdentifierExpression) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: IdentifierExpression) []const u8 {
        return self.ident;
    }
};

test "Program toString()" {
    var program = Program.init(testing.allocator);
    defer program.deinit();

    const declare_assign_statement = Statement{
        .declare_assign = DeclareAssignStatement{
            // foo
            .ident_expr = IdentifierExpression{ .token = Token{ .type = .Ident, .literal = "foo" }, .ident = "foo" },
            // :=
            .token = Token{ .type = .DeclareAssign, .literal = ":=" },
            // bar
            .expr = Expression{ .ident_expr = IdentifierExpression{ .token = Token{ .type = .Ident, .literal = "bar" }, .ident = "bar" } },
        },
    };
    try program.statements.append(declare_assign_statement);

    const program_str = try program.toString(testing.allocator);
    defer testing.allocator.free(program_str);

    try testing.expectEqualStrings(program_str, "foo := bar;");
}
