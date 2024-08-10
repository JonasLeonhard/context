const Token = @import("token.zig").Token;
const std = @import("std");
const ArrayList = std.ArrayList;

pub const Program = struct {
    allocator: std.mem.Allocator,
    statements: ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Program {
        return .{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
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

    pub fn toString(self: Program) []const u8 {
        const buffer = std.ArrayList(u8).init(self.allocator);
        defer buffer.deinit();

        for (self.statements) |statement| {
            buffer.appendSlice(statement.toString());
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

    pub fn toString(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| return case.toString(),
        }
    }
};

pub const Expression = union(enum) {
    ident_expr: IdentifierExpression,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
        }
    }

    pub fn toString(self: Statement) []const u8 {
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
    pub fn toString(self: DeclareAssignStatement) []const u8 {
        return self.ident_expr.tokenLiteral() + " " + self.token.tokenLiteral() + " " + self.expr.toString() + ";";
    }
};

/// return <expression>; // Example: return 5;
pub const ReturnStatement = struct {
    token: Token,
    expr: ?Expression,
    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }
    pub fn toString(self: ReturnStatement) []const u8 {
        return self.tokenLiteral() + " " + self.expr.toString() + ";";
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
    pub fn toString(self: ExpressionStatement) []const u8 {
        return self.expr.toString() + ";";
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
