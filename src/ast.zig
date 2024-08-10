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
};

// _____________________________________________

pub const Statement = union(enum) {
    declare_assign: DeclareAssignStatement,
    return_: ReturnStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
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
};

// _____________________________________________

/// <ident> := <expr>; // something like foo := 5;
pub const DeclareAssignStatement = struct {
    token: Token,
    ident_expr: IdentifierExpression,
    expr: ?Expression,
    pub fn tokenLiteral(self: DeclareAssignStatement) []const u8 {
        return self.token.literal;
    }
};

/// return <expression>; // something like return 5;
pub const ReturnStatement = struct {
    token: Token,
    expr: ?Expression,
    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

/// <expression>; // something like x + 10;
pub const ExpressionStatement = struct { value: ?Expression };

// _____________________________________________

/// <ident> // something like foo
pub const IdentifierExpression = struct {
    token: Token,
    ident: []const u8,

    pub fn tokenLiteral(self: IdentifierExpression) []const u8 {
        return self.token.literal;
    }
};
