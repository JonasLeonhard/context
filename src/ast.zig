const Token = @import("token.zig").Token;
const std = @import("std");
const ArrayList = std.ArrayList;

pub const Statement = union(enum) {
    declare_assign: DeclareAssignStatement,
    todo: DeclareAssignStatement, // TODO added because of match with single arm

    pub fn tokenLiteral(self: Statement) []u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
        }
    }
};

pub const Expression = union(enum) {
    identifier: IdentifierExpression,

    pub fn tokenLiteral(self: Statement) []const u8 {
        switch (self) {
            inline else => |case| return case.tokenLiteral(),
        }
    }
};

// _____________________________________________

pub const DeclareAssignStatement = struct {
    token: Token,
    name: IdentifierExpression,
    value: ?Expression,
    pub fn tokenLiteral(self: DeclareAssignStatement) []const u8 {
        return self.token.literal;
    }
};

pub const IdentifierExpression = struct {
    token: Token,
    value: []const u8,

    pub fn tokenLiteral(self: IdentifierExpression) []const u8 {
        return self.token.literal;
    }
};

// _____________________________________________

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
