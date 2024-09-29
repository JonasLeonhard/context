const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");

pub const Object = union(enum) {
    integer: Integer,
    boolean: Boolean,
    null: Null,
    return_: Return,
    function: Function,
    error_: Error,

    pub const Integer = struct {
        value: i64,
    };

    pub const Boolean = struct {
        value: bool,
    };

    pub const Return = struct {
        value: *Object,
    };

    pub const Function = struct {
        parameters: std.ArrayList(ast.Expression.Ident),
        body: ast.Statement.BlockStatement,
    };

    pub const Error = struct {
        message: []const u8,
    };

    pub const Null = struct {};

    pub fn isTruthy(self: Object) bool {
        switch (self) {
            .null => {
                return false;
            },
            .boolean => |boolean| {
                return boolean.value;
            },
            else => {
                return true;
            },
        }
    }

    pub fn jsonStringify(self: Object, jw: anytype) !void {
        try jw.beginObject();

        switch (self) {
            .integer => |i| {
                try jw.objectField("type");
                try jw.write("integer");
                try jw.objectField("value");
                try jw.write(i.value);
            },
            .boolean => |b| {
                try jw.objectField("type");
                try jw.write("boolean");
                try jw.objectField("value");
                try jw.write(b.value);
            },
            .null => {
                try jw.objectField("type");
                try jw.write("null");
            },
            .return_ => |r| {
                try jw.objectField("type");
                try jw.write("return");
                try jw.objectField("value");
                try r.value.jsonStringify(jw);
            },
            .function => |f| {
                try jw.objectField("type");
                try jw.write("function");
                try jw.objectField("parameters");
                try jw.beginArray();
                for (f.parameters.items) |param| {
                    try jw.write(param.value);
                }
                try jw.endArray();
                try jw.objectField("body");
                try jw.write(f.body); // Simplified representation
            },
            .error_ => |e| {
                try jw.objectField("type");
                try jw.write("error");
                try jw.objectField("message");
                try jw.write(e.message);
            },
        }

        try jw.endObject();
    }

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .integer => |int| {
                return try std.fmt.allocPrint(allocator, "{d}", .{int.value});
            },
            .boolean => |boolean| {
                return try std.fmt.allocPrint(allocator, "{any}", .{boolean.value});
            },
            .null => {
                return "null";
            },
            .return_ => |return_| {
                return return_.value.toString(allocator);
            },
            .function => |f| {
                var params = std.ArrayList(u8).init(allocator);
                defer params.deinit();

                for (f.parameters.items, 0..) |param, i| {
                    if (i > 0) {
                        try params.appendSlice(", ");
                    }
                    try params.appendSlice(param.value);
                }

                return try std.fmt.allocPrint(allocator, "Function(params: [{s}], body: <...>)", .{params.items});
            },
            .error_ => |e| {
                return try std.fmt.allocPrint(allocator, "Error: {s}", .{e.message});
            },
        }
    }
};
