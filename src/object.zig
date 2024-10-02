const std = @import("std");
const ast = @import("ast.zig");
const Environment = @import("Environment.zig");

pub const Object = union(enum) {
    integer: Integer,
    string: String,
    boolean: Boolean,
    null: Null,
    return_: Return,
    function: Function,
    error_: Error,

    pub const Integer = struct {
        value: i64,
    };

    pub const String = struct {
        value: []const u8,

        pub fn clone(self: String, alloc: std.mem.Allocator) !String {
            const owned_value = try alloc.dupe(u8, self.value);
            return String{ .value = owned_value };
        }
    };

    pub const Boolean = struct {
        value: bool,
    };

    pub const Return = struct {
        value: *Object,

        pub fn clone(self: *Return, alloc: std.mem.Allocator) anyerror!Return {
            const return_val = try alloc.create(Object);
            return_val.* = try self.value.clone(alloc);
            return Return{ .value = return_val };
        }
    };

    pub const Function = struct {
        parameters: std.ArrayList(ast.Expression.Ident),
        body: ast.Statement.BlockStatement,

        pub fn clone(self: Function, alloc: std.mem.Allocator) !Function {
            return Function{ .parameters = try self.parameters.clone(), .body = try self.body.clone(alloc) };
        }
    };

    pub const Error = struct {
        message: []const u8,

        pub fn clone(self: Error, alloc: std.mem.Allocator) !Error {
            const owned_value = try alloc.dupe(u8, self.message);
            return Error{ .message = owned_value };
        }
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
            .string => |str| {
                try jw.objectField("type");
                try jw.write("string");
                try jw.objectField("value");
                try jw.write(str.value);
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
            .string => |string| {
                return try std.fmt.allocPrint(allocator, "{s}", .{string.value});
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

    pub fn clone(self: *Object, alloc: std.mem.Allocator) !Object {
        return switch (self.*) {
            .error_ => |error_| Object{ .error_ = try error_.clone(alloc) },
            .null => self.*,
            .string => |string| Object{ .string = try string.clone(alloc) },
            .integer => self.*,
            .boolean => self.*,
            .return_ => |*return_| Object{ .return_ = try return_.clone(alloc) },
            .function => |function| Object{ .function = try function.clone(alloc) },
        };
    }
};
