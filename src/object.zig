const std = @import("std");

pub const Object = union(enum) {
    integer: Integer,
    boolean: *const Boolean,
    null: *const Null,
    return_: Return,
    error_: Error,

    testing: Integer, // TODO: remove

    pub const Integer = struct {
        value: i64,
    };

    pub const Boolean = struct {
        value: bool,
    };

    pub const Return = struct {
        value: *const Object,
    };

    pub const Error = struct {
        message: []const u8,
    };

    pub const Null = struct {};

    pub fn toString(self: Object, alloc: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .integer => |int| {
                return try std.fmt.allocPrint(alloc, "{d}", .{int.value});
            },
            .boolean => |boolean| {
                return try std.fmt.allocPrint(alloc, "{any}", .{boolean.value});
            },
            .return_ => |return_| {
                return try return_.value.toString(alloc);
            },
            .error_ => |error_| {
                return try std.fmt.allocPrint(alloc, "ERROR: {s}", .{error_.message});
            },
            .null => {
                return try std.fmt.allocPrint(alloc, "{any}", .{null});
            },
            else => {
                return "toString not implemented yet";
            },
        }
    }
};