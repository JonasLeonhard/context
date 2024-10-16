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
    array: Array,
    builtin: Builtin,
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

    pub const Array = struct {
        elements: std.ArrayList(Object),

        pub fn clone(self: Array, alloc: std.mem.Allocator) anyerror!Array {
            var cloned_elements = try std.ArrayList(Object).initCapacity(alloc, self.elements.items.len);
            errdefer cloned_elements.deinit();

            for (self.elements.items) |*element| {
                const cloned_element = try element.clone(alloc);
                try cloned_elements.append(cloned_element);
            }

            return Array{
                .elements = cloned_elements,
            };
        }
    };

    pub const Builtin = struct {
        name: []const u8,
        func: *const fn (args: []const Object, alloc: std.mem.Allocator) anyerror!Object,

        pub fn get(name: []const u8) ?Object {
            if (std.mem.eql(u8, name, "len")) {
                return Object{
                    .builtin = .{
                        .name = name,
                        .func = lenFunction,
                    },
                };
            }

            if (std.mem.eql(u8, name, "first")) {
                return Object{ .builtin = .{
                    .name = name,
                    .func = first,
                } };
            }

            if (std.mem.eql(u8, name, "last")) {
                return Object{ .builtin = .{
                    .name = name,
                    .func = last,
                } };
            }

            return null;
        }

        fn lenFunction(args: []const Object, alloc: std.mem.Allocator) anyerror!Object {
            if (args.len != 1) return Object{ .error_ = .{ .message = try std.fmt.allocPrint(alloc, "wrong number of arguments. got={d}, want=1", .{args.len}) } };

            switch (args[0]) {
                .string => |s| return Object{ .integer = .{ .value = @intCast(s.value.len) } },
                .array => |a| return Object{ .integer = .{ .value = @intCast(a.elements.items.len) } },
                else => {
                    return Object{ .error_ = .{ .message = try std.fmt.allocPrint(alloc, "argument to 'len' not supported, got {s}", .{@tagName(args[0])}) } };
                },
            }
        }

        fn first(args: []const Object, alloc: std.mem.Allocator) anyerror!Object {
            if (args.len != 1) return Object{ .error_ = .{ .message = try std.fmt.allocPrint(alloc, "wrong number of arguments. got={d}, want=1", .{args.len}) } };

            switch (args[0]) {
                .array => |a| {
                    if (a.elements.items.len > 0) {
                        return a.elements.items[0];
                    }

                    return Object{ .null = .{} };
                },
                else => {
                    return Object{ .error_ = .{ .message = try std.fmt.allocPrint(alloc, "argument to 'first' not supported, got {s}", .{@tagName(args[0])}) } };
                },
            }
        }

        fn last(args: []const Object, alloc: std.mem.Allocator) anyerror!Object {
            if (args.len != 1) return Object{ .error_ = .{ .message = try std.fmt.allocPrint(alloc, "wrong number of arguments. got={d}, want=1", .{args.len}) } };

            switch (args[0]) {
                .array => |a| {
                    if (a.elements.items.len > 0) {
                        return a.elements.getLast();
                    }

                    return Object{ .null = .{} };
                },
                else => {
                    return Object{ .error_ = .{ .message = try std.fmt.allocPrint(alloc, "argument to 'last' not supported, got {s}", .{@tagName(args[0])}) } };
                },
            }
        }

        pub fn clone(self: Builtin, alloc: std.mem.Allocator) !Builtin {
            return Builtin{
                .name = try alloc.dupe(u8, self.name),
                .func = self.func,
            };
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
            .array => |a| {
                try jw.objectField("type");
                try jw.write("array");
                try jw.objectField("elements");
                try jw.beginArray();
                for (a.elements.items) |element| {
                    try jw.write(element);
                }
                try jw.endArray();
            },
            .builtin => |b| {
                try jw.objectField("type");
                try jw.write("builtin");
                try jw.objectField("name");
                try jw.write(b.name);
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
                return try allocator.dupe(u8, "null");
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
            .array => |a| {
                var elements = std.ArrayList(u8).init(allocator);
                defer elements.deinit();
                for (a.elements.items, 0..) |element, i| {
                    if (i > 0) {
                        try elements.appendSlice(", ");
                    }
                    const element_str = try element.toString(allocator);
                    defer allocator.free(element_str);
                    try elements.appendSlice(element_str);
                }

                return try std.fmt.allocPrint(allocator, "Array[{s}]", .{elements.items});
            },
            .builtin => |b| {
                return try std.fmt.allocPrint(allocator, "Builtin function: {s}", .{b.name});
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
            .array => |*array| Object{ .array = try array.clone(alloc) },
            .builtin => |b| Object{ .builtin = try b.clone(alloc) },
            .function => |function| Object{ .function = try function.clone(alloc) },
        };
    }
};
