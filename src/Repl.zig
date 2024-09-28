const std = @import("std");

const Repl = @This();

alloc: std.mem.Allocator,
stdin: std.io.AnyReader,
stdout: std.io.AnyWriter,

pub fn init(alloc: std.mem.Allocator, stdin: std.io.AnyReader, stdout: std.io.AnyWriter) Repl {
    return Repl{
        .alloc = alloc,
        .stdin = stdin,
        .stdout = stdout,
    };
}

pub fn eval(self: Repl, filepath: []const u8) !void {
    try self.stdout.print("TODO: start eval of filepath: {s}", .{filepath});
}

pub fn eval_to_ast(self: Repl, filepath: []const u8) !void {
    try self.stdout.print("TODO: start eval of filepath to ast: {s}", .{filepath});
}

pub fn repl_eval(self: Repl) !void {
    try self.stdout.print("TODO: start eval repl", .{});
}

pub fn repl_eval_to_ast(self: Repl) !void {
    try self.stdout.print("TODO: start eval repl to ast", .{});
}
