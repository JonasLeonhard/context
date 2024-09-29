const std = @import("std");

const Repl = @This();

alloc: std.mem.Allocator,
stdin: std.io.AnyReader,
stdout: std.io.AnyWriter,

const version = "0.0.0";
const colors = .{
    .green = "\x1b[32m",
    .gray = "\x1b[38;5;15m",
    .reset = "\x1b[0m",
};

pub fn init(alloc: std.mem.Allocator, stdin: std.io.AnyReader, stdout: std.io.AnyWriter) Repl {
    return Repl{
        .alloc = alloc,
        .stdin = stdin,
        .stdout = stdout,
    };
}

pub fn eval(self: Repl, file_path: []const u8, file_contents: []const u8) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n", .{ colors.gray, version, colors.reset });
    try self.stdout.print("   {s}--file {s}{s}\n", .{ colors.gray, file_path, colors.reset });

    _ = file_contents; // TODO
}

pub fn eval_to_ast(self: Repl, file_path: []const u8, file_contents: []const u8) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n", .{ colors.gray, version, colors.reset });
    try self.stdout.print("   {s}--file {s}{s}\n", .{ colors.gray, file_path, colors.reset });
    try self.stdout.print("   {s}--tree json{s}\n\n", .{ colors.gray, colors.reset });

    _ = file_contents; // TODO
}

pub fn repl_eval(self: Repl) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n\n", .{ colors.gray, version, colors.reset });

    try self.stdout.print("TODO: start eval repl", .{});
}

pub fn repl_eval_to_ast(self: Repl) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n", .{ colors.gray, version, colors.reset });
    try self.stdout.print("   {s}--tree json{s}\n\n", .{ colors.gray, colors.reset });
    try self.stdout.print("TODO: start eval repl to ast", .{});
}
