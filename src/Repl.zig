// TODO: this is missing a Garbage Collector.

const std = @import("std");
const Environment = @import("Environment.zig");
const Evaluator = @import("Evaluator.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Tree = @import("ast.zig").Tree;

const Repl = @This();

alloc: std.mem.Allocator,
stdin: std.io.AnyReader,
stdout: std.io.AnyWriter,
env: Environment,
evaluator: Evaluator,

const version = "0.0.0";
const colors = .{
    .green = "\x1b[32m",
    .red = "\x1b[0;31m",
    .gray = "\x1b[38;5;15m",
    .reset = "\x1b[0m",
};

pub fn init(alloc: std.mem.Allocator, stdin: std.io.AnyReader, stdout: std.io.AnyWriter) Repl {
    return Repl{
        .alloc = alloc,
        .stdin = stdin,
        .stdout = stdout,
        .env = Environment.init(alloc),
        .evaluator = Evaluator.init(alloc),
    };
}

pub fn deinit(self: *Repl) void {
    self.env.deinit();
    self.evaluator.deinit();
}

pub fn eval(self: *Repl, file_path: []const u8, file_contents: []const u8) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n", .{ colors.gray, version, colors.reset });
    try self.stdout.print("   {s}--file {s}{s}\n", .{ colors.gray, file_path, colors.reset });

    var ast_tree = try self.parse_ast_tree(file_contents);
    defer ast_tree.deinit();

    const evaluated_obj = try self.evaluator.evalTree(&ast_tree, &self.env);
    const evaluated_str = try evaluated_obj.toString(self.alloc);
    defer self.alloc.free(evaluated_str);

    if (evaluated_obj == .error_) {
        try self.stdout.print("{s}>> {s}{s}\n", .{ colors.red, evaluated_str, colors.reset });
    } else {
        try self.stdout.print("{s}>> {s}{s}\n", .{ colors.green, evaluated_str, colors.reset });
    }
}

pub fn eval_to_ast(self: *Repl, file_path: []const u8, file_contents: []const u8) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n", .{ colors.gray, version, colors.reset });
    try self.stdout.print("   {s}--file {s}{s}\n", .{ colors.gray, file_path, colors.reset });
    try self.stdout.print("   {s}--tree json{s}\n\n", .{ colors.gray, colors.reset });

    var ast_tree = try self.parse_ast_tree(file_contents);
    defer ast_tree.deinit();

    const ast_tree_str = try std.json.stringifyAlloc(self.alloc, ast_tree, .{ .whitespace = .indent_2 });
    defer self.alloc.free(ast_tree_str);
    try self.stdout.print("{s}>> {s}{s}\n", .{ colors.green, ast_tree_str, colors.reset });
}

pub fn repl_eval(self: *Repl) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n\n", .{ colors.gray, version, colors.reset });

    while (true) {
        try self.stdout.print("{s}>>{s} ", .{ colors.green, colors.reset });
        const user_input = try self.stdin.readUntilDelimiterAlloc(self.alloc, '\n', 1024);
        defer self.alloc.free(user_input);

        if (stopRunning(user_input))
            break;

        var ast_tree = self.parse_ast_tree(user_input) catch {
            continue;
        };
        defer ast_tree.deinit();

        const evaluated_obj = try self.evaluator.evalTree(&ast_tree, &self.env);
        const evaluated_str = try evaluated_obj.toString(self.alloc);
        defer self.alloc.free(evaluated_str);

        if (evaluated_obj == .error_) {
            try self.stdout.print("{s}{s}{s}\n", .{ colors.red, evaluated_str, colors.reset });
        } else {
            try self.stdout.print("{s}{s}{s}\n", .{ colors.green, evaluated_str, colors.reset });
        }

        self.evaluator.reset();
    }
}

pub fn repl_eval_to_ast(self: *Repl) !void {
    try self.stdout.print("{s}Welcome to Context:{s}\n", .{ colors.green, colors.reset });
    try self.stdout.print("   {s}--version {s}{s}\n", .{ colors.gray, version, colors.reset });
    try self.stdout.print("   {s}--tree json{s}\n\n", .{ colors.gray, colors.reset });

    while (true) {
        try self.stdout.print("{s}>>{s} ", .{ colors.green, colors.reset });
        const user_input = try self.stdin.readUntilDelimiterAlloc(self.alloc, '\n', 1024);
        defer self.alloc.free(user_input);

        if (stopRunning(user_input))
            break;

        var ast_tree = self.parse_ast_tree(user_input) catch {
            continue;
        };
        defer ast_tree.deinit();

        const ast_tree_str = try std.json.stringifyAlloc(self.alloc, ast_tree, .{ .whitespace = .indent_2 });
        defer self.alloc.free(ast_tree_str);
        try self.stdout.print("{s}{s}{s}\n", .{ colors.green, ast_tree_str, colors.reset });

        self.evaluator.reset();
    }
}

fn stopRunning(input: []const u8) bool {
    if (std.mem.startsWith(u8, input, "exit") or std.mem.startsWith(u8, input, "quit")) {
        return true;
    }
    return false;
}

fn parse_ast_tree(self: Repl, text: []const u8) !Tree {
    const lexer = Lexer.init(text);
    var parser = try Parser.init(self.alloc, lexer);
    defer parser.deinit();

    return parser.parseTree(self.alloc) catch |err| {
        if (parser.errors.items.len == 0) {
            return err;
        }

        std.debug.print("{s}Error: Parser has {d} errors{s}\n", .{ colors.red, parser.errors.items.len, colors.reset });
        for (parser.errors.items) |parse_err| {
            std.debug.print("{s}   -> {s}{s}\n", .{ colors.red, parse_err, colors.reset });
        }
        return err;
    };
}
