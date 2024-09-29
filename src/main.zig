const std = @import("std");
const io = std.io;
const token = @import("token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Repl = @import("Repl.zig");
const clap = @import("clap");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-t, --tree             Output the ast tree as json.
        \\<str>...               Filepath to execute.
        \\                       If the filepath is empty, start a repl
    );
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = alloc,
    }) catch |err| {
        diag.report(stdout, err) catch {};
        return;
    };
    defer res.deinit();

    if (res.args.help != 0)
        return clap.help(std.io.getStdErr().writer(), clap.Help, &params, .{});

    const repl = Repl.init(alloc, stdin.any(), stdout.any());

    if (res.positionals.len == 0) {
        if (res.args.tree != 0) {
            try repl.repl_eval_to_ast();
        } else {
            try repl.repl_eval();
        }
    } else {
        const filepath = res.positionals[0];
        // Run a file
        const file_contents = try std.fs.cwd().readFileAlloc(alloc, filepath, 1024 * 1024); // 1MB limit
        defer alloc.free(file_contents);

        if (res.args.tree != 0) {
            try repl.eval_to_ast(file_contents);
        } else {
            try repl.eval(file_contents);
        }
    }
}
