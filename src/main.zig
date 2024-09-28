const std = @import("std");
const io = std.io;
const token = @import("token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Repl = @import("Repl.zig");
const clap = @import("clap");

// TODO: use cli args lib?
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help             Display this help and exit.
        \\-t, --tree <str>   An option parameter, which takes a value.
        \\-s, --string <str>...  An option parameter which can be specified multiple times.
        \\<str>...
        \\
    );

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
        .allocator = alloc,
    }) catch |err| {
        // Report useful error and exit
        diag.report(stdout, err) catch {};
        return err;
    };
    defer res.deinit();

    const repl = Repl.init(alloc, stdin.any(), stdout.any());
    _ = repl;

    std.debug.print("{any}", .{res.args});

    // if (filepath) |path| {
    //     // Run a file
    //     const file_contents = try std.fs.cwd().readFileAlloc(alloc, path, 1024 * 1024); // 1MB limit
    //     defer alloc.free(file_contents);
    //
    //     if (tree_mode) {
    //         try repl.eval_to_ast(file_contents);
    //     } else {
    //         try repl.eval(file_contents);
    //     }
    // } else {
    //     // Run REPL
    //     if (tree_mode) {
    //         try repl.repl_eval();
    //     } else {
    //         try repl.repl_eval_to_ast();
    //     }
    // }
}
