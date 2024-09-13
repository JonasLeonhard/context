const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

/// Read-Parse-Print-Loop
pub const Repl = struct {
    pub fn start(self: Repl, alloc: std.mem.Allocator) !void {
        _ = self; // TODO
        const stdout = std.io.getStdOut();
        const tty_config = std.io.tty.detectConfig(stdout);

        try tty_config.setColor(stdout.writer(), .green);
        try stdout.writer().print("started REPL, eval", .{});
        try tty_config.setColor(stdout.writer(), .reset);
        try stdout.writer().print(": \n", .{});

        const stdin = std.io.getStdIn().reader();
        while (true) {
            const user_input = try stdin.readUntilDelimiterAlloc(alloc, '\n', 1024);
            defer alloc.free(user_input);

            const lexer = Lexer.init(user_input);
            var parser = try Parser.init(alloc, lexer);
            defer parser.deinit();

            var ast_tree = parser.parseTree(alloc) catch |err| {
                try tty_config.setColor(stdout.writer(), .red);
                try stdout.writer().print("{any} when parsing: {s}\n", .{ err, user_input });

                parser.checkParserErrors();
                try tty_config.setColor(stdout.writer(), .reset);
                continue;
            };

            defer ast_tree.deinit();

            const ast_tree_string = try ast_tree.toString(alloc);
            defer alloc.free(ast_tree_string);

            try tty_config.setColor(stdout.writer(), .yellow);
            try stdout.writer().print("{s}\n", .{ast_tree_string});
            try tty_config.setColor(stdout.writer(), .reset);
        }
    }
};
