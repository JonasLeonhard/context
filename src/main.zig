const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Repl = @import("Repl.zig");

pub fn main() !void {
    var gpa_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_allocator.deinit();
    const gpa = gpa_allocator.allocator();

    // REPL
    const repl = Repl{};
    // TODO: switch the eval type here...
    // try repl.start_eval_to_ast(gpa);
    try repl.start_eval(gpa);
}
