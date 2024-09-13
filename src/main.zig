const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tree = ast.Tree;
const NodeIndex = ast.NodeIndex;

const Repl = @import("repl.zig").Repl;

pub fn main() !void {
    var gpa_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa_allocator.deinit();
    const gpa = gpa_allocator.allocator();

    // REPL
    const repl = Repl{};
    try repl.start(gpa);
}
