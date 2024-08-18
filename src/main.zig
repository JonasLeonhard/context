const std = @import("std");
const token = @import("token.zig");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const ast = @import("ast.zig");
const Tree = ast.Tree;
const NodeIndex = ast.NodeIndex;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    // TODO
    _ = alloc;
}
