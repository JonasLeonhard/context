const std = @import("std");

pub fn main() !void {
    std.debug.print("All your {d} are belong to us.\n", .{"codebase"});
}
