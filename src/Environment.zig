const std = @import("std");
const Object = @import("object.zig").Object;

const Environment = @This();

store: std.StringHashMap(Object),
arena: std.heap.ArenaAllocator,

pub fn init(alloc: std.mem.Allocator) Environment {
    return Environment{
        .store = std.StringHashMap(Object).init(alloc),
        .arena = std.heap.ArenaAllocator.init(alloc),
    };
}

pub fn deinit(self: *Environment) void {
    self.store.deinit();
    self.arena.deinit();
}

pub fn get(self: Environment, name: []const u8) ?Object {
    return self.store.get(name);
}

pub fn set(self: *Environment, name: []const u8, val: Object) !Object {
    const owned_name = try self.arena.allocator().dupe(u8, name);
    try self.store.put(owned_name, val);
    return val;
}
