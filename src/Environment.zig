const std = @import("std");
const Object = @import("object.zig").Object;

const Environment = @This();

store: std.StringHashMap(Object),
arena: std.heap.ArenaAllocator,
outer: ?*Environment,

pub fn init(alloc: std.mem.Allocator) Environment {
    return Environment{
        .store = std.StringHashMap(Object).init(alloc),
        .arena = std.heap.ArenaAllocator.init(alloc),
        .outer = null,
    };
}

pub fn deinit(self: *Environment) void {
    self.store.deinit();
    self.arena.deinit();
}

pub fn get(self: Environment, name: []const u8) ?Object {
    const value = self.store.get(name);

    if (value == null) {
        if (self.outer) |outer| {
            return outer.get(name);
        }
        return null;
    }

    return value;
}

pub fn set(self: *Environment, name: []const u8, val: Object) !Object {
    const owned_name = try self.arena.allocator().dupe(u8, name);
    try self.store.put(owned_name, val);
    return val;
}

pub fn newEnclosedEnvironment(outer: *Environment) Environment {
    var env = Environment.init(outer.arena.allocator());
    env.outer = outer;
    return env;
}
