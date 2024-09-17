const std = @import("std");

/// const Result = @import("result.zig").Result;
///
/// fn returns_result() Result(u32) {
///    // return Result(u32){ .Err = error.SomeError };
///    return Result(u32){ .Ok = 32 };
///}
///
/// _ = try returns_result().unwrap();
pub fn Result(comptime T: type) type {
    return union(enum) {
        const Self = @This();

        Ok: T,
        Err: anyerror,

        pub fn unwrap(self: Self) !T {
            return switch (self) {
                .Ok => |value| value,
                .Err => |err| return err,
            };
        }
        pub fn deinit(self: Self) void {
            switch (self) {
                .err => {},
                .ok => |value| {
                    if (comptime std.meta.hasFn("deinit")(T)) {
                        value.deinit();
                    }
                },
            }
        }
    };
}
