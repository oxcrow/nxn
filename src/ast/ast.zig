const std = @import("std");

const lib = @import("../mod.zig");

/// Abstract Syntax Tree (AST).
pub const Ast = struct {
    code: []const u8,
    ctrls: lib.ArrayList(Ctrl),
    exprs: lib.ArrayList(Expr),
    types: lib.ArrayList(Type),
    names: lib.ArrayList(Name),
    vars: lib.ArrayList(Var),
    allocator: std.mem.Allocator,

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator, code: []const u8) Self {
        return Ast{
            .code = code,
            .ctrls = lib.ArrayList(Ctrl).init(allocator),
            .exprs = lib.ArrayList(Expr).init(allocator),
            .types = lib.ArrayList(Type).init(allocator),
            .names = lib.ArrayList(Name).init(allocator),
            .vars = lib.ArrayList(Var).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: Self) void {
        self.ctrls.deinit();
        self.exprs.deinit();
        self.types.deinit();
        self.names.deinit();
        self.vars.deinit();
    }

    pub fn state(self: Self) AstState {
        return AstState{
            .ctrlsLen = self.ctrls.len(),
            .exprsLen = self.exprs.len(),
            .typesLen = self.types.len(),
            .namesLen = self.names.len(),
            .varsLen = self.vars.len(),
        };
    }

    pub fn reset(self: *Self, s: AstState) void {
        self.ctrls.array.shrinkRetainingCapacity(s.ctrlsLen);
        self.exprs.array.shrinkRetainingCapacity(s.exprsLen);
        self.types.array.shrinkRetainingCapacity(s.typesLen);
        self.names.array.shrinkRetainingCapacity(s.namesLen);
        self.vars.array.shrinkRetainingCapacity(s.varsLen);
    }
};

pub const AstState = struct {
    ctrlsLen: usize,
    exprsLen: usize,
    typesLen: usize,
    namesLen: usize,
    varsLen: usize,
};

/// Control flow graph ndoes.
pub const Ctrl = struct {
    idx: u32,
    node: union(CtrlKind) {
        Fn: struct {
            name: lib.IndexHandle(Name),
            typex: lib.IndexHandle(Type),
            block: lib.IndexHandle(Expr),
        },
        Let: struct {
            name: lib.IndexHandle(Name),
            vars: lib.IndexSlice(Var),
            expr: lib.IndexHandle(Expr),
        },
        None: void,
    },

    const Self = @This();

    pub fn kind(self: Self) CtrlKind {
        switch (self.node) {
            .Fn => .Fn,
            .Let => .Let,
            .None => .None,
        }
    }
};

/// Expression nodes.
pub const Expr = struct {
    idx: u32,
    node: union(ExprKind) {
        Terminal: void,
        Invoke: void,
        Block: void,
        Name: Name,
        None: void,
    },
};

///  Variable nodes.
pub const Var = struct {
    idx: u32,
    name: []const u8,
    state: State,
};

/// Type nodes.
pub const Type = struct {
    idx: u32,
    node: union {
        Void: void,
        Int: void,
        Float: void,
    },
};

/// Name node.
pub const Name = struct {
    idx: u32,
    string: []const u8,
};

pub const CtrlKind = enum {
    Fn,
    Let,
    None,
};

pub const ExprKind = enum {
    Terminal,
    Invoke,
    Block,
    Name,
    None,
};

pub const State = enum {
    Con,
    Mut,
    Set,
};
