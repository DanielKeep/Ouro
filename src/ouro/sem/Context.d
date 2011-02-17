/**
    Semantic context.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Context;

import Sit = ouro.sit.Nodes;

struct Context
{
    alias Sit.Value delegate(char[] name) BuiltinFn;

    Sit.Scope scop;
    Sit.Stmt* stmt;
    BuiltinFn builtinFn;

    static Context opCall(Sit.Scope scop, Sit.Stmt* stmt, BuiltinFn builtinFn)
    {
        Context r;
        r.scop = scop;
        r.stmt = stmt;
        r.builtinFn = builtinFn;
        return r;
    }

    static Context opCall(Sit.Scope scop, BuiltinFn builtinFn = null)
    {
        return Context(scop, null, builtinFn);
    }

    static Context opCall(ref Context ctx)
    {
        return Context(ctx.scop, ctx.stmt, ctx.builtinFn);
    }

    Sit.Value builtin(char[] name)
    {
        auto v = builtinFn(name);
        assert( v !is null );
        return v;
    }

    Sit.FunctionValue builtinFunction(char[] name)
    {
        auto v = cast(Sit.FunctionValue) builtin(name);
        assert( v !is null );
        return v;
    }
}

