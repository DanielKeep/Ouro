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
    Sit.EnclosedValue[] enclosedValues;
    void delegate(Sit.Node) dumpNode;

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

    Context dup()
    {
        return *this;
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

    void clearEnclosedValues()
    {
        enclosedValues = null;
    }

    void addEnclosedValue(Sit.EnclosedValue value)
    {
        foreach( ev ; enclosedValues )
            if( ev.value is value.value )
                return;

        enclosedValues ~= value;
    }

    void mergeEnclosedValues(ref Context fromCtx)
    {
        /*
            This function needs to work out what closure values to take from a
            sub-context.

            In general, we need to omit values which are defined inside the
            current enclosed scope.  The way we do this is to walk the scope
            chain (starting with the sub-context's scope).  For each scope, we
            drop all closure values defined in it.

            We stop when we attempt to go up a scope and find the current one
            is enclosed.  The remaining closure values are appended to the
            current context's list.
         */

        auto curScop = fromCtx.scop;
        auto enclosed = fromCtx.enclosedValues;
        bool lastScopEnclosed = false;

        while( curScop !is null && ! lastScopEnclosed )
        {
            bool dropThis = false;

            for( size_t i=0;
                 i < enclosed.length;
                 dropThis = false, i += (dropThis ? 0 : 1) )
            {
                auto cv = enclosed[i];
                dropThis = cv.value.scop is curScop;
                if( dropThis )
                {
                    // Move a value from the end of the array up into this
                    // spot (swap remove without the swap bit).
                    auto endIdx = enclosed.length-1;
                    enclosed[i] = enclosed[endIdx];
                    enclosed = enclosed[0..endIdx];
                }
            }

            lastScopEnclosed = curScop.enclosed;
            curScop = curScop.parent;
        }

        if( enclosed.length > 0 )
            this.enclosedValues ~= enclosed;
    }
}

