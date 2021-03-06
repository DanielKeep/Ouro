/**
    Semantic Evaluator.

    Takes a semantic tree and computes the result.

    TODO: Rewrite to use a trampoline to support tail calls.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Eval;

import ouro.sem.Abort;
import ouro.sem.Formatters;
import ouro.sit.Visitor;

import InvokeFn = ouro.sem.InvokeFn;
import Sit      = ouro.sit.Nodes;
debug
    import Repr     = ouro.sit.ReprVisitor;

struct Context
{
    alias void function(Sit.UnfixedValue)       OnUnfixed;

    auto evalCtx = Sit.EvalContext.Runtime;
    FixScope* fixScope;
    OnUnfixed onUnfixed = &onUnfixedDefault;
    void delegate(Sit.Node) dumpNode;

    bool foldFunctionBodies = false;

    static void onUnfixedDefault(Sit.UnfixedValue value)
    {
        NonFatalAbort.throwForUnfixed(value);
    }

    Sit.Value fixValue(Sit.Value value)
    {
        auto uv = cast(Sit.UnfixedValue) value;
        if( uv is null )
            return value;

        // Try to resolve the value
        if( auto resolvable = cast(Sit.Resolvable) uv )
        {
            auto rv = resolvable.resolve;
            if( null is cast(Sit.UnfixedValue) rv )
                // Fixed value
                return rv;
        }

        // Look for a fixed value
        auto fs = fixScope;
        while( fs !is null )
        {
            if( fs.scop is uv.scop )
            {
                auto fvp = (uv.ident in fs.values);
                assert( fvp !is null, "expected fixed value for "
                        ~ uv.toString ~ " " ~ uv.ident
                        ~ ", didn't find it" );
                return *fvp;
            }
            fs = fs.parent;
        }

        // Oh damn it all to hell
        debug(DumpUnfixed) if( dumpNode !is null )
            dumpNode(value);
        onUnfixed(uv);

        // Return un-fixed.
        return value;
    }
}

struct FixScope
{
    Sit.Scope scop;
    Sit.Value[char[]] values;
    FixScope* parent;
}

class EvalVisitor : Visitor!(Sit.Value, Context)
{
    protected
    {
        // TODO: error generation stuffs
    }

    Sit.Value visitValue(Sit.Node node, Context ctx)
    {
        Sit.Value r;
        Sit.TailCall tail;

        try
        {
            r = visitValueTail(node, ctx);
        }
        catch( Sit.TailCall tc )
        {
            tail = tc;
        }

        if( tail !is null )
            return InvokeFn.invoke(tail.callable, tail.args, ctx.evalCtx);
        else
        {
            assert( r !is null );
            return r;
        }
    }

    Sit.Value visitValueTail(Sit.Node node, Context ctx)
    {
        auto result = visitBase(node, ctx);
        assert( result !is null, "expected non-null result node" );
        auto resultValue = cast(Sit.Value) result;
        assert( resultValue !is null, "expected Value result" );
        return resultValue;
    }

    Sit.CallableValue visitCallable(Sit.Node node, Context ctx,
            out Sit.FunctionValue fn)
    {
        auto result = visitValue(node, ctx);
        auto callable = cast(Sit.CallableValue) result;
        assert( callable !is null, "expected callable result" );

        if( auto cv = cast(Sit.ClosureValue) callable )
            fn = cv.fn;
        else if( auto fv = cast(Sit.FunctionValue) callable )
            fn = fv;
        else
            assert( false );

        return callable;
    }

    override Sit.Value visit(Sit.Module node, Context ctx)
    {
        assert( false, "dunno wat dew" );
    }

    Sit.Value evalModule(Sit.Module node, Context ctx = Context.init)
    {
        if( node.stmts.length == 0 )
            return new Sit.NilValue(null);

        Sit.Value result;

        foreach( stmt ; node.stmts )
            result = visitValue(stmt.value, ctx);

        return result;
    }

    Sit.Value evalExpr(Sit.Expr node, Context ctx = Context.init)
    {
        return visitValue(node, ctx);
    }

    override Sit.Value visit(Sit.CallExpr node, Context ctx)
    {
        Sit.FunctionValue fn;
        auto callable = visitCallable(node.funcExpr, ctx, fn);

        assert( fn !is null );

        // Evaluate arguments
        Sit.Value[] args;

        void addArg(size_t i, Sit.Value value, bool explode)
        {
            if( ! explode )
            {
                if( i >= args.length )
                    args.length = i+1;
                if( args[i] !is null )
                    assert( false );
                args[i] = value;
            }
            else
            {
                auto listValue = cast(Sit.ListValue) value;
                assert( listValue !is null, node.astNode.loc.toString
                        ~ ": can only explode a List; got a "
                        ~ value.classinfo.name );

                foreach( j,argValue ; listValue.elemValues )
                    addArg(i+j, argValue, false);
            }
        }

        foreach( i,nodeArg ; node.args )
            addArg(args.length,
                    visitValue(nodeArg.expr, ctx), nodeArg.explode);

        foreach( ident,namedArg ; node.namedArgs )
        {
            size_t i = ~0;
            foreach( argIdx,arg ; fn.args )
                if( arg.ident == ident )
                {
                    i = argIdx;
                    break;
                }
            assert( i != ~0, "unknown argument "~ident );

            addArg(i, visitValue(namedArg.expr, ctx), namedArg.explode);
        }

        return Sit.TailCall.call(callable, args);
    }

    Sit.Value invokeExprFn(Sit.FunctionValue fn,
            Sit.Value[] args, Sit.Value[] closureValues,
            Sit.EvalContext evalCtx = Sit.EvalContext.Runtime)
    {
        assert( fn.expr !is null );

        if( fn.args.length != args.length )
            assert( false, "argument number mismatch" );

        // Construct scope for call
        FixScope fsBuffer;
        FixScope* fs;

        // Handle closure values
        if( closureValues.length > 0 )
        {
            assert( closureValues.length == fn.enclosedValues.length );
            FixScope*[Sit.Scope] scopeMap;

            // What we're doing here is constructing a chain of FixScopes
            // which contain the fixed values for the enclosed values, matched
            // against the closure values.  Or something.

            foreach( i,ev ; fn.enclosedValues )
            {
                if( !(ev.value.scop in scopeMap) )
                {
                    auto newFs = new FixScope;
                    newFs.scop = ev.value.scop;
                    newFs.parent = fs;
                    scopeMap[newFs.scop] = newFs;
                    fs = newFs;
                }

                auto evFs = scopeMap[ev.value.scop];
                evFs.values[ev.value.ident] = closureValues[i];
            }
        }

        // Add arguments
        fsBuffer.parent = fs;
        fs = &fsBuffer;
        fs.scop = fn.scop;

        foreach( i,arg ; args )
            fs.values[fn.args[i].ident] = arg;

        // Construct new context for call
        Context subCtx;
        subCtx.evalCtx = evalCtx;
        subCtx.fixScope = fs;
        debug
        {
            auto repr = Repr.ReprVisitor.forStderr;
            subCtx.dumpNode = (Sit.Node node)
            {
                repr.visitBase(node, true);
                return;
            };
        }

        auto result = visitBase(fn.expr, subCtx);
        return result;
    }

    override Sit.Value visit(Sit.ArgumentValue node, Context ctx)
    {
        auto v = ctx.fixValue(node);
        return visitBase(v, ctx);
    }

    override Sit.Value visit(Sit.EnclosedValue node, Context ctx)
    {
        auto v = ctx.fixValue(node.value);
        return visitBase(v, ctx);
    }

    override Sit.Value visit(Sit.DeferredValue node, Context ctx)
    {
        auto v = ctx.fixValue(node);
        return visitBase(v, ctx);
    }

    override Sit.Value visit(Sit.QuantumValue node, Context ctx)
    {
        auto v = ctx.fixValue(node);
        return visitBase(v, ctx);
    }

    override Sit.Value visit(Sit.RuntimeValue node, Context ctx)
    {
        auto v = node.resolve;

        if( v is node )
            // Attempt to fix.
            v = node.fixValue({ return visitBase(node.expr, ctx); });

        return v;
    }

    override Sit.Value visit(Sit.AstQuoteValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.ClosureValue node, Context ctx)
    {
        auto values = new Sit.Value[node.values.length];

        foreach( i,encValue ; node.values )
        {
            auto fixValue = visitValue(encValue, ctx);
            assert( (cast(Sit.UnfixedValue) fixValue) is null );
            values[i] = fixValue;
        }

        return new Sit.ClosureValue(node.astNode, node.fn, values);
    }

    override Sit.Value visit(Sit.FunctionValue node, Context ctx)
    {
        if( node.enclosedValues.length == 0 )
            // No need for a closure.
            return node;

        // Make a closure
        auto closureValues = new Sit.Value[node.enclosedValues.length];
        foreach( i,ev ; node.enclosedValues )
            closureValues[i] = ctx.fixValue(ev.value);

        return new Sit.ClosureValue(node.astNode, node, closureValues);
    }

    override Sit.Value visit(Sit.ListExpr node, Context ctx)
    {
        auto elemValues = new Sit.Value[node.elemExprs.length];

        foreach( i,elemExpr ; node.elemExprs )
            elemValues[i] = visitValue(elemExpr, ctx);

        return new Sit.ListValue(node.astNode, elemValues);
    }

    override Sit.Value visit(Sit.ListValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.LogicalValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.MapExpr node, Context ctx)
    {
        auto kvps = new Sit.ValueKVP[node.kvps.length];

        foreach( i,kvp ; node.kvps )
            kvps[i] = Sit.ValueKVP(kvp.loc,
                    visitValue(kvp.key, ctx),
                    visitValue(kvp.value, ctx));

        return new Sit.MapValue(node.astNode, kvps);
    }

    override Sit.Value visit(Sit.MapValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.ModuleValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.NilValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.StringValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.SymbolValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.NumberValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.RangeValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.HostObjectValue node, Context ctx)
    {
        return node;
    }
}

