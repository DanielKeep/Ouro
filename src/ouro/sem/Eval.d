/**
    Semantic Evaluator.

    Takes a semantic tree and computes the result.

    TODO: Rewrite to use a trampoline to support tail calls.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Eval;

import ouro.sit.Visitor;

import InvokeFn = ouro.sem.InvokeFn;
import Sit      = ouro.sit.Nodes;

struct Context
{
    alias Sit.FunctionValue.Host.EvalContext    EvalContext;
    alias void function(Sit.UnfixedValue)       OnUnfixed;

    EvalContext evalCtx = EvalContext.Runtime;
    FixScope* fixScope;
    OnUnfixed onUnfixed = &onUnfixedDefault;

    static void onUnfixedDefault(Sit.UnfixedValue value)
    {
        assert( false, "unexpected unfixed value "~value.ident );
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
                assert( fvp !is null, "expected fixed value, didn't find it" );
                return *fvp;
            }
            fs = fs.parent;
        }

        // Oh damn it all to hell
        onUnfixed(uv);
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
        auto result = visitBase(node, ctx);
        assert( result !is null, "expected non-null result node" );
        auto resultValue = cast(Sit.Value) result;
        assert( resultValue !is null, "expected Value result" );
        return resultValue;
    }

    Sit.FunctionValue visitFn(Sit.Node node, Context ctx)
    {
        auto result = visitValue(node, ctx);
        auto resultFn = cast(Sit.FunctionValue) result;
        assert( resultFn !is null, "expected Function result" );
        return resultFn;
    }

    override Sit.Value visit(Sit.Module node, Context ctx)
    {
        assert( false, "dunno wat dew" );
    }

    override Sit.Value visit(Sit.CallExpr node, Context ctx)
    {
        auto fn = visitFn(node.funcExpr, ctx);

        // Evaluate arguments
        auto args = new Sit.Value[node.args.length];
        size_t argIdx = 0;

        void addArg(Sit.Value value, bool explode)
        {
            if( ! explode )
            {
                if( argIdx < args.length )
                    args[argIdx++] = value;
                else
                    args ~= value;
            }
            else
            {
                auto listValue = cast(Sit.ListValue) value;
                assert( listValue !is null, node.astNode.loc.toString
                        ~ ": can only explode a List; got a "
                        ~ value.classinfo.name );

                foreach( argValue ; listValue.elemValues )
                    addArg(argValue, false);
            }
        }

        foreach( i,nodeArg ; node.args )
            addArg(visitValue(nodeArg.expr, ctx), nodeArg.explode);

        return InvokeFn.invokeFn(fn, args);
    }

    Sit.Value invokeExprFn(Sit.FunctionValue fn, Sit.Value[] args,
            Context.EvalContext evalCtx = Context.EvalContext.Runtime)
    {
        assert( fn.expr !is null );

        if( fn.args.length != args.length )
            assert( false, "argument number mismatch" );

        // Construct scope for call
        FixScope fs;
        fs.scop = fn.scop;

        foreach( i,arg ; args )
            fs.values[fn.args[i].ident] = arg;

        // Construct new context for call
        Context subCtx;
        subCtx.evalCtx = evalCtx;
        subCtx.fixScope = &fs;

        auto result = visitBase(fn.expr, subCtx);
        return result;
    }

    override Sit.Value visit(Sit.ArgumentValue node, Context ctx)
    {
        return ctx.fixValue(node);
    }

    override Sit.Value visit(Sit.DeferredValue node, Context ctx)
    {
        return ctx.fixValue(node);
    }

    override Sit.Value visit(Sit.QuantumValue node, Context ctx)
    {
        return ctx.fixValue(node);
    }

    override Sit.Value visit(Sit.AstQuoteValue node, Context ctx)
    {
        return node;
    }

    override Sit.Value visit(Sit.FunctionValue node, Context ctx)
    {
        return node;
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

    override Sit.Value visit(Sit.NumberValue node, Context ctx)
    {
        return node;
    }
}

