/**
    Compile-time Folder.

    Works like the regular evaluator, except that it folds expressions instead
    of values.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Fold;

import ouro.sem.Abort;
import ouro.sit.Visitor;

import Eval     = ouro.sem.Eval;
import InvokeFn = ouro.sem.InvokeFn;
import Sit      = ouro.sit.Nodes;

alias Eval.Context Context;
alias Sit.EvalContext.Compile CompileCtx;

class FoldVisitor : Visitor!(Sit.Expr, Context)
{
    Sit.Value visitValue(Sit.Node node, Context ctx)
    {
        auto result = visitBase(node, ctx);
        assert( result !is null, "expected non-null result node" );
        auto resultValue = cast(Sit.Value) result;
        assert( resultValue !is null, "expected Value result" );
        return resultValue;
    }

    override Sit.Expr visit(Sit.Module node, Context ctx)
    {
        assert( false, "nyi; too lazy" );
    }

    protected static bool foldedValue(Sit.Expr node)
    {
        if( auto fv = cast(Sit.FunctionValue) node )
        {
            if( fv.enclosedValues.length > 0 )
                return false;
            return true;
        }

        return (cast(Sit.Value) node !is null)
            && (cast(Sit.RuntimeValue) node is null)
            && (cast(Sit.EnclosedValue) node is null)
            && (cast(Sit.UnfixedValue) node is null);
    }

    override Sit.Expr visit(Sit.CallExpr node, Context ctx)
    {
        // Process function
        auto funcExpr = visitBase(node.funcExpr, ctx);
        auto callable = cast(Sit.CallableValue) funcExpr;

        // Process arguments
        auto args = new Sit.CallArg[node.args.length];
        size_t argIdx = 0;

        // If set to false, at least one argument couldn't be statically
        // evaluated.
        bool argsAreValues = true;

        void addArg(Sit.Expr expr, bool explode)
        {
            // Have to check for both non-value expressions and runtime
            // values.
            argsAreValues &= foldedValue(expr);

            if( ! explode )
            {
                if( argIdx < args.length )
                    args[argIdx++] = Sit.CallArg(expr, false);
                else
                    args ~= Sit.CallArg(expr, false);
            }
            else
            {
                if( auto le = cast(Sit.ListExpr) expr )
                {
                    foreach( e ; le.elemExprs )
                        addArg(e, false);
                }
                else if( auto lv = cast(Sit.ListValue) expr )
                {
                    foreach( e ; lv.elemValues )
                        addArg(e, false);
                }
                else if( auto v = cast(Sit.Value) expr
                        && cast(Sit.UnfixedValue) expr is null
                        && cast(Sit.EnclosedValue) expr is null )
                {
                    assert( false, node.astNode.loc.toString
                            ~ ": can only explode a List; got a "
                            ~ expr.classinfo.name );
                }
                else
                {
                    if( argIdx < args.length )
                        args[argIdx++] = Sit.CallArg(expr, true);
                    else
                        args ~= Sit.CallArg(expr, true);
                }
            }
        }

        foreach( nodeArg ; node.args )
            addArg(visitBase(nodeArg.expr, ctx), nodeArg.explode);

        // We can only actually make this call if the function and *all* of
        // the arguments are values.  We also need to make sure the callable
        // is fully folded.
        auto canCall = (callable !is null) && foldedValue(callable)
            && callable.trueFn.foldable && argsAreValues;

        if( canCall )
        {
            // We have to convert the array of expressions into an array of
            // values.  It should just be a quick cast of all elements.
            auto argValues = new Sit.Value[args.length];

            foreach( i,arg ; args )
            {
                argValues[i] = cast(Sit.Value) arg.expr;
                assert( argValues[i] !is null );
            }

            // Ok, call that sucker!
            Sit.Value result;
            try
            {
                result = InvokeFn.invoke(callable, argValues, CompileCtx);
            }
            catch( EarlyCallAbort )
            {
                // Yes: really, really ignore the exception.  This isn't so
                // much a failure as a "not yet".  The only reason there's
                // nothing here is that we aren't allowed to return from
                // inside a catch block.  Honest!
            }

            // If the call succeeded, return.
            if( result !is null )
                return result;

            // Otherwise, we got a delayed call.
            auto callArgs = new Sit.CallArg[argValues.length];

            foreach( i,argValue ; argValues )
                callArgs[i] = Sit.CallArg(argValue, false);

            return new Sit.CallExpr(node.astNode, callable, callArgs);
        }
        else
        {
            return new Sit.CallExpr(node.astNode, funcExpr, args);
        }
    }

    override Sit.Expr visit(Sit.ArgumentValue node, Context ctx)
    {
        /*
            If we got this, then one of two things has happened:

            1.  The semantic analysis went crazy and inserted an
                ArgumentValue outside a function, or

            2.  We've been asked to fold a function body.

            Discounting #1, we should just return the value un-resolved.
        */
        return node;
    }

    override Sit.Expr visit(Sit.EnclosedValue node, Context ctx)
    {
        // See above for ArgumentValue.
        return node;
    }

    override Sit.Expr visit(Sit.DeferredValue node, Context ctx)
    {
        return ctx.fixValue(node);
    }

    override Sit.Expr visit(Sit.QuantumValue node, Context ctx)
    {
        return ctx.fixValue(node);
    }

    override Sit.Expr visit(Sit.RuntimeValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.AstQuoteValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.ClosureValue node, Context ctx)
    {
        auto values = new Sit.Value[node.values.length];

        foreach( i,encValue ; node.values )
        {
            auto fixValue = visitValue(encValue, ctx);
            assert( cast(Sit.UnfixedValue) fixValue is null );
            values[i] = fixValue;
        }

        return new Sit.ClosureValue(node.astNode, node.fn, values);
    }

    // Map from un-folded to folded functions.
    Sit.FunctionValue[Sit.FunctionValue] functionFoldMap;

    override Sit.Expr visit(Sit.FunctionValue node, Context ctx)
    {
        //+
        // Are we folding function bodies?
        if( ctx.foldFunctionBodies )
        {
            // If this node is in the set of folded functions, return
            // immediately; it's been processed before.
            if( node.folded )
                return node;

            // If this node is in the folded function map, return the
            // associated node.
            if( auto foldedPtr = node in functionFoldMap )
                return *foldedPtr;

            // First, fold the body if we can.
            if( node.expr !is null )
            {
                auto foldExpr = visitBase(node.expr, ctx);
                auto foldNode = new Sit.FunctionValue(node, foldExpr);
                functionFoldMap[node] = foldNode;

                // Replace the current function node with a new one.
                node = new Sit.FunctionValue(node, foldExpr);
                node = foldNode;
            }
        }
        // +/

        if( node.enclosedValues.length == 0 )
            // No need for a closure.
            return node;

        // Make a closure, but make sure that all enclosed values were folded.
        // If we get an unfolded enclosed value, don't create a closure.
        auto closureValues = new Sit.Value[node.enclosedValues.length];
        {
            auto onUnfixedOld = ctx.onUnfixed;
            ctx.onUnfixed = function void(Sit.UnfixedValue v) {};
            scope(exit) ctx.onUnfixed = onUnfixedOld;

            foreach( i,ev ; node.enclosedValues )
            {
                auto cv = ctx.fixValue(ev.value);
                if( ! foldedValue(cv) )
                    // Blargh; give up
                    return node;

                closureValues[i] = cv;
            }
        }
        
        return new Sit.ClosureValue(node.astNode, node, closureValues);
    }

    override Sit.Expr visit(Sit.ListExpr node, Context ctx)
    {
        auto elemExprs = new Sit.Expr[node.elemExprs.length];
        bool allValues = true;

        foreach( i,elemExpr ; node.elemExprs )
        {
            auto expr = visitBase(elemExpr, ctx);
            elemExprs[i] = expr;
            allValues &= foldedValue(expr);
        }

        if( allValues )
        {
            auto elemValues = new Sit.Value[node.elemExprs.length];

            foreach( i,elemExpr ; elemExprs )
            {
                elemValues[i] = cast(Sit.Value) elemExpr;
                assert( elemValues[i] !is null );
            }

            return new Sit.ListValue(node.astNode, elemValues);
        }
        else
        {
            return new Sit.ListExpr(node.astNode, elemExprs);
        }
    }

    override Sit.Expr visit(Sit.ListValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.LogicalValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.MapExpr node, Context ctx)
    {
        auto kvpExprs = new Sit.ExprKVP[node.kvps.length];
        auto allValues = true;

        foreach( i,kvpExpr ; node.kvps )
        {
            auto k = visitBase(kvpExpr.key, ctx);
            auto v = visitBase(kvpExpr.value, ctx);
            allValues &= foldedValue(k) && foldedValue(v);

            kvpExprs[i] = Sit.ExprKVP(kvpExpr.loc, k, v);
        }

        if( allValues )
        {
            auto kvpValues = new Sit.ValueKVP[node.kvps.length];
            
            foreach( i,kvpExpr ; kvpExprs )
            {
                auto k = cast(Sit.Value) kvpExpr.key;
                auto v = cast(Sit.Value) kvpExpr.value;
                assert( k !is null && v !is null );
                kvpValues[i] = Sit.ValueKVP(kvpExpr.loc, k, v);
            }

            return new Sit.MapValue(node.astNode, kvpValues);
        }
        else
        {
            return new Sit.MapExpr(node.astNode, kvpExprs);
        }
    }

    override Sit.Expr visit(Sit.MapValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.ModuleValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.NilValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.StringValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.SymbolValue node, Context ctx)
    {
        return node;
    }

    override Sit.Expr visit(Sit.NumberValue node, Context ctx)
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

