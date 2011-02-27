/**
    SIT Visitor base.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sit.Visitor;

import Sit = ouro.sit.Nodes;

private
{
    alias Sit.Nodes SitClassList;

    char[] genDispatch(char[] callSuffix = "")
    {
        char[] r;

        foreach( name ; SitClassList )
        {
            r ~= "if( auto n = cast(Sit."~name~") node ) return visit(n"
                ~ callSuffix ~ ");\n";
        }

        return r;
    }
}

abstract class Visitor(Result = void, Arg = void)
{
    static if( is( Arg == void ) )
    {
        final Result visitBase(Sit.Node node)
        {
            mixin(genDispatch());

            return defaultVisit(node);
        }

        protected Result defaultVisitResult()
        {
            static if( is( Result == void ) )
                return;
            else
                return Result.init;
        }

        Result defaultVisit(Sit.Node node)
        {
            assert(false, "missing visit for "~node.classinfo.name);
        }

        Result visitScope(Sit.Scope scop)
        {
            foreach( k,v ; scop.entries )
                visitBase(v);
            return defaultVisitResult;
        }

        Result visit(Sit.Module node)
        {
            visitScope(node.scop);
            foreach( stmt ; node.stmts )
                visitBase(stmt.expr);
            return defaultVisitResult;
        }

        Result visit(Sit.CallExpr node)
        {
            visitBase(node.funcExpr);
            foreach( arg ; node.args )
                visitBase(arg.expr);
            return defaultVisitResult;
        }

        Result visit(Sit.ArgumentValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.ClosureValue node)
        {
            return visitBase(node.value);
        }

        Result visit(Sit.DeferredValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.QuantumValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.AstQuoteValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.FunctionValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.ListExpr node)
        {
            foreach( elemExpr ; node.elemExprs )
                visitBase(elemExpr);
            return defaultVisitResult;
        }

        Result visit(Sit.ListValue node)
        {
            foreach( elemValue ; node.elemValues )
                visitBase(elemValue);
            return defaultVisitResult;
        }

        Result visit(Sit.LogicalValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.MapExpr node)
        {
            foreach( kvp ; node.kvps )
            {
                visitBase(kvp.key);
                visitBase(kvp.value);
            }
            return defaultVisitResult;
        }

        Result visit(Sit.MapValue node)
        {
            foreach( kvp ; node.kvps )
            {
                visitBase(kvp.key);
                visitBase(kvp.value);
            }
            return defaultVisitResult;
        }

        Result visit(Sit.ModuleValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.NilValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.StringValue node)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.NumberValue node)
        {
            return defaultVisitResult;
        }
    }
    else
    {
        final Result visitBase(Sit.Node node, Arg arg)
        {
            mixin(genDispatch(", arg"));

            return defaultVisit(node, arg);
        }

        protected Result defaultVisitResult()
        {
            static if( is( Result == void ) )
                return;
            else
                return Result.init;
        }

        Result defaultVisit(Sit.Node node, Arg arg)
        {
            assert(false, "missing visit for "
                    ~ (node !is null ? node.classinfo.name : "(null)"));
        }

        Result visitScope(Sit.Scope scop, Arg arg)
        {
            foreach( k,v ; scop.entries )
                visitBase(v, arg);
            return defaultVisitResult;
        }

        Result visit(Sit.Module node, Arg arg)
        {
            visitScope(node.scop, arg);
            foreach( stmt ; node.stmts )
                visitBase(stmt.expr, arg);
            return defaultVisitResult;
        }

        Result visit(Sit.CallExpr node, Arg arg)
        {
            visitBase(node.funcExpr, arg);
            foreach( nodeArg ; node.args )
                visitBase(nodeArg.expr, arg);
            return defaultVisitResult;
        }

        Result visit(Sit.ArgumentValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.ClosureValue node, Arg arg)
        {
            return visitBase(node.value, arg);
        }

        Result visit(Sit.DeferredValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.QuantumValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.AstQuoteValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.FunctionValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.ListExpr node, Arg arg)
        {
            foreach( elemExpr ; node.elemExprs )
                visitBase(elemExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Sit.ListValue node, Arg arg)
        {
            foreach( elemExpr ; node.elemValues )
                visitBase(elemExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Sit.LogicalValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.MapExpr node, Arg arg)
        {
            foreach( kvp ; node.kvps )
            {
                visitBase(kvp.key, arg);
                visitBase(kvp.value, arg);
            }
            return defaultVisitResult;
        }

        Result visit(Sit.MapValue node, Arg arg)
        {
            foreach( kvp ; node.kvps )
            {
                visitBase(kvp.key, arg);
                visitBase(kvp.value, arg);
            }
            return defaultVisitResult;
        }

        Result visit(Sit.ModuleValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.NilValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.StringValue node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Sit.NumberValue node, Arg arg)
        {
            return defaultVisitResult;
        }
    }
}

