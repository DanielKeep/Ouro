/**
    AST Visitor base.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.ast.Visitor;

import Ast = ouro.ast.Nodes;

private
{
    const char[][] AstClassList =
    [
        "Module"[],
        "ImportStmt",
        "LetExprStmt",
        "LetFuncStmt",
        "ExprStmt",
        "RewrittenExpr",
        "BinaryExpr",
        "InfixFuncExpr",
        "PrefixExpr",
        "PostfixFuncExpr",
        "NumberExpr",
        "StringExpr",
        "LogicalExpr",
        "NilExpr",
        "ListExpr",
        "MapExpr",
        "LambdaExpr",
        "ExplodeExpr",
        "CallExpr",
        "VariableExpr",
        "RangeExpr",
        "AstQuoteExpr",
        "AstQuasiQuoteExpr",
        "AstQQSubExpr",
        "LetExpr",
        "ImportExpr",
        "TernaryExpr",
    ];

    char[] genDispatch(char[] callSuffix = "")
    {
        char[] r;

        foreach( name ; AstClassList )
        {
            r ~= "if( auto n = cast(Ast."~name~") node ) return visit(n"
                ~ callSuffix ~ ");\n";
        }

        return r;
    }
}

abstract class Visitor(Result = void, Arg = void)
{
    static if( is( Arg == void ) )
    {
        final Result visitBase(Ast.Node node)
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

        Result defaultVisit(Ast.Node node)
        {
            assert(false, "missing visit for "~node.classinfo.name);
        }

        Result visit(Ast.Module node)
        {
            foreach( stmt ; node.stmts )
                visitBase(stmt);
            return defaultVisitResult;
        }

        Result visit(Ast.ImportStmt node)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.LetExprStmt node)
        {
            return visitBase(node.expr);
        }

        Result visit(Ast.LetFuncStmt node)
        {
            return visitBase(node.expr);
        }

        Result visit(Ast.ExprStmt node)
        {
            return visitBase(node.expr);
        }

        Result visit(Ast.RewrittenExpr node)
        {
            // Note: original is not visited
            return visitBase(node.rewrite);
        }

        Result visit(Ast.BinaryExpr node)
        {
            visitBase(node.lhs);
            visitBase(node.rhs);
            return defaultVisitResult;
        }

        Result visit(Ast.InfixFuncExpr node)
        {
            visitBase(node.func);
            visitBase(node.lhs);
            visitBase(node.rhs);
            return defaultVisitResult;
        }

        Result visit(Ast.PrefixExpr node)
        {
            visitBase(node.subExpr);
            return defaultVisitResult;
        }

        Result visit(Ast.PostfixFuncExpr node)
        {
            visitBase(node.func);
            visitBase(node.subExpr);
            return defaultVisitResult;
        }

        Result visit(Ast.NumberExpr node)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.StringExpr node)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.LogicalExpr node)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.NilExpr node)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.ListExpr node)
        {
            foreach( elem ; node.elemExprs )
                visitBase(elem);
            return defaultVisitResult;
        }

        Result visit(Ast.MapExpr node)
        {
            foreach( kvp ; node.keyValuePairs )
            {
                visitBase(kvp.key);
                visitBase(kvp.value);
            }
            return defaultVisitResult;
        }

        Result visit(Ast.LambdaExpr node)
        {
            visitBase(node.expr);
            return defaultVisitResult;
        }

        Result visit(Ast.ExplodeExpr node)
        {
            visitBase(node.subExpr);
            return defaultVisitResult;
        }

        Result visit(Ast.CallExpr node)
        {
            visitBase(node.funcExpr);
            foreach( arg ; node.argExprs )
                visitBase(arg);
            return defaultVisitResult;
        }

        Result visit(Ast.VariableExpr node)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.RangeExpr node)
        {
            visitBase(node.lowerExpr);
            visitBase(node.upperExpr);
            return defaultVisitResult;
        }

        Result visit(Ast.AstQuoteExpr node)
        {
            visitBase(node.expr);
            return defaultVisitResult;
        }

        Result visit(Ast.AstQuasiQuoteExpr node)
        {
            visitBase(node.expr);
            return defaultVisitResult;
        }

        Result visit(Ast.AstQQSubExpr node)
        {
            visitBase(node.expr);
            return defaultVisitResult;
        }

        Result visit(Ast.LetExpr node)
        {
            foreach( bindExpr ; node.bindExprs )
                visitBase(bindExpr);
            visitBase(node.subExpr);
            return defaultVisitResult;
        }

        Result visit(Ast.ImportExpr node)
        {
            visitBase(node.scopeExpr);
            visitBase(node.symbolsExpr);
            visitBase(node.subExpr);
            return defaultVisitResult;
        }

        Result visit(Ast.TernaryExpr node)
        {
            visitBase(node.lhs);
            visitBase(node.mid);
            visitBase(node.rhs);
            return defaultVisitResult;
        }
    }
    else
    {
        final Result visitBase(Ast.Node node, Arg arg)
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

        Result defaultVisit(Ast.Node node, Arg arg)
        {
            assert(false, "missing visit for "~node.classinfo.name);
        }

        Result visit(Ast.Module node, Arg arg)
        {
            foreach( stmt ; node.stmts )
                visitBase(stmt, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.ImportStmt node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.LetExprStmt node, Arg arg)
        {
            return visitBase(node.expr, arg);
        }

        Result visit(Ast.LetFuncStmt node, Arg arg)
        {
            return visitBase(node.expr, arg);
        }

        Result visit(Ast.ExprStmt node, Arg arg)
        {
            return visitBase(node.expr, arg);
        }

        Result visit(Ast.RewrittenExpr node, Arg arg)
        {
            // Note: original is not visited
            return visitBase(node.rewrite, arg);
        }

        Result visit(Ast.BinaryExpr node, Arg arg)
        {
            visitBase(node.lhs, arg);
            visitBase(node.rhs, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.InfixFuncExpr node, Arg arg)
        {
            visitBase(node.func, arg);
            visitBase(node.lhs, arg);
            visitBase(node.rhs, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.PrefixExpr node, Arg arg)
        {
            visitBase(node.subExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.PostfixFuncExpr node, Arg arg)
        {
            visitBase(node.func, arg);
            visitBase(node.subExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.NumberExpr node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.StringExpr node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.LogicalExpr node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.NilExpr node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.ListExpr node, Arg arg)
        {
            foreach( elem ; node.elemExprs )
                visitBase(elem, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.MapExpr node, Arg arg)
        {
            foreach( kvp ; node.keyValuePairs )
            {
                visitBase(kvp.key, arg);
                visitBase(kvp.value, arg);
            }
            return defaultVisitResult;
        }

        Result visit(Ast.LambdaExpr node, Arg arg)
        {
            visitBase(node.expr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.ExplodeExpr node, Arg arg)
        {
            visitBase(node.subExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.CallExpr node, Arg arg)
        {
            visitBase(node.funcExpr, arg);
            foreach( arg ; node.argExprs )
                visitBase(arg, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.VariableExpr node, Arg arg)
        {
            return defaultVisitResult;
        }

        Result visit(Ast.RangeExpr node, Arg arg)
        {
            visitBase(node.lowerExpr, arg);
            visitBase(node.upperExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.AstQuoteExpr node, Arg arg)
        {
            visitBase(node.expr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.AstQuasiQuoteExpr node, Arg arg)
        {
            visitBase(node.expr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.AstQQSubExpr node, Arg arg)
        {
            visitBase(node.expr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.LetExpr node, Arg arg)
        {
            foreach( bindExpr ; node.bindExprs )
                visitBase(bindExpr, arg);
            visitBase(node.subExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.ImportExpr node, Arg arg)
        {
            visitBase(node.scopeExpr, arg);
            visitBase(node.symbolsExpr, arg);
            visitBase(node.subExpr, arg);
            return defaultVisitResult;
        }

        Result visit(Ast.TernaryExpr node, Arg arg)
        {
            visitBase(node.lhs, arg);
            visitBase(node.mid, arg);
            visitBase(node.rhs, arg);
            return defaultVisitResult;
        }
    }
}

