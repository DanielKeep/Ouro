/**
    Rewrites a quasi-quoted AST, replacing ast substitutions with index
    substitutions and builds an ordered list of said substitutions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.ast.QQRewriteVisitor;

import ouro.ast.Visitor;

import Ast = ouro.ast.Nodes;

struct Context
{
    Ast.Expr[] subExprs;

    size_t addSubExpr(Ast.Expr expr)
    {
        subExprs ~= expr;
        return subExprs.length - 1;
    }
}

class QQRewriteVisitor : Visitor!(Ast.Expr, Context*)
{
    Ast.Expr visitExpr(Ast.Node node, Context* ctx)
    {
        auto r = visitBase(node, ctx);
        assert( r !is null );
        auto e = cast(Ast.Expr) r;
        assert( e !is null );
        return e;
    }

    override Ast.Expr visit(Ast.ImportStmt node, Context* ctx)
    {
        assert(false);
    }

    override Ast.Expr visit(Ast.LetExprStmt node, Context* ctx)
    {
        assert(false);
    }

    override Ast.Expr visit(Ast.LetFuncStmt node, Context* ctx)
    {
        assert(false);
    }

    override Ast.Expr visit(Ast.BinaryExpr node, Context* ctx)
    {
        auto lhs = visitExpr(node.lhs, ctx);
        auto rhs = visitExpr(node.rhs, ctx);

        if( lhs is node.lhs && rhs is node.rhs )
            return node;

        return new Ast.BinaryExpr(node.loc, node.op, lhs, rhs);
    }

    override Ast.Expr visit(Ast.TernaryExpr node, Context* ctx)
    {
        auto lhs = visitExpr(node.lhs, ctx);
        auto mid = visitExpr(node.mid, ctx);
        auto rhs = visitExpr(node.rhs, ctx);

        if( lhs is node.lhs && rhs is node.rhs && mid is node.mid )
            return node;

        return new Ast.TernaryExpr(node.loc, node.op, lhs, mid, rhs);
    }

    override Ast.Expr visit(Ast.InfixFuncExpr node, Context* ctx)
    {
        auto funcExpr = visitExpr(node.funcExpr, ctx);
        auto lhs = visitExpr(node.lhs, ctx);
        auto rhs = visitExpr(node.rhs, ctx);

        if( funcExpr is node.funcExpr && lhs is node.lhs && rhs is node.rhs )
            return node;

        return new Ast.InfixFuncExpr(node.loc, funcExpr, lhs, rhs);
    }

    override Ast.Expr visit(Ast.PrefixExpr node, Context* ctx)
    {
        auto subExpr = visitExpr(node.subExpr, ctx);

        if( subExpr is node.subExpr )
            return node;

        return new Ast.PrefixExpr(node.loc, node.op, subExpr);
    }

    override Ast.Expr visit(Ast.PostfixFuncExpr node, Context* ctx)
    {
        auto funcExpr = visitExpr(node.funcExpr, ctx);
        auto subExpr = visitExpr(node.subExpr, ctx);

        if( funcExpr is node.funcExpr && subExpr is node.subExpr )
            return node;

        return new Ast.PostfixFuncExpr(node.loc, funcExpr, subExpr);
    }

    override Ast.Expr visit(Ast.NumberExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.StringExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.LogicalExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.NilExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.ListExpr node, Context* ctx)
    {
        bool same = true;
        auto elemExprs = new Ast.Expr[node.elemExprs.length];

        foreach( i,elemExpr ; node.elemExprs )
        {
            elemExprs[i] = visitExpr(elemExpr, ctx);
            same = same && (elemExprs[i] is elemExpr);
        }

        if( same ) return node;

        return new Ast.ListExpr(node.loc, elemExprs);
    }

    override Ast.Expr visit(Ast.MapExpr node, Context* ctx)
    {
        bool same = true;
        auto kvps = new Ast.KeyValuePair[node.keyValuePairs.length];

        foreach( i,kvp ; node.keyValuePairs )
        {
            auto k = visitExpr(kvp.key, ctx);
            auto v = visitExpr(kvp.value, ctx);
            same = same && (k is kvp.key && v is kvp.value);

            kvps[i] = Ast.KeyValuePair(kvp.loc, k, v);
        }

        if( same ) return node;

        return new Ast.MapExpr(node.loc, kvps);
    }

    override Ast.Expr visit(Ast.LambdaExpr node, Context* ctx)
    {
        auto expr = visitExpr(node.expr, ctx);

        if( expr is node.expr )
            return node;

        return new Ast.LambdaExpr(node.loc, node.isMacro, node.args, expr);
    }

    override Ast.Expr visit(Ast.ExplodeExpr node, Context* ctx)
    {
        auto subExpr = visitExpr(node.subExpr, ctx);

        if( subExpr is node.subExpr )
            return node;

        return new Ast.ExplodeExpr(node.loc, subExpr);
    }

    override Ast.Expr visit(Ast.CallExpr node, Context* ctx)
    {
        bool same = true;
        auto funcExpr = visitExpr(node.funcExpr, ctx);
        auto argExprs = new Ast.Expr[node.argExprs.length];

        foreach( i,argExpr ; node.argExprs )
        {
            argExprs[i] = visitExpr(argExpr, ctx);
            same = same && (argExprs[i] is argExpr);
        }

        if( funcExpr is node.funcExpr && same )
            return node;

        return new Ast.CallExpr(node.loc, node.isMacro, funcExpr, argExprs);
    }

    override Ast.Expr visit(Ast.VariableExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.RangeExpr node, Context* ctx)
    {
        auto lowerExpr = visitExpr(node.lowerExpr, ctx);
        auto upperExpr = visitExpr(node.upperExpr, ctx);

        if( lowerExpr is node.lowerExpr && upperExpr is node.upperExpr )
            return node;

        return new Ast.RangeExpr(node.loc, node.incLower, node.incUpper,
                lowerExpr, upperExpr);
    }

    override Ast.Expr visit(Ast.AstQuoteExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.AstQuasiQuoteExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.Expr visit(Ast.AstQQSubExpr node, Context* ctx)
    {
        return new Ast.AstQQSubExpr(node.loc, ctx.addSubExpr(node.expr));
    }

    override Ast.Expr visit(Ast.BuiltinExpr node, Context* ctx)
    {
        return node;
    }

    override Ast.LetExpr visit(Ast.LetExpr node, Context* ctx)
    {
        bool same = true;
        auto bindExprs = new Ast.Expr[node.bindExprs.length];

        foreach( i,bindExpr ; node.bindExprs )
        {
            bindExprs[i] = visitExpr(bindExpr, ctx);
            same = same && (bindExprs[i] is bindExpr);
        }

        auto subExpr = visitExpr(node.subExpr, ctx);

        if( same && subExpr is node.subExpr )
            return node;

        return new Ast.LetExpr(node.loc, bindExprs, subExpr);
    }

    override Ast.ImportExpr visit(Ast.ImportExpr node, Context* ctx)
    {
        auto scopeExpr = visitExpr(node.scopeExpr, ctx);
        auto symbolsExpr = visitExpr(node.symbolsExpr, ctx);
        auto subExpr = visitExpr(node.subExpr, ctx);

        if( scopeExpr is node.scopeExpr
                && symbolsExpr is node.symbolsExpr
                && subExpr is node.subExpr )
            return node;

        return new Ast.ImportExpr(node.loc, scopeExpr, symbolsExpr, subExpr);
    }
}

