/**
    Substitutes ASTs into a index-rewritten QQ'ed AST.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.ast.QQSubVisitor;

import ouro.ast.Visitor;

import Ast = ouro.ast.Nodes;

class QQSubVisitor : Visitor!(Ast.Expr, Ast.Expr[])
{
    Ast.Expr visitExpr(Ast.Node node, Ast.Expr[] subExprs)
    {
        auto r = visitBase(node, subExprs);
        assert( r !is null );
        auto e = cast(Ast.Expr) r;
        assert( e !is null );
        return e;
    }

    override Ast.Expr visit(Ast.ImportStmt node, Ast.Expr[] subExprs)
    {
        assert(false);
    }

    override Ast.Expr visit(Ast.LetExprStmt node, Ast.Expr[] subExprs)
    {
        assert(false);
    }

    override Ast.Expr visit(Ast.LetFuncStmt node, Ast.Expr[] subExprs)
    {
        assert(false);
    }

    override Ast.Expr visit(Ast.BinaryExpr node, Ast.Expr[] subExprs)
    {
        auto lhs = visitExpr(node.lhs, subExprs);
        auto rhs = visitExpr(node.rhs, subExprs);

        if( lhs is node.lhs && rhs is node.rhs )
            return node;

        return new Ast.BinaryExpr(node.loc, node.op, lhs, rhs);
    }

    override Ast.Expr visit(Ast.TernaryExpr node, Ast.Expr[] subExprs)
    {
        auto lhs = visitExpr(node.lhs, subExprs);
        auto mid = visitExpr(node.mid, subExprs);
        auto rhs = visitExpr(node.rhs, subExprs);

        if( lhs is node.lhs && rhs is node.rhs && mid is node.mid )
            return node;

        return new Ast.TernaryExpr(node.loc, node.op, lhs, mid, rhs);
    }

    override Ast.Expr visit(Ast.InfixFuncExpr node, Ast.Expr[] subExprs)
    {
        auto funcExpr = visitExpr(node.funcExpr, subExprs);
        auto lhs = visitExpr(node.lhs, subExprs);
        auto rhs = visitExpr(node.rhs, subExprs);

        if( funcExpr is node.funcExpr && lhs is node.lhs && rhs is node.rhs )
            return node;

        return new Ast.InfixFuncExpr(node.loc, funcExpr, lhs, rhs);
    }

    override Ast.Expr visit(Ast.PrefixExpr node, Ast.Expr[] subExprs)
    {
        auto subExpr = visitExpr(node.subExpr, subExprs);

        if( subExpr is node.subExpr )
            return node;

        return new Ast.PrefixExpr(node.loc, node.op, subExpr);
    }

    override Ast.Expr visit(Ast.PostfixFuncExpr node, Ast.Expr[] subExprs)
    {
        auto funcExpr = visitExpr(node.funcExpr, subExprs);
        auto subExpr = visitExpr(node.subExpr, subExprs);

        if( funcExpr is node.funcExpr && subExpr is node.subExpr )
            return node;

        return new Ast.PostfixFuncExpr(node.loc, funcExpr, subExpr);
    }

    override Ast.Expr visit(Ast.NumberExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.StringExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.LogicalExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.NilExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.ListExpr node, Ast.Expr[] subExprs)
    {
        bool same = true;
        auto elemExprs = new Ast.Expr[node.elemExprs.length];

        foreach( i,elemExpr ; node.elemExprs )
        {
            elemExprs[i] = visitExpr(elemExpr, subExprs);
            same = same && (elemExprs[i] is elemExpr);
        }

        if( same ) return node;

        return new Ast.ListExpr(node.loc, elemExprs);
    }

    override Ast.Expr visit(Ast.MapExpr node, Ast.Expr[] subExprs)
    {
        bool same = true;
        auto kvps = new Ast.KeyValuePair[node.keyValuePairs.length];

        foreach( i,kvp ; node.keyValuePairs )
        {
            auto k = visitExpr(kvp.key, subExprs);
            auto v = visitExpr(kvp.value, subExprs);
            same = same && (k is kvp.key && v is kvp.value);

            kvps[i] = Ast.KeyValuePair(kvp.loc, k, v);
        }

        if( same ) return node;

        return new Ast.MapExpr(node.loc, kvps);
    }

    override Ast.Expr visit(Ast.LambdaExpr node, Ast.Expr[] subExprs)
    {
        auto expr = visitExpr(node.expr, subExprs);

        auto args = node.args.dup;
        foreach( i, ref arg ; args )
            if( arg.defaultExpr !is null )
                arg.defaultExpr = visitExpr(arg.defaultExpr, subExprs);

        if( expr is node.expr && args == node.args )
            return node;

        return new Ast.LambdaExpr(node.loc, node.isMacro, args, expr);
    }

    override Ast.Expr visit(Ast.ExplodeExpr node, Ast.Expr[] subExprs)
    {
        auto subExpr = visitExpr(node.subExpr, subExprs);

        if( subExpr is node.subExpr )
            return node;

        return new Ast.ExplodeExpr(node.loc, subExpr);
    }

    override Ast.Expr visit(Ast.CallExpr node, Ast.Expr[] subExprs)
    {
        bool same = true;
        auto funcExpr = visitExpr(node.funcExpr, subExprs);
        auto argExprs = new Ast.Expr[node.argExprs.length];
        Ast.Expr[char[]] namedExprs;

        foreach( i,argExpr ; node.argExprs )
        {
            argExprs[i] = visitExpr(argExpr, subExprs);
            same = same && (argExprs[i] is argExpr);
        }

        foreach( ident,argExpr ; node.namedArgExprs )
        {
            namedExprs[ident] = visitExpr(argExpr, subExprs);
            same = same && (namedExprs[ident] is argExpr);
        }

        if( funcExpr is node.funcExpr && same )
            return node;

        return new Ast.CallExpr(node.loc, node.isMacro,
                funcExpr, argExprs, namedExprs);
    }

    override Ast.Expr visit(Ast.VariableExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.RangeExpr node, Ast.Expr[] subExprs)
    {
        auto lowerExpr = visitExpr(node.lowerExpr, subExprs);
        auto upperExpr = visitExpr(node.upperExpr, subExprs);

        if( lowerExpr is node.lowerExpr && upperExpr is node.upperExpr )
            return node;

        return new Ast.RangeExpr(node.loc, node.incLower, node.incUpper,
                lowerExpr, upperExpr);
    }

    override Ast.Expr visit(Ast.AstQuoteExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.AstQuasiQuoteExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.Expr visit(Ast.AstQQSubExpr node, Ast.Expr[] subExprs)
    {
        assert( node.expr is null );
        assert( node.index != ~0 );

        return subExprs[node.index];
    }

    override Ast.Expr visit(Ast.BuiltinExpr node, Ast.Expr[] subExprs)
    {
        return node;
    }

    override Ast.LetExpr visit(Ast.LetExpr node, Ast.Expr[] subExprs)
    {
        bool same = true;
        auto bindExprs = new Ast.Expr[node.bindExprs.length];

        foreach( i,bindExpr ; node.bindExprs )
        {
            bindExprs[i] = visitExpr(bindExpr, subExprs);
            same = same && (bindExprs[i] is bindExpr);
        }

        auto subExpr = visitExpr(node.subExpr, subExprs);

        if( same && subExpr is node.subExpr )
            return node;

        return new Ast.LetExpr(node.loc, bindExprs, subExpr);
    }

    override Ast.ImportExpr visit(Ast.ImportExpr node, Ast.Expr[] subExprs)
    {
        auto scopeExpr = visitExpr(node.scopeExpr, subExprs);
        auto symbolsExpr = visitExpr(node.symbolsExpr, subExprs);
        auto subExpr = visitExpr(node.subExpr, subExprs);

        if( scopeExpr is node.scopeExpr
                && symbolsExpr is node.symbolsExpr
                && subExpr is node.subExpr )
            return node;

        return new Ast.ImportExpr(node.loc, scopeExpr, symbolsExpr, subExpr);
    }
}

