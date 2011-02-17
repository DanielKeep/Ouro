/**
    Semantic analysis.

    This module's purpose is to, given necessary context, convert an AST
    subtree into a tree of semantic nodes.

    Essentially, it is the bridge between the ast and sit.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Semantic;

import ouro.Error;
import ouro.sem.Context;

import Ast = ouro.ast.Nodes;
import Sit = ouro.sit.Nodes;

import AstVisitor = ouro.ast.Visitor;

class SemInitialVisitor : AstVisitor.Visitor!(Sit.Node, Context)
{
    protected
    {
        void err(CEC code, Ast.Node node, char[] arg0 = null, char[] arg1 = null)
        {
            err(code, node.loc, arg0, arg1);
        }

        void err(CEC code, Location loc, char[] arg0 = null, char[] arg1 = null)
        {
            throw new CompilerException(code, loc, arg0, arg1);
        }

        Sit.Expr visitExpr(Ast.Node node, Context ctx)
        {
            auto newNode = visitBase(node, ctx);
            assert( newNode !is null );
            auto expr = cast(Sit.Expr) newNode;
            assert( expr !is null );
            return expr;
        }
    }

    override Sit.Node visit(Ast.Module node, Context ctx)
    {
        auto stmts = new Sit.Stmt[node.stmts.length];
        auto scop = new Sit.PartialScope(ctx.scop);
        ctx.scop = scop;

        foreach( i,modStmt ; node.stmts )
        {
            ctx.stmt = &stmts[i];
            ctx.stmt.loc = modStmt.loc;
            ctx.stmt.expr = visitExpr(modStmt, ctx);
        }

        return new Sit.Module(node, stmts, /*exportSymbols*/null, ctx.scop);
    }

    override Sit.Node visit(Ast.ImportStmt node, Context ctx)
    {
        assert( ctx.stmt !is null );

        auto moduleExpr = new Sit.CallExpr(node,
            ctx.builtinFunction("ouro.module"),
            [Sit.CallArg(new Sit.StringValue(node, node.modulePath), false)]
        );

        if( node.ident != "" )
        {
            if( node.symbols.length != 0 )
                err(CEC.SLetSelImp, node);

            ctx.stmt.bind = true;
            ctx.stmt.bindIdent = node.ident;
        }
        else
        {
            ctx.stmt.mergeAll = node.all;
            ctx.stmt.mergeList = node.symbols;
        }

        return moduleExpr;
    }

    override Sit.Node visit(Ast.LetExprStmt node, Context ctx)
    {
        assert( ctx.stmt !is null );

        ctx.stmt.bind = true;
        ctx.stmt.bindIdent = node.ident;

        return visitBase(node.expr, ctx);
    }

    override Sit.Node visit(Ast.LetFuncStmt node, Context ctx)
    {
        assert( ctx.stmt !is null );

        ctx.stmt.bind = true;
        ctx.stmt.bindIdent = node.ident;

        ctx.scop = new Sit.Scope(ctx.scop);

        auto args = new Sit.Argument[node.args.length];

        foreach( i,arg ; node.args )
            args[i] = Sit.Argument(arg.loc, arg.ident, arg.isVararg);

        return new Sit.FunctionValue(node, node.ident, args, ctx.scop,
                visitExpr(node.expr, ctx));
    }

    override Sit.Node visit(Ast.ExprStmt node, Context ctx)
    {
        assert( ctx.stmt !is null );
        return visitBase(node.expr, ctx);
    }

    override Sit.Node visit(Ast.RewrittenExpr node, Context ctx)
    {
        // We don't really care if an expression was rewritten or not.
        return visitBase(node.rewrite, ctx);
    }

    override Sit.Node visit(Ast.BinaryExpr node, Context ctx)
    {
        auto func = ctx.builtinFunction(node.builtin);
        auto lhs = visitExpr(node.lhs, ctx);
        auto rhs = visitExpr(node.rhs, ctx);
        
        return new Sit.CallExpr(node, func,
                [Sit.CallArg(lhs, false), Sit.CallArg(rhs, false)]);
    }

    override Sit.Node visit(Ast.TernaryExpr node, Context ctx)
    {
        auto func = ctx.builtinFunction(node.builtin);
        auto lhs = visitExpr(node.lhs, ctx);
        auto mid = visitExpr(node.mid, ctx);
        auto rhs = visitExpr(node.rhs, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(lhs, false),
                 Sit.CallArg(mid, false),
                 Sit.CallArg(rhs, false)]);
    }

    override Sit.Node visit(Ast.InfixFuncExpr node, Context ctx)
    {
        auto func = visitExpr(node.funcExpr, ctx);
        auto lhs = visitExpr(node.lhs, ctx);
        auto rhs = visitExpr(node.rhs, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(lhs, false),
                 Sit.CallArg(rhs, false)]);
    }

    override Sit.Node visit(Ast.PrefixExpr node, Context ctx)
    {
        auto func = ctx.builtinFunction(node.builtin);
        auto subExpr = visitExpr(node.subExpr, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(subExpr, false)]);
    }

    override Sit.Node visit(Ast.PostfixFuncExpr node, Context ctx)
    {
        auto func = visitExpr(node.funcExpr, ctx);
        auto subExpr = visitExpr(node.subExpr, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(subExpr, false)]);
    }

    override Sit.Node visit(Ast.NumberExpr node, Context ctx)
    {
        return new Sit.NumberValue(node, node.value);
    }

    override Sit.Node visit(Ast.StringExpr node, Context ctx)
    {
        return new Sit.StringValue(node, node.value);
    }

    override Sit.Node visit(Ast.LogicalExpr node, Context ctx)
    {
        return new Sit.LogicalValue(node, node.value);
    }

    override Sit.Node visit(Ast.NilExpr node, Context ctx)
    {
        return new Sit.NilValue(node);
    }

    override Sit.Node visit(Ast.ListExpr node, Context ctx)
    {
        auto elemExprs = new Sit.Expr[node.elemExprs.length];

        foreach( i,elemExpr ; node.elemExprs )
            elemExprs[i] = visitExpr(elemExpr, ctx);

        return new Sit.ListValue(node, elemExprs);
    }

    override Sit.Node visit(Ast.MapExpr node, Context ctx)
    {
        auto kvps = new Sit.KeyValuePair[node.keyValuePairs.length];

        foreach( i,kvp ; node.keyValuePairs )
            kvps[i] = Sit.KeyValuePair(kvp.loc,
                visitExpr(kvp.key, ctx),
                visitExpr(kvp.value, ctx));

        return new Sit.MapValue(node, kvps);
    }

    override Sit.Node visit(Ast.LambdaExpr node, Context ctx)
    {
        auto args = new Sit.Argument[node.args.length];

        foreach( i,arg ; node.args )
            args[i] = Sit.Argument(arg.loc, arg.ident, arg.isVararg);

        return new Sit.FunctionValue(node, "anon", args, ctx.scop,
                visitExpr(node.expr, ctx));
    }

    override Sit.Node visit(Ast.ExplodeExpr node, Context ctx)
    {
        err(CEC.SUnexExplode, node);
        assert(false);
    }

    override Sit.Node visit(Ast.CallExpr node, Context ctx)
    {
        auto funcExpr = visitExpr(node.funcExpr, ctx);
        if( node.isMacro )
        {
            auto argExprs = new Sit.CallArg[node.argExprs.length];
            foreach( i,argExpr ; node.argExprs )
            {
                bool explode = false;
                if( auto explodeExpr = cast(Ast.ExplodeExpr) argExpr )
                {
                    explode = true;
                    argExpr = explodeExpr.subExpr;
                }
                argExprs[i] = Sit.CallArg(
                    new Sit.AstQuoteValue(argExpr, argExpr),
                    explode);
            }

            return new Sit.AstMixinExpr(node,
                new Sit.CallExpr(node, funcExpr, argExprs)
            );
        }
        else
        {
            auto argExprs = new Sit.CallArg[node.argExprs.length];
            foreach( i,argExpr ; node.argExprs )
            {
                bool explode = false;
                if( auto explodeExpr = cast(Ast.ExplodeExpr) argExpr )
                {
                    explode = true;
                    argExpr = explodeExpr.subExpr;
                }
                argExprs[i] = Sit.CallArg(visitExpr(argExpr, ctx), explode);
            }

            return new Sit.CallExpr(node, funcExpr, argExprs);
        }
    }

    override Sit.Node visit(Ast.VariableExpr node, Context ctx)
    {
        return ctx.scop.lookup(node, node.ident);
    }

    override Sit.Node visit(Ast.RangeExpr node, Context ctx)
    {
        return new Sit.CallExpr(node,
            ctx.builtinFunction("ouro.range"),
            [Sit.CallArg(new Sit.LogicalValue(node, node.incLower), false),
             Sit.CallArg(new Sit.LogicalValue(node, node.incUpper), false),
             Sit.CallArg(visitExpr(node.lowerExpr, ctx), false),
             Sit.CallArg(visitExpr(node.upperExpr, ctx), false)]
        );
    }

    override Sit.Node visit(Ast.AstQuoteExpr node, Context ctx)
    {
        return new Sit.AstQuoteValue(node, node.expr);
    }

    override Sit.Node visit(Ast.AstQuasiQuoteExpr node, Context ctx)
    {
        return new Sit.CallExpr(node,
            ctx.builtinFunction("ouro.quasiquote"),
            [Sit.CallArg(new Sit.AstQuoteValue(node, node.expr), false)]
        );
    }

    override Sit.Node visit(Ast.AstQQSubExpr node, Context ctx)
    {
        return new Sit.AstMixinExpr(node, visitExpr(node.expr, ctx));
    }

    override Sit.Node visit(Ast.LetExpr node, Context ctx)
    {
        auto bindExprs = new Sit.Expr[node.bindExprs.length];

        foreach( i,bindExpr ; node.bindExprs )
            bindExprs[i] = new Sit.AstQuoteValue(bindExpr, bindExpr);

        auto bindListExpr = new Sit.ListValue(null, bindExprs);

        auto subExpr = new Sit.AstQuoteValue(node.subExpr, node.subExpr);

        return new Sit.AstMixinExpr(node,
            new Sit.CallExpr(node,
                ctx.builtinFunction("ouro.let"),
                [Sit.CallArg(bindListExpr, false),
                 Sit.CallArg(subExpr, false)]
            )
        );
    }

    override Sit.Node visit(Ast.ImportExpr node, Context ctx)
    {
        auto scopeExpr = new Sit.AstQuoteValue(node.scopeExpr, node.scopeExpr);
        auto symbolsExpr = new Sit.AstQuoteValue(node.symbolsExpr, node.symbolsExpr);
        auto subExpr = new Sit.AstQuoteValue(node.subExpr, node.subExpr);

        return new Sit.CallExpr(node,
            ctx.builtinFunction("ouro.import"),
            [Sit.CallArg(scopeExpr, false),
             Sit.CallArg(symbolsExpr, false),
             Sit.CallArg(subExpr, false)]
        );
    }

    override Sit.Node visit(Ast.BuiltinExpr node, Context ctx)
    {
        return ctx.builtin(node.ident);
    }
}

