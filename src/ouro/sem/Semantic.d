/**
    Semantic analysis.

    This module's purpose is to, given necessary context, convert an AST
    subtree into a tree of semantic nodes.

    Essentially, it is the bridge between the ast and sit.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Semantic;

import tango.io.Stdout;

import ouro.Error;
import ouro.sem.Abort;
import ouro.sem.Context;

import Ast = ouro.ast.Nodes;
import Sit = ouro.sit.Nodes;

import AstVisitor   = ouro.ast.Visitor;
import QQRewrite    = ouro.ast.QQRewriteVisitor;
import Eval         = ouro.sem.Eval;

bool aborted(void delegate() dg)
{
    bool result = false;
    try
    {
        dg();
    }
    catch( NonFatalAbort )
    {
        result = true;
    }
    return result;
}

class SemInitialVisitor : AstVisitor.Visitor!(Sit.Node, Context*)
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

        Sit.Expr visitExpr(Ast.Node node, Context* ctx)
        {
            auto newNode = visitBase(node, ctx);
            assert( newNode !is null );
            auto expr = cast(Sit.Expr) newNode;
            assert( expr !is null );
            return expr;
        }

        Eval.EvalVisitor eval;
        QQRewrite.QQRewriteVisitor qqr;

        Sit.Value evalExpr(Sit.Expr expr)
        {
            alias Eval.Context Context;

            Context ctx;
            ctx.evalCtx = Context.EvalContext.Compile;
            ctx.onUnfixed = &NonFatalAbort.throwForUnfixed;

            return eval.visitValue(expr, ctx);
        }

        Sit.AstQuoteValue qqRewrite(Ast.Expr astExpr,
                out Ast.Expr[] subExprs)
        {
            QQRewrite.Context ctx;
            auto aqv = new Sit.AstQuoteValue(astExpr,
                    qqr.visitBase(astExpr, &ctx));
            subExprs = ctx.subExprs;

            return aqv;
        }
    }

    this()
    {
        eval = new Eval.EvalVisitor;
        qqr = new QQRewrite.QQRewriteVisitor;
    }

    Sit.Node visitMacro(Context* ctx, Ast.Node node,
            Sit.FunctionValue fn, Sit.CallArg[] args...)
    {
        auto call = new Sit.CallExpr(node, fn, args);
        auto value = evalExpr(call);
        auto astValue = cast(Sit.AstQuoteValue) value;
        assert( astValue !is null, "expected ast result from macro; "
                "got a " ~ value.toString );
        return visitExpr(astValue.ast, ctx);
    }

    override Sit.Node visit(Ast.Module node, Context* ctx)
    {
        auto stmts = new Sit.Stmt[node.stmts.length];
        auto scop = new Sit.PartialScope(ctx.scop, false);
        ctx.scop = scop;

        /*
            This is going to be horrible.  *sigh*

            The problem is that we allow definitions to be used in any order.
            We do not know what this order is.  The only way I can think of to
            handle this is to iterate over the statements repeatedly until
            they're all done.

            Of course, it is possible to construct a module with no valid
            ordering.  We'll detect that by making sure that, on every cycle,
            we successfully semantic at least one statement.
         */

        bool failedStmt = false;    // Did at least one stmt fail?
        bool successStmt = false;   // Did at least one stmt succeed?

        do
        {
            Stderr("New pass").newline;
            failedStmt = false;
            successStmt = false;

            foreach( i,ref stmt ; stmts )
            {
                if( stmt.expr !is null )
                    // Already done this one!  YAY!
                    continue;

                // Try to process
                auto subCtx = ctx.dup;
                auto modStmt = node.stmts[i];
                Sit.Expr expr;

                subCtx.stmt = &stmt;
                subCtx.stmt.loc = modStmt.loc;

                Stderr("Processing ")(modStmt)("...");

                if( aborted({ expr = visitExpr(modStmt, &subCtx); }) )
                {
                    // Bastard.
                    Stderr(" failed.").newline;
                    failedStmt = true;
                }
                else
                {
                    // Hooray!
                    Stderr(" success; eval");

                    debug(SemVerbose) if( subCtx.dumpNode !is null )
                    {
                        Stderr(" ");
                        subCtx.dumpNode(expr);
                        Stderr(" ");
                    }

                    /*
                        We've got a semantic tree, but not (necessarily) an
                        actual value.  This is important if we try to use an
                        indirectly defined macro.

                        Try to evaluate it.
                     */
                    Sit.Value value;
                    if( aborted({ value = evalExpr(expr); }) )
                    {
                        Stderr(" failed.").newline;
                        failedStmt = true;
                    }
                    else
                    {
                        Stderr(" worked.");
                        successStmt = true;

                        debug(SemVerbose) if( subCtx.dumpNode !is null )
                        {
                            Stderr(" = ");
                            subCtx.dumpNode(value);
                        }

                        Stderr.newline;

                        if( subCtx.stmt.bind )
                        {
                            subCtx.scop.bind(subCtx.stmt.bindIdent, value);
                        }

                        if( subCtx.stmt.mergeAll )
                        {
                            assert(false, "nyi");
                        }
                        else if( subCtx.stmt.mergeList.length != 0 )
                        {
                            assert(false, "nyi");
                        }

                        subCtx.stmt.expr = value;
                    }
                }
            }

            assert( successStmt, "could not complete semantic analysis" );
        }
        while( failedStmt )

        return new Sit.Module(node, stmts, /*exportSymbols*/null, ctx.scop);
    }

    override Sit.Node visit(Ast.ImportStmt node, Context* ctx)
    {
        assert( ctx.stmt !is null );
        assert( false, "imports not implemented yet" );

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

    override Sit.Node visit(Ast.LetExprStmt node, Context* ctx)
    {
        assert( ctx.stmt !is null );

        ctx.stmt.bind = true;
        ctx.stmt.bindIdent = node.ident;

        return visitBase(node.expr, ctx);
    }

    override Sit.Node visit(Ast.LetFuncStmt node, Context* ctx)
    {
        assert( ctx.stmt !is null );

        auto subCtx = ctx.dup;

        subCtx.stmt.bind = true;
        subCtx.stmt.bindIdent = node.ident;

        subCtx.scop = new Sit.Scope(ctx.scop, true);

        auto args = new Sit.Argument[node.args.length];

        foreach( i,arg ; node.args )
        {
            subCtx.scop.bindArg(null, arg.ident);
            args[i] = Sit.Argument(arg.loc, arg.ident, arg.isVararg);
        }

        auto expr = visitExpr(node.expr, &subCtx);

        return new Sit.FunctionValue(node, node.ident, args, null,
                subCtx.scop, expr);
    }

    override Sit.Node visit(Ast.ExprStmt node, Context* ctx)
    {
        assert( ctx.stmt !is null );
        return visitBase(node.expr, ctx);
    }

    override Sit.Node visit(Ast.RewrittenExpr node, Context* ctx)
    {
        // We don't really care if an expression was rewritten or not.
        return visitBase(node.rewrite, ctx);
    }

    override Sit.Node visit(Ast.BinaryExpr node, Context* ctx)
    {
        auto func = ctx.builtinFunction(node.builtin);
        auto lhs = visitExpr(node.lhs, ctx);
        auto rhs = visitExpr(node.rhs, ctx);
        
        return new Sit.CallExpr(node, func,
                [Sit.CallArg(lhs, false), Sit.CallArg(rhs, false)]);
    }

    override Sit.Node visit(Ast.TernaryExpr node, Context* ctx)
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

    override Sit.Node visit(Ast.InfixFuncExpr node, Context* ctx)
    {
        auto func = visitExpr(node.funcExpr, ctx);
        auto lhs = visitExpr(node.lhs, ctx);
        auto rhs = visitExpr(node.rhs, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(lhs, false),
                 Sit.CallArg(rhs, false)]);
    }

    override Sit.Node visit(Ast.PrefixExpr node, Context* ctx)
    {
        auto func = ctx.builtinFunction(node.builtin);
        auto subExpr = visitExpr(node.subExpr, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(subExpr, false)]);
    }

    override Sit.Node visit(Ast.PostfixFuncExpr node, Context* ctx)
    {
        auto func = visitExpr(node.funcExpr, ctx);
        auto subExpr = visitExpr(node.subExpr, ctx);

        return new Sit.CallExpr(node, func,
                [Sit.CallArg(subExpr, false)]);
    }

    override Sit.Node visit(Ast.NumberExpr node, Context* ctx)
    {
        return new Sit.NumberValue(node, node.value);
    }

    override Sit.Node visit(Ast.StringExpr node, Context* ctx)
    {
        return new Sit.StringValue(node, node.value);
    }

    override Sit.Node visit(Ast.LogicalExpr node, Context* ctx)
    {
        return new Sit.LogicalValue(node, node.value);
    }

    override Sit.Node visit(Ast.NilExpr node, Context* ctx)
    {
        return new Sit.NilValue(node);
    }

    override Sit.Node visit(Ast.ListExpr node, Context* ctx)
    {
        auto elemExprs = new Sit.Expr[node.elemExprs.length];

        foreach( i,elemExpr ; node.elemExprs )
            elemExprs[i] = visitExpr(elemExpr, ctx);

        return new Sit.ListExpr(node, elemExprs);
    }

    override Sit.Node visit(Ast.MapExpr node, Context* ctx)
    {
        auto kvps = new Sit.ExprKVP[node.keyValuePairs.length];

        foreach( i,kvp ; node.keyValuePairs )
            kvps[i] = Sit.ExprKVP(kvp.loc,
                visitExpr(kvp.key, ctx),
                visitExpr(kvp.value, ctx));

        return new Sit.MapExpr(node, kvps);
    }

    override Sit.Node visit(Ast.LambdaExpr node, Context* ctx)
    {
        auto subCtx = ctx.dup;
        subCtx.clearEnclosedValues;
        scope(exit) ctx.mergeEnclosedValues(subCtx);

        subCtx.scop = new Sit.Scope(ctx.scop, true);
        auto args = new Sit.Argument[node.args.length];

        foreach( i,arg ; node.args )
        {
            subCtx.scop.bindArg(null, arg.ident);
            args[i] = Sit.Argument(arg.loc, arg.ident, arg.isVararg);
        }

        auto expr = visitExpr(node.expr, &subCtx);

        return new Sit.FunctionValue(node, "λ", args,
                subCtx.enclosedValues, subCtx.scop, expr);
    }

    override Sit.Node visit(Ast.ExplodeExpr node, Context* ctx)
    {
        err(CEC.SUnexExplode, node);
        assert(false);
    }

    override Sit.Node visit(Ast.CallExpr node, Context* ctx)
    {
        auto funcExpr = visitExpr(node.funcExpr, ctx);
        if( node.isMacro )
        {
            auto argExprs = new Sit.CallArg[node.argExprs.length];
            foreach( i,argExpr ; node.argExprs )
            {
                argExprs[i] = Sit.CallArg(
                        new Sit.AstQuoteValue(argExpr, argExpr),
                        false);
            }

            auto funcValue = cast(Sit.FunctionValue) evalExpr(funcExpr);

            return visitMacro(ctx, node, funcValue, argExprs);
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

    override Sit.Node visit(Ast.VariableExpr node, Context* ctx)
    {
        auto v = ctx.scop.lookup(node, node.ident);
        if( auto ev = cast(Sit.EnclosedValue) v )
            ctx.addEnclosedValue(ev);

        return v;
    }

    override Sit.Node visit(Ast.RangeExpr node, Context* ctx)
    {
        return new Sit.CallExpr(node,
            ctx.builtinFunction("ouro.range"),
            [Sit.CallArg(new Sit.LogicalValue(node, node.incLower), false),
             Sit.CallArg(new Sit.LogicalValue(node, node.incUpper), false),
             Sit.CallArg(visitExpr(node.lowerExpr, ctx), false),
             Sit.CallArg(visitExpr(node.upperExpr, ctx), false)]
        );
    }

    override Sit.Node visit(Ast.AstQuoteExpr node, Context* ctx)
    {
        return new Sit.AstQuoteValue(node, node.expr);
    }

    override Sit.Node visit(Ast.AstQuasiQuoteExpr node, Context* ctx)
    {
        Ast.Expr[] subExprs;
        auto qq = qqRewrite(node.expr, subExprs);

        auto args = new Sit.CallArg[1 + subExprs.length];
        
        args[0] = Sit.CallArg(qq, false);

        foreach( i,subExpr ; subExprs )
            args[1+i] = Sit.CallArg(visitExpr(subExpr, ctx), false);

        return new Sit.CallExpr(node,
                ctx.builtinFunction("ouro.qqsub"), args);
    }

    override Sit.Node visit(Ast.AstQQSubExpr node, Context* ctx)
    {
        auto astExpr = node.expr;
        auto sitExpr = visitExpr(astExpr, ctx);
        auto value = evalExpr(sitExpr);
        auto astValue = cast(Sit.AstQuoteValue) value;
        assert( astValue !is null, "expected ast result from macro; "
                "got a " ~ value.toString );
        return visitExpr(astValue.ast, ctx);
    }

    override Sit.Node visit(Ast.LetExpr node, Context* ctx)
    {
        auto bindExprs = new Sit.Expr[node.bindExprs.length];

        foreach( i,bindExpr ; node.bindExprs )
            bindExprs[i] = new Sit.AstQuoteValue(bindExpr, bindExpr);

        auto bindListExpr = new Sit.ListExpr(null, bindExprs);

        auto subExpr = new Sit.AstQuoteValue(node.subExpr, node.subExpr);

        return visitMacro(ctx, node,
            ctx.builtinFunction("ouro.let"),
            Sit.CallArg(bindListExpr, false),
            Sit.CallArg(subExpr, false)
        );
    }

    override Sit.Node visit(Ast.ImportExpr node, Context* ctx)
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

    override Sit.Node visit(Ast.BuiltinExpr node, Context* ctx)
    {
        return ctx.builtin(node.ident);
    }
}

