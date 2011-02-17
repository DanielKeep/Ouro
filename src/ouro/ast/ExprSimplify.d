/**
    AST Simplification visitor.

    This module's purpose is to simplify expressions.
 */
module ouro.ast.ExprSimplify;

import ouro.Error;
import ouro.ast.Visitor;

import Ast = ouro.ast.Nodes;

class SimplifyExpr : Visitor!(Ast.Node)
{
    this() {}

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

        Ast.Expr visitExpr(Ast.Node node)
        {
            auto node2 = visitBase(node);
            assert( node2 !is null, "got null result from simplifying "
                    ~ node.toString );
            auto expr = cast(Ast.Expr) node2;
            assert( expr !is null, "expected expression, got "
                    ~ node2.toString );
            return expr;
        }
    }

    Ast.Node visit(Ast.Module node)
    {
        assert(false, "cannot simplify module nodes");
    }

    Ast.Node visit(Ast.ImportStmt node)
    {
        assert(false, "cannot simplify import nodes");
    }

    Ast.Node visit(Ast.LetExprStmt node)
    {
        assert(false, "cannot simplify let expr nodes");
    }

    Ast.Node visit(Ast.LetFuncStmt node)
    {
        assert(false, "cannot simplify let func nodes");
    }

    Ast.Node visit(Ast.ExprStmt node)
    {
        assert(false, "cannot simplify expr stmt nodes");
    }

    Ast.Node visit(Ast.RewrittenExpr node)
    {
        auto rewrite = visitExpr(node.rewrite);
        
        if( rewrite is node.rewrite )
            return node;

        return new Ast.RewrittenExpr(node.loc,
            node.original,
            rewrite);
    }

    Ast.Node visit(Ast.BinaryExpr node)
    {
        /*
            lhs (Op) rhs
                --> __builtin__("ouro.op(Op)")(lhs, rhs)
        */

        auto lhs = visitExpr(node.lhs);
        auto rhs = visitExpr(node.rhs);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                new Ast.BuiltinExpr(node.loc, "ouro.op"~node.opToString(node.op)),
                [lhs, rhs]
            )
        );
    }

    Ast.Node visit(Ast.InfixFuncExpr node)
    {
        /*
            lhs (.func.) rhs
                --> func(lhs, rhs)
        */

        auto func = visitExpr(node.func);
        auto lhs = visitExpr(node.lhs);
        auto rhs = visitExpr(node.rhs);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                func, [lhs, rhs]
            )
        );
    }

    Ast.Node visit(Ast.PrefixExpr node)
    {
        /*
            (Op) subExpr
                --> __builtin__("ouro.op(Op)")(subExpr)
        */

        auto subExpr = visitExpr(node.subExpr);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                new Ast.BuiltinExpr(node.loc, "ouro.op"~node.opToString(node.op)),
                [subExpr]
            )
        );
    }

    Ast.Node visit(Ast.PostfixFuncExpr node)
    {
        /*
            subExpr (.func)
                --> func(subExpr)
        */

        auto func = visitExpr(node.func);
        auto subExpr = visitExpr(node.subExpr);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                func, [subExpr]
            )
        );
    }

    Ast.Node visit(Ast.NumberExpr node)
    {
        return node;
    }

    Ast.Node visit(Ast.StringExpr node)
    {
        return node;
    }

    Ast.Node visit(Ast.LogicalExpr node)
    {
        return node;
    }

    Ast.Node visit(Ast.NilExpr node)
    {
        return node;
    }

    Ast.Node visit(Ast.ListExpr node)
    {
        auto elemExprs = node.elemExprs.dup;
        bool unchanged = true;

        foreach( i,ref elem ; elemExprs )
        {
            elem = visitExpr(elem);
            unchanged &= (elem is node.elemExprs[i]);
        }

        if( unchanged )
            return node;

        return new Ast.ListExpr(node.loc, elemExprs);
    }

    Ast.Node visit(Ast.MapExpr node)
    {
        auto keyValuePairs = node.keyValuePairs.dup;
        bool unchanged = true;

        foreach( i,ref kvp ; keyValuePairs )
        {
            kvp.key = visitExpr(kvp.key);
            kvp.value = visitExpr(kvp.value);
            unchanged &= (kvp.key is node.keyValuePairs[i].key
                    && kvp.value is node.keyValuePairs[i].value);
        }

        if( unchanged )
            return node;

        return new Ast.MapExpr(node.loc, keyValuePairs);
    }

    Ast.Node visit(Ast.LambdaExpr node)
    {
        auto expr = visitExpr(node.expr);

        if( expr is node.expr )
            return node;

        return new Ast.LambdaExpr(node.loc, node.isMacro, node.args, expr);
    }

    Ast.Node visit(Ast.ExplodeExpr node)
    {
        auto subExpr = visitExpr(node.subExpr);

        if( subExpr is node.subExpr )
            return node;

        return new Ast.ExplodeExpr(node.loc, subExpr);
    }

    Ast.Node visit(Ast.CallExpr node)
    {
        auto funcExpr = visitExpr(node.funcExpr);
        if( node.isMacro )
        {
            if( funcExpr is node.funcExpr )
                return node;

            return new Ast.RewrittenExpr(node.loc, node,
                new Ast.CallExpr(node.loc, node.isMacro,
                    funcExpr,
                    node.argExprs
                )
            );
        }
        else
        {
            auto argExprs = node.argExprs.dup;
            bool unchanged = true;

            foreach( i,ref arg ; argExprs )
            {
                arg = visitExpr(arg);
                unchanged &= (arg is node.argExprs[i]);
            }

            if( funcExpr is node.funcExpr && unchanged )
                return node;

            return new Ast.RewrittenExpr(node.loc, node,
                new Ast.CallExpr(node.loc, node.isMacro,
                    funcExpr, argExprs)
            );
        }
    }

    Ast.Node visit(Ast.VariableExpr node)
    {
        return node;
    }

    Ast.Node visit(Ast.RangeExpr node)
    {
        /*
            range (Li) lower, upper (Ui)
                --> __builtin__("ouro.opRange")(Li, Ui, lower, upper)
        */

        auto lowerExpr = visitExpr(node.lowerExpr);
        auto upperExpr = visitExpr(node.upperExpr);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                new Ast.BuiltinExpr(node.loc, "ouro.opRange"),
                [cast(Ast.Expr) new Ast.LogicalExpr(node.loc, node.incLower),
                 new Ast.LogicalExpr(node.loc, node.incUpper),
                 lowerExpr,
                 upperExpr]
            )
        );
    }

    /*
        Quotes are never rewritten.
     */

    Ast.Node visit(Ast.AstQuoteExpr node)
    {
        return node;
    }

    Ast.Node visit(Ast.AstQuasiQuoteExpr node)
    {
        /*
            #~"{...}
                --> __builtin__("ouro.quasiquote"){...}
        */

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/true,
                new Ast.BuiltinExpr(node.loc, "ouro.quasiquote"),
                [cast(Ast.Expr) node.expr]
            )
        );
    }

    Ast.Node visit(Ast.AstQQSubExpr node)
    {
        /*
            #~${...}
                --> __builtin__("ouro.mixin"){...}
        */

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/true,
                new Ast.BuiltinExpr(node.loc, "ouro.mixin"),
                [cast(Ast.Expr) node.expr]
            )
        );
    }

    Ast.Node visit(Ast.LetExpr node)
    {
        /*
            let(bind..., expr)
                --> __builtin__("ouro.let")(bind..., expr)
        */

        auto bindExprs = node.bindExprs.dup;
        auto subExpr = visitExpr(node.subExpr);
        auto unchanged = true;

        foreach( i,ref bindExpr ; bindExprs )
        {
            auto expr = visitExpr(bindExpr);
            unchanged &= (expr is bindExpr);
            bindExpr = expr;
        }

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                new Ast.BuiltinExpr(node.loc, "ouro.let"),
                bindExprs ~ [subExpr]
            )
        );
    }

    Ast.Node visit(Ast.ImportExpr node)
    {
        /*
            import(scope, symbols, subExpr)
                --> __builtin__("ouro.import")(scope, symbols, subExpr)
        */

        auto scopeExpr = visitExpr(node.scopeExpr);
        auto symbolsExpr = visitExpr(node.symbolsExpr);
        auto subExpr = visitExpr(node.subExpr);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                new Ast.BuiltinExpr(node.loc, "ouro.import"),
                [scopeExpr, symbolsExpr, subExpr]
            )
        );
    }

    Ast.Node visit(Ast.TernaryExpr node)
    {
        /*
            lhs (Op) mid (Op) rhs
                --> __builtin__("ouro.op(Op)")(lhs, mid, rhs)
         */

        auto lhs = visitExpr(node.lhs);
        auto mid = visitExpr(node.mid);
        auto rhs = visitExpr(node.rhs);

        return new Ast.RewrittenExpr(node.loc, node,
            new Ast.CallExpr(node.loc, /*isMacro*/false,
                new Ast.BuiltinExpr(node.loc, "ouro.op"~node.opToString(node.op)),
                [lhs, mid, rhs]
            )
        );
    }

    Ast.Node visit(Ast.BuiltinExpr node)
    {
        return node;
    }
}

