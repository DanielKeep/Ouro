/**
    Rewrite statements into expressions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.ast.StmtRewrite;

import ouro.Location;

import Ast = ouro.ast.Nodes;

Ast.Expr rewriteStmt(Ast.Module mod, Ast.Statement rawStmt,
        out bool bind, out char[] bindIdent,
        out bool mergeAll, out char[][] mergeList)
{
    if( auto stmt = cast(Ast.ImportStmt) rawStmt )
    {
        auto modExpr = new Ast.CallExpr(stmt.loc, /*isMacro*/true,
            new Ast.BuiltinExpr(stmt.loc, "ouro.module"),
            [cast(Ast.Expr) new Ast.StringExpr(stmt.loc, stmt.modulePath)]
        );

        if( stmt.ident != "" )
        {
            if( stmt.symbols.length != 0 )
                assert(false);

            bind = true;
            bindIdent = stmt.ident;
        }
        else
        {
            mergeAll = stmt.all;
            mergeList = stmt.symbols;
        }

        return new Ast.RewrittenExpr(stmt.loc, stmt, modExpr);
    }

    if( auto stmt = cast(Ast.LetExprStmt) rawStmt )
    {
        bind = true;
        bindIdent = stmt.ident;

        return new Ast.RewrittenExpr(stmt.loc, stmt, stmt.expr);
    }

    if( auto stmt = cast(Ast.LetFuncStmt) rawStmt )
    {
        bind = true;
        bindIdent = stmt.ident;

        return new Ast.RewrittenExpr(stmt.loc, stmt,
            new Ast.LambdaExpr(stmt.loc, stmt.isMacro, stmt.args, stmt.expr)
        );
    }

    if( auto stmt = cast(Ast.ExprStmt) rawStmt )
    {
        return new Ast.RewrittenExpr(stmt.loc, stmt, stmt.expr);
    }

    assert(false, "cannot rewrite statement; don't recognise the class");
}

