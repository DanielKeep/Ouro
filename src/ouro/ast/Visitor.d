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
        "Program"[],
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

    char[] genDispatch()
    {
        char[] r;

        foreach( name ; AstClassList )
        {
            r ~= "if( auto n = cast(Ast."~name~") node ) return visit(n);\n";
        }

        return r;
    }
}

abstract class Visitor
{
    void visitBase(Ast.Node node)
    {
        mixin(genDispatch());

        return defaultVisit(node);
    }

    void defaultVisit(Ast.Node node)
    {
        assert(false, "missing visit for "~node.classinfo.name);
    }

    void visit(Ast.Program node)
    {
        foreach( stmt ; node.stmts )
            visitBase(stmt);
    }

    void visit(Ast.ImportStmt node)
    {
    }

    void visit(Ast.LetExprStmt node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.LetFuncStmt node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.ExprStmt node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.RewrittenExpr node)
    {
        // Note: original is not visited
        visitBase(node.rewrite);
    }

    void visit(Ast.BinaryExpr node)
    {
        visitBase(node.lhs);
        visitBase(node.rhs);
    }

    void visit(Ast.InfixFuncExpr node)
    {
        visitBase(node.func);
        visitBase(node.lhs);
        visitBase(node.rhs);
    }

    void visit(Ast.PrefixExpr node)
    {
        visitBase(node.subExpr);
    }

    void visit(Ast.PostfixFuncExpr node)
    {
        visitBase(node.func);
        visitBase(node.subExpr);
    }

    void visit(Ast.NumberExpr node)
    {
    }

    void visit(Ast.StringExpr node)
    {
    }

    void visit(Ast.LogicalExpr node)
    {
    }

    void visit(Ast.NilExpr node)
    {
    }

    void visit(Ast.ListExpr node)
    {
        foreach( elem ; node.elemExprs )
            visitBase(elem);
    }

    void visit(Ast.MapExpr node)
    {
        foreach( kvp ; node.keyValuePairs )
        {
            visitBase(kvp.key);
            visitBase(kvp.value);
        }
    }

    void visit(Ast.LambdaExpr node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.ExplodeExpr node)
    {
        visitBase(node.subExpr);
    }

    void visit(Ast.CallExpr node)
    {
        visitBase(node.funcExpr);
        foreach( arg ; node.argExprs )
            visitBase(arg);
    }

    void visit(Ast.VariableExpr node)
    {
    }

    void visit(Ast.RangeExpr node)
    {
        visitBase(node.lowerExpr);
        visitBase(node.upperExpr);
    }

    void visit(Ast.AstQuoteExpr node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.AstQuasiQuoteExpr node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.AstQQSubExpr node)
    {
        visitBase(node.expr);
    }

    void visit(Ast.LetExpr node)
    {
        foreach( bindExpr ; node.bindExprs )
            visitBase(bindExpr);
        visitBase(node.subExpr);
    }

    void visit(Ast.ImportExpr node)
    {
        visitBase(node.scopeExpr);
        visitBase(node.symbolsExpr);
        visitBase(node.subExpr);
    }

    void visit(Ast.TernaryExpr node)
    {
        visitBase(node.lhs);
        visitBase(node.mid);
        visitBase(node.rhs);
    }
}

