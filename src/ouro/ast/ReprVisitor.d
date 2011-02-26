/**
    Turns an AST into an equivalent source-code representation.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.ast.ReprVisitor;

import Float = tango.text.convert.Float;

import ouro.ast.Visitor;
import ouro.util.Repr : reprIdent, reprString, reprReal, reprLogical, reprNil;
import ouro.util.StructuredOutput;

import Ast = ouro.ast.Nodes;

class ReprVisitor : Visitor!()
{
    StructuredOutput so;

    this(StructuredOutput so)
    {
        this.so = so;
    }

    override void visit(Ast.ImportStmt node)
    {
        so.p("import ");

        if( node.ident != "" )
            so("{} = ", reprIdent(node.ident));

        so(reprString(node.modulePath));

        if( node.all )
            so(" : *").l;

        else if( node.symbols.length > 0 )
        {
            foreach( i,sym ; node.symbols )
                so( (i==0) ? " : " : ", " )(sym);

            so.l;
        }
    }

    override void visit(Ast.LetExprStmt node)
    {
        so.f("let {} = ", node.ident);
        visitBase(node.expr);
        so.l;
    }

    override void visit(Ast.LetFuncStmt node)
    {
        so.f("let {}{}", node.isMacro ? "macro " : "",
                reprIdent(node.ident));
        foreach( i,arg ; node.args )
            so.r((i==0)?"(":",")(reprIdent(arg.ident))
                (arg.isVararg?"...":"");
        so(") = ");
        visitBase(node.expr);
        so.l;
    }

    override void visit(Ast.BinaryExpr node)
    {
        so.r("(").push;
        visitBase(node.lhs);
        so.pop.r(")")(node.opRepr(node.op))("(").push;
        visitBase(node.rhs);
        so.pop.r(")");
    }

    override void visit(Ast.InfixFuncExpr node)
    {
        so.r("(").push;
        visitBase(node.lhs);
        so.pop.r(")(.");
        visitBase(node.funcExpr);
        so.r(".)(").push;
        visitBase(node.rhs);
        so.pop.r(")");
    }

    override void visit(Ast.PrefixExpr node)
    {
        so.r(node.opRepr(node.op))(" (").push;
        visitBase(node.subExpr);
        so.pop.r(")");
    }

    override void visit(Ast.PostfixFuncExpr node)
    {
        so.r("(").push;
        visitBase(node.subExpr);
        so.pop.r(")(.");
        visitBase(node.funcExpr);
        so.r(")");
    }

    override void visit(Ast.NumberExpr node)
    {
        so.r(reprReal(node.value));
    }

    override void visit(Ast.StringExpr node)
    {
        so.r(reprString(node.value));
    }

    override void visit(Ast.LogicalExpr node)
    {
        so.r(reprLogical(node.value));
    }

    override void visit(Ast.NilExpr node)
    {
        so.r(reprNil);
    }

    override void visit(Ast.ListExpr node)
    {
        if( node.elemExprs.length == 0 )
            so.r("[]");

        else
        {
            foreach( i,expr ; node.elemExprs )
            {
                if( i == 0 )
                    so.r("[").push;
                else
                    so.r(", ");

                visitBase(expr);
            }
            so.pop.r("]");
        }
    }

    override void visit(Ast.MapExpr node)
    {
        if( node.keyValuePairs.length == 0 )
            so.r("[: :]");
        else
        {
            foreach( i,kvp ; node.keyValuePairs )
            {
                if( i == 0 )
                    so.r("[:").push;
                else
                    so.r(", ");

                so.r("(").push;
                visitBase(kvp.key);
                so.pop.r("):(").push;
                visitBase(kvp.value);
                so.pop.r(")");
            }
            so.pop.r(":]");
        }
    }

    override void visit(Ast.LambdaExpr node)
    {
        so.r("(").push;
        foreach( i,arg ; node.args )
        {
            if( i == 0 )
                so.r("\\");
            else
                so.r(",");

            so.r(arg.ident);
            if( arg.isVararg )
                so.r("...");
        }
        so.r(".");
        visitBase(node.expr);
        so.pop.r(")");
    }

    override void visit(Ast.ExplodeExpr node)
    {
        so.r("(").push;
        visitBase(node.subExpr);
        so.pop.r(")...");
    }

    override void visit(Ast.CallExpr node)
    {
        visitBase(node.funcExpr);
        
        foreach( i,expr ; node.argExprs )
        {
            if( i == 0 )
                so.r(node.isMacro ? "{" : "(").push;
            else
                so.r(", ");

            visitBase(expr);
        }
        so.pop.r(node.isMacro ? "}" : ")");
    }

    override void visit(Ast.VariableExpr node)
    {
        so.r(node.ident);
    }

    override void visit(Ast.RangeExpr node)
    {
        so.r("range ").r(node.incLower ? "[" : "(").push;
        visitBase(node.lowerExpr);
        so.r(", ");
        visitBase(node.upperExpr);
        so.pop.r(node.incUpper ? "]" : ")");
    }

    override void visit(Ast.AstQuoteExpr node)
    {
        so.r(`#~'{`).push;
        visitBase(node.expr);
        so.pop.r("}");
    }

    override void visit(Ast.AstQuasiQuoteExpr node)
    {
        so.r(`#~"{`).push;
        visitBase(node.expr);
        so.pop.r("}");
    }

    override void visit(Ast.AstQQSubExpr node)
    {
        if( node.expr is null )
        {
            so.rf(`#~${}`, node.index);
        }
        else
        {
            so.r(`#~${`).push;
            visitBase(node.expr);
            so.pop.r("}");
        }
    }

    override void visit(Ast.LetExpr node)
    {
        so.r("let").push;
        foreach( i,expr ; node.bindExprs )
        {
            if( i == 0 )
                so.r("(").push;
            else
                so.r(", ");

            visitBase(expr);
        }

        if( node.bindExprs.length > 0 )
            so.r(", ");

        visitBase(node.subExpr);
        so.pop.r(")");
    }

    override void visit(Ast.ImportExpr node)
    {
        so.r("import(").push;
        visitBase(node.scopeExpr);
        so.r(", ");
        visitBase(node.symbolsExpr);
        so.r(", ");
        visitBase(node.subExpr);
        so.pop.r(")");
    }

    override void visit(Ast.TernaryExpr node)
    {
        char[] lho, rho;
        node.opRepr(node.op, lho, rho);

        so.r("(").push;
        visitBase(node.lhs);
        so.r(")")(lho)("(");
        visitBase(node.mid);
        so.r(")")(rho)("(");
        visitBase(node.rhs);
        so.pop.r(")");
    }

    override void visit(Ast.BuiltinExpr node)
    {
        so.rf("__builtin__({})", reprString(node.ident));
    }
}

