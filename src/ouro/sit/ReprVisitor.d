/**
    Repr printer for SITs.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sit.ReprVisitor;

import ouro.sit.Visitor;
import ouro.util.Repr : reprIdent, reprString, reprReal;
import ouro.util.StructuredOutput;

import AstRepr = ouro.ast.ReprVisitor;
import Sit = ouro.sit.Nodes;

class ReprVisitor : Visitor!()
{
    StructuredOutput so;
    AstRepr.ReprVisitor arv;

    this(StructuredOutput so)
    {
        this.so = so;
        this.arv = new AstRepr.ReprVisitor(so);
    }

    override void visitScope(Sit.Scope scop)
    {
        scopeName(scop);
        so.r(" {").push.l;
        foreach( k,v ; scop.entries )
        {
            so.p(reprIdent(k)).r(": ");
            visitBase(v);
            so.l;
        }
        so.pop.p("}").l;
    }

    final void scopeName(Sit.Scope scop)
    {
        so.rf("0x{:x,8}", cast(void*) scop);
    }

    override void visit(Sit.Module node)
    {
        so.push("{");
        {
            so.p("scope: ");
            visitScope(node.scop);

            so.p("exportSymbols: ");
            so.r("[");
            foreach( i,exportSymbol ; node.exportSymbols )
                so.r(i==0 ? "" : ", ").r(reprIdent(exportSymbol));
            so.r("]").l;

            so.p("stmts: ").push("[").l;
            foreach( stmt ; node.stmts )
            {
                so.indent;
                if( stmt.bind )
                    so.r("bind ").r(reprIdent(stmt.bindIdent)).r(" ");
                if( stmt.mergeAll )
                    so.r("merge * ");
                else if( stmt.mergeList.length > 0 )
                {
                    so.r("merge ");
                    foreach( i,merge ; stmt.mergeList )
                        so.r(i==0 ? "[" : ", ").r(reprIdent(merge));
                    so.r("] ");
                }
                so.r("{").push;
                {
                    visitBase(stmt.expr);
                }
                so.r("}").pop.l;
            }
            so.pop("]").l;
        }
        so.pop("}").l;
    }

    override void visit(Sit.AstMixinExpr node)
    {
        so.r("AstMixin { ").push;
        visitBase(node.expr);
        so.pop.r(" }").l;
    }

    override void visit(Sit.CallExpr node)
    {
        so.r("Call ").r("{").push.l;
        {
            so.p("funcExpr: ").push;
            visitBase(node.funcExpr);
            so.pop.l;

            so.p("args: ").push("[").l;
            foreach( arg ; node.args )
            {
                so.indent;
                if( arg.explode )
                    so.r("explode ");
                visitBase(arg.expr);
                so.l;
            }
            so.pop.p("]").l;
        }
        so.pop.p("}");
    }

    override void visit(Sit.ArgumentValue node)
    {
        so.r("Argument { ");
        scopeName(node.scop);
        so.r(" : ").r(reprIdent(node.ident)).r(" }");
    }

    override void visit(Sit.QuantumValue node)
    {
        so.r("Quantum { ");
        scopeName(node.scop);
        so.r(" : ").r(reprIdent(node.ident)).r(" }");
    }

    override void visit(Sit.AstQuoteValue node)
    {
        so.r("AstQuote { ").push;
        arv.visitBase(node.ast);
        so.pop.r(" }").l;
    }

    override void visit(Sit.FunctionValue node)
    {
        so.r("Function ").r(reprString(node.name));
    }

    override void visit(Sit.ListExpr node)
    {
        so.r("List Expr ").push("[");
        foreach( elemExpr ; node.elemExprs )
            visitBase(elemExpr);
        so.pop("]").l;
    }

    override void visit(Sit.ListValue node)
    {
        so.r("List ").push("[");
        foreach( elemValue ; node.elemValues )
            visitBase(elemValue);
        so.pop("]").l;
    }

    override void visit(Sit.LogicalValue node)
    {
        so.rf("Logical {}", node.value);
    }

    override void visit(Sit.MapExpr node)
    {
        so.r("Map Expr ").push("{");
        foreach( kvp ; node.kvps )
        {
            so.push("{");
            visitBase(kvp.key);
            visitBase(kvp.value);
            so.pop("}").l;
        }
        so.pop("}").l;
    }

    override void visit(Sit.MapValue node)
    {
        so.r("Map ").push("{");
        foreach( kvp ; node.kvps )
        {
            so.push("{");
            visitBase(kvp.key);
            visitBase(kvp.value);
            so.pop("}").l;
        }
        so.pop("}").l;
    }

    override void visit(Sit.ModuleValue node)
    {
        so.r("Module ").r(reprString(node.modul.astNode.loc.file));
    }

    override void visit(Sit.NilValue node)
    {
        so.r("Nil");
    }

    override void visit(Sit.StringValue node)
    {
        so.r("String ").r(reprString(node.value));
    }

    override void visit(Sit.NumberValue node)
    {
        so.r("Number ").r(reprReal(node.value));
    }
}

