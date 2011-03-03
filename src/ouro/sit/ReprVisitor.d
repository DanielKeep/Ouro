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

class ReprVisitor : Visitor!(void, bool)
{
    StructuredOutput so;
    AstRepr.ReprVisitor arv;

    this(StructuredOutput so)
    {
        this.so = so;
        this.arv = new AstRepr.ReprVisitor(so);
    }

    static ReprVisitor forStdout()
    {
        return new ReprVisitor(StructuredOutput.forStdout);
    }

    static ReprVisitor forStderr()
    {
        return new ReprVisitor(StructuredOutput.forStderr);
    }

    override void visitScope(Sit.Scope scop, bool showDef)
    {
        scopeName(scop);
        so.r(" {").push.l;
        foreach( k,v ; scop.entries )
        {
            so.p(reprIdent(k)).r(": ");
            visitBase(v, true);
            so.l;
        }
        so.pop.p("}").l;
    }

    final void scopeName(Sit.Scope scop)
    {
        so.rf("0x{:x,8}", cast(void*) scop);
    }

    void visitBaseDef(Sit.Node node)
    {
        return visitBase(node, false);
    }

    override void visit(Sit.Module node, bool showDef)
    {
        so.push("{");
        {
            so.p("scope: ");
            visitScope(node.scop,false);

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
                so.p("{").push;
                {
                    visitBaseDef(stmt.expr);
                }
                so.pop.p("}").l;
            }
            so.pop.p("]").l;
        }
        so.pop.p("}").l;
    }

    override void visit(Sit.CallExpr node, bool showDef)
    {
        so.r("Call ").r("{").push.l;
        {
            so.p("funcExpr: ").push;
            visitBaseDef(node.funcExpr);
            so.pop.l;

            so.p("args: ").push("[").l;
            foreach( arg ; node.args )
            {
                so.indent;
                if( arg.explode )
                    so.r("explode ");
                visitBaseDef(arg.expr);
                so.l;
            }
            so.pop.p("]").l;
        }
        so.pop.p("}");
    }

    override void visit(Sit.ArgumentValue node, bool showDef)
    {
        so.r("Argument { ");
        scopeName(node.scop);
        so.r(" : ").r(reprIdent(node.ident)).r(" }");
    }

    override void visit(Sit.EnclosedValue node, bool showDef)
    {
        so.r("Enclosed ");
        visitBase(node.value, showDef);
    }

    override void visit(Sit.QuantumValue node, bool showDef)
    {
        so.r("Quantum { ");
        scopeName(node.scop);
        so.r(" : ").r(reprIdent(node.ident)).r(" }");
    }

    override void visit(Sit.AstQuoteValue node, bool showDef)
    {
        so.r("AstQuote { ").push;
        arv.visitBase(node.ast);
        so.pop.r(" }").l;
    }
    
    override void visit(Sit.ClosureValue node, bool showDef)
    {
        so.r("Closure {").push.l;
        {
            so.p("func: ");
            visitBase(node.fn, showDef);
            so.l();

            so.p("values: [").push.l;
            foreach( v ; node.values )
            {
                so.indent;
                visitBase(v, false);
                so.l;
            }
            so.pop.p("]").l;
        }
        so.pop.p("}");
    }

    override void visit(Sit.FunctionValue node, bool showDef)
    {
        so.r("Function ").r(reprString(node.name));
        if( showDef )
        {
            so.r(" {").push.l;
            so.p("args: [").push.l;

            foreach( arg ; node.args )
            {
                so.p(arg.ident);
                if( arg.isVararg )
                    so.r("...");
                so.l;
            }

            so.pop.p("]").l;
            if( node.expr !is null )
            {
                so.p("expr: ");
                visitBaseDef(node.expr);
                so.l;

                if( node.enclosedValues.length > 0 )
                {
                    so.p("enclosed: [").push.l;
                    foreach( ev ; node.enclosedValues )
                    {
                        so.indent;
                        visitBase(ev, false);
                        so.l;
                    }
                    so.pop.p("]").l;
                }
            }
            else if( node.host.fn !is null )
            {
                so.fl("host: 0x{:x,8}", cast(void*) node.host.fn);
            }
            so.pop.p("}").l;
        }
    }

    override void visit(Sit.ListExpr node, bool showDef)
    {
        so.r("List Expr ").push("[");
        if( node.elemExprs.length > 0 )
            so.l;
        foreach( elemExpr ; node.elemExprs )
        {
            so.indent;
            visitBaseDef(elemExpr);
            so.l;
        }
        so.pop.p("]").l;
    }

    override void visit(Sit.ListValue node, bool showDef)
    {
        so.r("List ").push("[");
        if( node.elemValues.length > 0 )
            so.l;
        foreach( elemValue ; node.elemValues )
        {
            so.indent;
            visitBaseDef(elemValue);
            so.l;
        }
        so.pop.p("]").l;
    }

    override void visit(Sit.LogicalValue node, bool showDef)
    {
        so.rf("Logical {}", node.value);
    }

    override void visit(Sit.MapExpr node, bool showDef)
    {
        so.r("Map Expr ").push("{");
        foreach( kvp ; node.kvps )
        {
            so.push("{");
            visitBaseDef(kvp.key);
            visitBaseDef(kvp.value);
            so.pop("}").l;
        }
        so.pop("}").l;
    }

    override void visit(Sit.MapValue node, bool showDef)
    {
        so.r("Map ").push("{");
        foreach( kvp ; node.kvps )
        {
            so.push("{");
            visitBaseDef(kvp.key);
            visitBaseDef(kvp.value);
            so.pop("}").l;
        }
        so.pop("}").l;
    }

    override void visit(Sit.ModuleValue node, bool showDef)
    {
        so.r("Module ").r(reprString(node.modul.astNode.loc.file));
    }

    override void visit(Sit.NilValue node, bool showDef)
    {
        so.r("Nil");
    }

    override void visit(Sit.StringValue node, bool showDef)
    {
        so.r("String ").r(reprString(node.value));
    }

    override void visit(Sit.NumberValue node, bool showDef)
    {
        so.r("Number ").r(reprReal(node.value));
    }
}

