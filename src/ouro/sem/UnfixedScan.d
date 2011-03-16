/**
    This module ensures there are no unfixed values remaining.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.UnfixedScan;

import ouro.sem.Abort;
import ouro.sit.Visitor;

import Sit = ouro.sit.Nodes;

class UnfixedScanVisitor : Visitor!()
{
    bool[Sit.FunctionValue] visited;

    final Sit.Value check(Sit.UnfixedValue uv)
    {
        auto rv = cast(Sit.Resolvable) uv;
        assert( rv !is null );
        auto v = rv.resolve;
        if( null !is cast(Sit.UnfixedValue) v )
            NonFatalAbort.throwForUnfixed(uv);
        return v;
    }

    override void visit(Sit.DeferredValue node)
    {
        visitBase(check(node));
    }

    override void visit(Sit.QuantumValue node)
    {
        visitBase(check(node));
    }

    override void visit(Sit.FunctionValue node)
    {
        if( node in visited )
            return;
        visited[node] = true;
        if( node.expr !is null )
            visitBase(node.expr);
    }
}

