/**
    Semantic Information Tree nodes.

    Yes, that's what "sit" stands for.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sit.Nodes;

import ouro.Location;

import Ast = ouro.ast.Nodes;

class Scope
{
    Value[char[]] entries;
    Scope parent;

    this(Scope parent)
    {
        this.parent = parent;
    }

    void bind(char[] ident, Value value)
    {
        entries[ident] = value;
    }

    void bindArg(Ast.Node node, Scope scop, char[] ident)
    {
        entries[ident] = new ArgumentValue(node, scop, ident);
    }

    Value lookup(Ast.Node astNode, char[] ident)
    {
        return lookup(astNode, ident, true);
    }

    Value lookup(Ast.Node astNode, char[] ident, bool allowFallbacks)
    {
        if( auto entry = (ident in entries) )
            return *entry;
        
        else if( parent !is null )
            return parent.lookup(astNode, ident, allowFallbacks);

        return null;
    }
}

class PartialScope : Scope
{
    bool complete = false;

    Value[char[]] quantumCache;

    this(Scope parent)
    {
        super(parent);
    }

    void fix()
    {
        complete = true;
    }

    void defer(Ast.Node astNode, char[] ident)
    {
        bind(ident, new DeferredValue(astNode, this, ident));
    }

    override Value lookup(Ast.Node astNode, char[] ident, bool allowFallbacks)
    {
        auto v = super.lookup(astNode, ident, false);

        if( allowFallbacks && ! complete && v is null )
        {
            if( auto entry = (ident in quantumCache) )
                return *entry;

            auto qv = new QuantumValue(astNode, this, ident);
            quantumCache[ident] = qv;
            return qv;
        }
        else
            return v;
    }
}

class Node
{
    Ast.Node astNode;

    this(Ast.Node astNode)
    {
        this.astNode = astNode;
    }
}

class Module : Node
{
    Stmt[] stmts;
    char[][] exportSymbols;
    Scope scop;

    this(Ast.Node astNode, Stmt[] stmts, char[][] exportSymbols, Scope scop)
    in
    {
        assert( scop !is null );
    }
    body
    {
        super(astNode);
        this.stmts = stmts;
        this.exportSymbols = exportSymbols;
        this.scop = scop;
    }
}

struct Stmt
{
    Location loc;
    Expr expr;
    bool bind;
    char[] bindIdent;
    bool mergeAll;
    char[][] mergeList;

    static Stmt opCall(Location loc, Expr expr)
    {
        Stmt r;
        r.loc = loc;
        r.expr = expr;
        return r;
    }

    static Stmt opCall(Location loc, Expr expr, char[] bindIdent)
    {
        auto r = Stmt(loc, expr);
        r.bind = true;
        r.bindIdent = bindIdent;
        return r;
    }

    static Stmt opCall(Location loc, Expr expr, bool mergeAll, char[][] mergeList)
    {
        auto r = Stmt(loc, expr);
        r.mergeAll = mergeAll;
        r.mergeList = mergeList;
        return r;
    }
}

/**
    Base class for all evaluable expressions.
 */
class Expr : Node
{
    this(Ast.Node astNode)
    {
        super(astNode);
    }
}

class AstMixinExpr : Expr
{
    Expr expr;

    this(Ast.Node astNode, Expr expr)
    in
    {
        assert( expr !is null );
    }
    body
    {
        super(astNode);
        this.expr = expr;
    }
}

class CallExpr : Expr
{
    Expr funcExpr;
    CallArg[] args;

    this(Ast.Node astNode, Expr funcExpr, CallArg[] args)
    in
    {
        assert( funcExpr !is null );
    }
    body
    {
        super(astNode);
        this.funcExpr = funcExpr;
        this.args = args;
    }
}

struct CallArg
{
    Expr expr;
    bool explode = false;

    static CallArg opCall(Expr expr, bool explode)
    {
        CallArg r;
        r.expr = expr;
        r.explode = explode;
        return r;
    }
}

abstract class Value : Expr
{
    this(Ast.Node astNode)
    {
        super(astNode);
    }
}

class ArgumentValue : Value
{
    Scope scop;
    char[] ident;

    this(Ast.Node astNode, Scope scop, char[] ident)
    in
    {
        assert( scop !is null );
        assert( ident != "" );
    }
    body
    {
        super(astNode);
        this.scop = scop;
        this.ident = ident;
    }
}

class DeferredValue : Value
{
    Scope scop;
    char[] ident;

    this(Ast.Node astNode, Scope scop, char[] ident)
    in
    {
        assert( scop !is null );
        assert( ident != "" );
    }
    body
    {
        super(astNode);
        this.scop = scop;
        this.ident = ident;
    }

    Value resolve()
    {
        return scop.lookup(astNode, ident);
    }
}

class QuantumValue : Value
{
    Scope scop;
    char[] ident;

    this(Ast.Node astNode, Scope scop, char[] ident)
    in
    {
        assert( scop !is null );
        assert( ident != "" );
    }
    body
    {
        super(astNode);
        this.scop = scop;
        this.ident = ident;
    }

    Value resolve()
    {
        return scop.lookup(astNode, ident);
    }
}

class AstQuoteValue : Value
{
    Ast.Node ast;

    this(Ast.Node astNode, Ast.Node ast)
    in
    {
        assert( ast !is null );
    }
    body
    {
        super(astNode);
        this.ast = ast;
    }
}

class FunctionValue : Value
{
    char[] name;
    Argument[] args;
    Scope scop;

    Expr expr;

    alias Value function(Value[]) HostFn;
    HostFn hostFn;

    this(Ast.Node astNode, char[] name, Argument[] args, Scope scop, Expr expr)
    in
    {
        assert( name != "" );
        assert( scop !is null );
        assert( expr !is null );
    }
    body
    {
        super(astNode);
        this.name = name;
        this.args = args;
        this.scop = scop;
        this.expr = expr;
    }

    this(char[] name, Argument[] args, HostFn hostFn)
    in
    {
        assert( name != "" );
        assert( hostFn !is null );
    }
    body
    {
        super(null);
        this.name = name;
        this.args = args;
        this.hostFn = hostFn;
    }
}

struct Argument
{
    Location loc;
    char[] ident;
    bool isVararg;

    static Argument opCall(Location loc, char[] ident, bool isVararg)
    in
    {
        assert( ident != "" );
    }
    body
    {
        Argument r;
        r.loc = loc;
        r.ident = ident;
        r.isVararg = isVararg;
        return r;
    }
}

class ListValue : Value
{
    Expr[] elemExprs;

    this(Ast.Node astNode, Expr[] elemExprs)
    {
        super(astNode);
        this.elemExprs = elemExprs;
    }
}

class LogicalValue : Value
{
    bool value;

    this(Ast.Node astNode, bool value)
    {
        super(astNode);
        this.value = value;
    }
}

class MapValue : Value
{
    KeyValuePair[] keyValuePairs;

    this(Ast.Node astNode, KeyValuePair[] keyValuePairs)
    {
        super(astNode);
        this.keyValuePairs = keyValuePairs;
    }
}

struct KeyValuePair
{
    Location loc;
    Expr key, value;

    static KeyValuePair opCall(Location loc, Expr key, Expr value)
    in
    {
        assert( key !is null );
        assert( value !is null );
    }
    body
    {
        KeyValuePair r;
        r.loc = loc;
        r.key = key;
        r.value = value;
        return r;
    }
}

class ModuleValue : Value
{
    Module modul;

    this(Ast.Node astNode, Module modul)
    in
    {
        assert( modul !is null );
    }
    body
    {
        super(astNode);
        this.modul = modul;
    }
}

class NilValue : Value
{
    this(Ast.Node astNode)
    {
        super(astNode);
    }
}

class StringValue : Value
{
    char[] value;

    this(Ast.Node astNode, char[] value)
    {
        super(astNode);
        this.value = value;
    }
}

class NumberValue : Value
{
    real value;

    this(Ast.Node astNode, real value)
    {
        super(astNode);
        this.value = value;
    }
}

