/**
    Utilities used for defining builtins.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.Util;

public:

import ouro.sem.InvokeFn : invoke;
import ouro.Location;

import Ast = ouro.ast.Nodes;
import Builtins = ouro.sem.builtins.Builtins;
import Sit = ouro.sit.Nodes;

alias Sit.EvalContext EC;
alias Sit.Value Value;

void chkArgNum(Value[] args, size_t num)
{
    assert( args.length == num );
}

void chkArgNumMin(Value[] args, size_t num)
{
    assert( args.length >= num );
}

real chkArgNumber(Value[] args, size_t i)
{
    if( auto v = cast(Sit.NumberValue) args[i] )
        return v.value;
    else
        assert( false, "expected number; got " ~ args[i].classinfo.name );
}

bool chkArgLogical(Value[] args, size_t i)
{
    if( auto v = cast(Sit.LogicalValue) args[i] )
        return v.value;
    else
        assert( false, "expected logical; got " ~ args[i].classinfo.name );
}

Sit.CallableValue chkArgCall(Value[] args, size_t i)
{
    if( auto v = cast(Sit.CallableValue) args[i] )
        return v;
    else
        assert( false, "expected function; got " ~ args[i].classinfo.name );
}

Sit.ListValue chkArgList(Value[] args, size_t i)
{
    if( auto v = cast(Sit.ListValue) args[i] )
        return v;
    else
        assert( false, "expected list; got " ~ args[i].classinfo.name );
}

char[] chkArgString(Value[] args, size_t i)
{
    return chkArgStringValue(args, i).value;
}

Sit.StringValue chkArgStringValue(Value[] args, size_t i)
{
    if( auto v = cast(Sit.StringValue) args[i] )
        return v;
    else
        assert( false, "expected string; got " ~ args[i].classinfo.name );
}

char[] chkArgStringOrSymbol(Value[] args, size_t i)
{
    if( auto v = cast(Sit.StringValue) args[i] )
        return v.value;
    else if( auto v = cast(Sit.SymbolValue) args[i] )
        return v.value;
    else
        assert( false, "expected string of symbol; got "
                ~ args[i].classinfo.name );
}

Ast.Expr chkArgAst(Value[] args, size_t i)
{
    if( auto v = cast(Sit.AstQuoteValue) args[i] )
    {
        auto expr = cast(Ast.Expr) v.ast;
        assert( expr !is null );
        return expr;
    }
    else
        assert( false, "expected ast expr; got " ~ args[i].classinfo.name );
}

Ast.Node chkArgAstNode(Value[] args, size_t i)
{
    if( auto v = cast(Sit.AstQuoteValue) args[i] )
    {
        auto expr = cast(Ast.Node) v.ast;
        assert( expr !is null );
        return expr;
    }
    else
        assert( false, "expected ast node; got " ~ args[i].classinfo.name );
}

Sit.Module chkArgModule(Value[] args, size_t i)
{
    if( auto v = cast(Sit.ModuleValue) args[i] )
        return v.modul;
    else
        assert( false, "expected module; got " ~ args[i].classinfo.name );
}

Sit.SymbolValue chkArgSymbol(Value[] args, size_t i)
{
    if( auto v = cast(Sit.SymbolValue) args[i] )
        return v;
    else
        assert( false, "expected symbol; got " ~ args[i].classinfo.name );
}

Object chkArgObject(Value[] args, size_t i, bool silent = false)
{
    if( auto v = cast(Sit.HostObjectValue) args[i] )
        return v.obj;
    else if( silent )
        return null;
    else
        assert( false, "expected host object; got " ~ args[i].classinfo.name );
}

void chkArgNil(Value[] args, size_t i)
{
    auto v = cast(Sit.NilValue) args[i];
    assert( v !is null, "expected nil; got " ~ args[i].classinfo.name );
}

