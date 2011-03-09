/**
    Builtin functions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Builtins;

import tango.math.Math : pow, floor, trunc;

import ouro.Location : Location;
import ouro.sem.InvokeFn : invoke;

import Ast      = ouro.ast.Nodes;
import QQSub    = ouro.ast.QQSubVisitor;
import Sit      = ouro.sit.Nodes;

alias Sit.EvalContext EC;

Sit.Value lookupBuiltin(char[] name)
{
    if( auto fnp = name in builtins )
        return *fnp;
    else
        return null;
}

private:

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

Sit.StringValue chkArgStringValue(Value[] args, size_t i)
{
    if( auto v = cast(Sit.StringValue) args[i] )
        return v;
    else
        assert( false, "expected string; got " ~ args[i].classinfo.name );
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
        assert( false, "expected ast; got " ~ args[i].classinfo.name );
}

Value compareOp(Sit.Order ord)(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    return new Sit.LogicalValue(null, (args[0].order(args[1]) & ord) != 0);
}

private alias Sit.Order Ord;

alias compareOp!(Ord.Eq) ouro_opEq;
alias compareOp!(Ord.Ne) ouro_opNe;
alias compareOp!(Ord.Lt) ouro_opLt;
alias compareOp!(Ord.Le) ouro_opLtEq;
alias compareOp!(Ord.Gt) ouro_opGt;
alias compareOp!(Ord.Ge) ouro_opGtEq;

Value binaryNumberOp(char[] op)(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgNumber(args, 0);
    auto rhs = chkArgNumber(args, 1);
    return new Sit.NumberValue(null, mixin("lhs "~op~" rhs"));
}

alias binaryNumberOp!("+")  ouro_opAdd;
alias binaryNumberOp!("-")  ouro_opSub;
alias binaryNumberOp!("*")  ouro_opMul;
alias binaryNumberOp!("/")  ouro_opDiv;

Value binaryNumberExpr(char[] expr)(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgNumber(args, 0);
    auto rhs = chkArgNumber(args, 1);
    return new Sit.NumberValue(null, mixin(expr));
}

alias binaryNumberExpr!("floor(lhs / rhs)")     ouro_opIntDiv;
alias binaryNumberExpr!("lhs - rhs*floor(lhs / rhs)")   ouro_opMod;
alias binaryNumberExpr!("lhs - rhs*trunc(lhs / rhs)")   ouro_opRem;
alias binaryNumberExpr!("pow(lhs, rhs)")        ouro_opExp;

Value binaryLogicalExpr(bool stopValue)(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgLogical(args, 0);
    auto rhsFn = chkArgCall(args, 1);
    if( lhs == stopValue )
        return new Sit.LogicalValue(null, stopValue);
    else
        return invoke(rhsFn, null);
}

alias binaryLogicalExpr!(false) ouro_opAnd;
alias binaryLogicalExpr!(true)  ouro_opOr;

Value ouro_opComp(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgCall(args, 0);
    auto rhs = chkArgCall(args, 1);
    return Sit.FunctionValue.compose(lhs, rhs);
}

Value ouro_opCons(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = args[0];
    auto rhs = chkArgList(args, 1);
    return Sit.ListValue.cons(lhs, rhs);
}

Value ouro_opJoin(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    if( auto lhs = cast(Sit.ListValue) args[0] )
    {
        auto rhs = chkArgList(args, 1);
        return Sit.ListValue.join(lhs, rhs);
    }
    else if( auto lhs = cast(Sit.StringValue) args[0] )
    {
        auto rhs = chkArgStringValue(args, 1);
        return Sit.StringValue.join(lhs, rhs);
    }
    else
        assert( false, "expected list or string" );
}

Value ternCmpOp(Sit.Order ordL, Sit.Order ordR)(EC ec, Value[] args)
{
    chkArgNum(args, 3);
    return new Sit.LogicalValue(null,
            ((args[0].order(args[1]) & ordL) != 0)
            && ((args[1].order(args[2]) & ordR) != 0));
}

alias ternCmpOp!(Ord.Lt, Ord.Lt) ouro_opLtLt;
alias ternCmpOp!(Ord.Le, Ord.Lt) ouro_opLeLt;
alias ternCmpOp!(Ord.Lt, Ord.Le) ouro_opLtLe;
alias ternCmpOp!(Ord.Le, Ord.Le) ouro_opLeLe;
alias ternCmpOp!(Ord.Gt, Ord.Gt) ouro_opGtGt;
alias ternCmpOp!(Ord.Ge, Ord.Gt) ouro_opGeGt;
alias ternCmpOp!(Ord.Gt, Ord.Ge) ouro_opGtGe;
alias ternCmpOp!(Ord.Ge, Ord.Ge) ouro_opGeGe;

Value unaryOp(alias chkArg, Result, char[] expr)(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto rhs = chkArg(args, 0);
    return new Result(null, mixin(expr));
}

Value ouro_opPos(EC ec, Value[] args)
{
    chkArgNum(args, 0);
    chkArgNumber(args, 0);
    return args[0];
}

alias unaryOp!(chkArgNumber, Sit.NumberValue, "-rhs") ouro_opNeg;
alias unaryOp!(chkArgLogical, Sit.LogicalValue, "! rhs") ouro_opNot;

Value ouro_range(EC ec, Value[] args)
{
    chkArgNum(args, 4);
    auto li = chkArgLogical(args, 0);
    auto ri = chkArgLogical(args, 1);
    return new Sit.RangeValue(null, li, ri, args[2], args[3]);
}

Value ouro_qqsub(EC ec, Value[] args) // ast, subs...
{
    chkArgNum(args, 2);
    auto qq = chkArgAst(args, 0);
    auto subs = chkArgList(args, 1).elemValues;
    auto subExprs = new Ast.Expr[subs.length];
    foreach( i,ref subExpr ; subExprs )
        subExpr = valueToAst(subs[i]);

    scope qqsv = new QQSub.QQSubVisitor;
    return new Sit.AstQuoteValue(null, qqsv.visitExpr(qq, subExprs));
}

Value ouro_let(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto bindExprsList = chkArgList(args,0).elemValues;
    auto subExpr = chkArgAst(args,args.length-1);

    auto bindArgs = new Ast.Argument[](bindExprsList.length);
    auto bindExprs = new Ast.Expr[](bindExprsList.length);

    foreach( i,bindExpr ; bindExprsList )
    {
        auto bindExprAst = cast(Sit.AstQuoteValue) bindExpr;
        assert( bindExprAst !is null );
        auto bindListAst = cast(Ast.ListExpr) bindExprAst.ast;
        assert( bindListAst !is null, "expected list expr, got "
                ~bindExprAst.ast.classinfo.name );

        assert( bindListAst.elemExprs.length == 2,
                "expected two-element list expr" );

        auto identAst = cast(Ast.VariableExpr) bindListAst.elemExprs[0];
        assert( identAst !is null, "expected identifier" );

        bindArgs[i] = Ast.Argument(identAst.loc, identAst.ident, false);
        bindExprs[i] = bindListAst.elemExprs[1];
    }

    return new Sit.AstQuoteValue(null,
            new Ast.CallExpr(Location.init, false,
                new Ast.LambdaExpr(subExpr.loc, false, bindArgs, subExpr),
                bindExprs));
}

Value ouro_import(EC ec, Value[] args)
{
    assert( false, "ouro.import nyi" );
}

Value ouro_branch(EC ec, Value[] args)
{
    chkArgNum(args, 3);
    auto cond = chkArgLogical(args, 0);
    auto b0 = chkArgCall(args, 1);
    auto b1 = chkArgCall(args, 2);

    auto b = cond ? b0 : b1;
    return invoke(b, null, ec);
}

Value ouro_fail(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto msg = chkArgStringValue(args, 0);

    throw new Exception(msg.value);
}

private Ast.Expr valueToAst(Value gv)
{
    if( auto v = cast(Sit.AstQuoteValue) gv )
    {
        return new Ast.AstQuoteExpr(Location.init, cast(Ast.Expr) v.ast);
    }
    else if( auto v = cast(Sit.FunctionValue) gv )
    {
        if( v.srcModule !is null )
        {
            assert( false, "lookup nyi" );
        }
        else
        {
            assert( v.host.fn !is null || v.host.dg !is null );
            return new Ast.BuiltinExpr(Location.init, v.name);
        }
    }
    else if( auto v = cast(Sit.ListValue) gv )
    {
        auto elemExprs = new Ast.Expr[v.elemValues.length];
        foreach( i,elemValue ; v.elemValues )
            elemExprs[i] = valueToAst(elemValue);
        
        return new Ast.ListExpr(Location.init, elemExprs);
    }
    else if( auto v = cast(Sit.LogicalValue) gv )
    {
        return new Ast.LogicalExpr(Location.init, v.value);
    }
    else if( auto v = cast(Sit.MapValue) gv )
    {
        assert( false, "map to ast nyi" );
    }
    else if( auto v = cast(Sit.NilValue) gv )
    {
        return new Ast.NilExpr(Location.init);
    }
    else if( auto v = cast(Sit.StringValue) gv )
    {
        return new Ast.StringExpr(Location.init, v.value);
    }
    else if( auto v = cast(Sit.NumberValue) gv )
    {
        return new Ast.NumberExpr(Location.init, v.value);
    }
    else if( auto v = cast(Sit.RangeValue) gv )
    {
        return new Ast.RangeExpr(Location.init,
            v.incLower, v.incUpper,
            valueToAst(v.lowerValue), valueToAst(v.upperValue));
    }
}

Value ouro_ast(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto gv = args[0]; // generic value

    return new Sit.AstQuoteValue(null, valueToAst(gv));
}

import ouro.sit.ReprVisitor : ReprVisitor;

ReprVisitor repr;
Sit.NilValue nil;

static this()
{
    repr = ReprVisitor.forStdout;
    nil = new Sit.NilValue(null);
}

Value ouro_dot_dump(EC ec, Value[] args)
{
    auto values = chkArgList(args, 0).elemValues;
    foreach( value ; values )
    {
        repr.visitBase(value, true);
        repr.so.l;
    }

    return nil;
}

Sit.FunctionValue[char[]] builtins;

static this()
{
    builtins["ouro.opEq"] = new Sit.FunctionValue("ouro.opEq", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opEq);
    builtins["ouro.opNe"] = new Sit.FunctionValue("ouro.opNe", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opNe);
    builtins["ouro.opLt"] = new Sit.FunctionValue("ouro.opLt", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opLt);
    builtins["ouro.opLtEq"] = new Sit.FunctionValue("ouro.opLtEq", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opLtEq);
    builtins["ouro.opGt"] = new Sit.FunctionValue("ouro.opGt", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opGt);
    builtins["ouro.opGtEq"] = new Sit.FunctionValue("ouro.opGtEq", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opGtEq);

    builtins["ouro.opAdd"] = new Sit.FunctionValue("ouro.opAdd", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opAdd);
    builtins["ouro.opSub"] = new Sit.FunctionValue("ouro.opSub", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opSub);
    builtins["ouro.opMul"] = new Sit.FunctionValue("ouro.opMul", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opMul);
    builtins["ouro.opDiv"] = new Sit.FunctionValue("ouro.opDiv", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opDiv);
    builtins["ouro.opIntDiv"] = new Sit.FunctionValue("ouro.opIntDiv", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opIntDiv);
    builtins["ouro.opMod"] = new Sit.FunctionValue("ouro.opMod", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opMod);
    builtins["ouro.opRem"] = new Sit.FunctionValue("ouro.opRem", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opRem);
    builtins["ouro.opExp"] = new Sit.FunctionValue("ouro.opExp", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opExp);

    builtins["ouro.opAnd"] = new Sit.FunctionValue("ouro.opAnd", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opAnd);
    builtins["ouro.opOr"] = new Sit.FunctionValue("ouro.opOr", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opOr);

    builtins["ouro.opComp"] = new Sit.FunctionValue("ouro.opComp", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opComp);
    builtins["ouro.opCons"] = new Sit.FunctionValue("ouro.opCons", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opCons);
    builtins["ouro.opJoin"] = new Sit.FunctionValue("ouro.opJoin", [Sit.Argument("lhs"), Sit.Argument("rhs")], &ouro_opJoin);

    builtins["ouro.opLtLt"] = new Sit.FunctionValue("ouro.opLtLt", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opLtLt);
    builtins["ouro.opLeLt"] = new Sit.FunctionValue("ouro.opLeLt", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opLeLt);
    builtins["ouro.opLtLe"] = new Sit.FunctionValue("ouro.opLtLe", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opLtLe);
    builtins["ouro.opLeLe"] = new Sit.FunctionValue("ouro.opLeLe", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opLeLe);
    builtins["ouro.opGtGt"] = new Sit.FunctionValue("ouro.opGtGt", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opGtGt);
    builtins["ouro.opGeGt"] = new Sit.FunctionValue("ouro.opGeGt", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opGeGt);
    builtins["ouro.opGtGe"] = new Sit.FunctionValue("ouro.opGtGe", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opGtGe);
    builtins["ouro.opGeGe"] = new Sit.FunctionValue("ouro.opGeGe", [Sit.Argument("lhs"), Sit.Argument("mid"), Sit.Argument("rhs")], &ouro_opGeGe);

    builtins["ouro.opPos"] = new Sit.FunctionValue("ouro.opPos", [Sit.Argument("r")], &ouro_opPos);
    builtins["ouro.opNeg"] = new Sit.FunctionValue("ouro.opNeg", [Sit.Argument("r")], &ouro_opNeg);
    builtins["ouro.opNot"] = new Sit.FunctionValue("ouro.opNot", [Sit.Argument("l")], &ouro_opNot);

    builtins["ouro.range"] = new Sit.FunctionValue("ouro.range", [Sit.Argument("li"), Sit.Argument("ui"), Sit.Argument("lv"), Sit.Argument("uv")], &ouro_range);
    builtins["ouro.qqsub"] = new Sit.FunctionValue("ouro.qqsub", [Sit.Argument("ast"), Sit.Argument("subs", true)], &ouro_qqsub);
    builtins["ouro.let"] = new Sit.FunctionValue("ouro.let", [Sit.Argument("bindings", true), Sit.Argument("expr")], &ouro_let);
    builtins["ouro.import"] = new Sit.FunctionValue("ouro.import", [Sit.Argument("scope"), Sit.Argument("symbols"), Sit.Argument("expr")], &ouro_import);
    builtins["ouro.branch"] = new Sit.FunctionValue("ouro.branch", [Sit.Argument("cond"), Sit.Argument("b0"), Sit.Argument("b1")], &ouro_branch);
    builtins["ouro.fail"] = new Sit.FunctionValue("ouro.fail", [Sit.Argument("msg")], &ouro_fail);
    builtins["ouro.ast"] = new Sit.FunctionValue("ouro.ast", [Sit.Argument("value")], &ouro_ast);

    builtins["ouro..dump"] = new Sit.FunctionValue("ouro..dump", [Sit.Argument("values", true)], &ouro_dot_dump, Sit.EvalContext.Runtime);
}

