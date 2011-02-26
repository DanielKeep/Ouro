/**
    Builtin functions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Builtins;

import tango.math.Math : pow, floor;

import ouro.sem.InvokeFn : invokeFn;

import Ast      = ouro.ast.Nodes;
import QQSub    = ouro.ast.QQSubVisitor;
import Sit      = ouro.sit.Nodes;

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

Sit.FunctionValue chkArgFn(Value[] args, size_t i)
{
    if( auto v = cast(Sit.FunctionValue) args[i] )
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

Value compareOp(Sit.Order ord)(Value[] args)
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

Value binaryNumberOp(char[] op)(Value[] args)
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

Value binaryNumberExpr(char[] expr)(Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgNumber(args, 0);
    auto rhs = chkArgNumber(args, 1);
    return new Sit.NumberValue(null, mixin(expr));
}

alias binaryNumberExpr!("floor(lhs / rhs)")     ouro_opIntDiv;
alias binaryNumberExpr!("pow(lhs, rhs)")        ouro_opExp;

Value binaryLogicalExpr(char[] expr)(Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgLogical(args, 0);
    auto rhs = chkArgLogical(args, 1);
    return new Sit.LogicalValue(null, mixin(expr));
}

alias binaryLogicalExpr!("lhs && rhs")  ouro_opAnd;
alias binaryLogicalExpr!("lhs || rhs")  ouro_opOr;

Value ouro_opComp(Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = chkArgFn(args, 0);
    auto rhs = chkArgFn(args, 1);
    return lhs.compose(rhs);
}

Value ouro_opCons(Value[] args)
{
    chkArgNum(args, 2);
    auto lhs = args[0];
    auto rhs = chkArgList(args, 1);
    return Sit.ListValue.cons(lhs, rhs);
}

Value ouro_opJoin(Value[] args)
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

Value ternCmpOp(Sit.Order ordL, Sit.Order ordR)(Value[] args)
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

Value unaryOp(alias chkArg, Result, char[] expr)(Value[] args)
{
    chkArgNum(args, 1);
    auto rhs = chkArg(args, 0);
    return new Result(null, mixin(expr));
}

Value ouro_opPos(Value[] args)
{
    chkArgNum(args, 0);
    chkArgNumber(args, 0);
    return args[0];
}

alias unaryOp!(chkArgNumber, Sit.NumberValue, "-rhs") ouro_opNeg;
alias unaryOp!(chkArgLogical, Sit.LogicalValue, "! rhs") ouro_opNot;

Value ouro_module(Value[] args)
{
    assert( false, "ouro.module nyi" );
}

Value ouro_range(Value[] args)
{
    chkArgNum(args, 4);
    auto li = chkArgLogical(args, 0);
    auto ri = chkArgLogical(args, 1);
    return new Sit.RangeValue(null, li, ri, args[2], args[3]);
}

Value ouro_qqsub(Value[] args) // ast, subs...
{
    chkArgNum(args, 2);
    auto qq = chkArgAst(args, 0);
    auto subs = chkArgList(args, 1).elemValues;
    auto subExprs = new Ast.Expr[subs.length];
    foreach( i,ref subExpr ; subExprs )
        subExpr = chkArgAst(subs, i);

    scope qqsv = new QQSub.QQSubVisitor;
    return new Sit.AstQuoteValue(null, qqsv.visitExpr(qq, subExprs));
}

Value ouro_let(Value[] args)
{
    assert( false, "ouro.let nyi" );
}

Value ouro_import(Value[] args)
{
    assert( false, "ouro.import nyi" );
}

Value ouro_branch(Value[] args)
{
    chkArgNum(args, 3);
    auto cond = chkArgLogical(args, 0);
    auto b0 = chkArgFn(args, 1);
    auto b1 = chkArgFn(args, 2);

    auto b = cond ? b0 : b1;
    return invokeFn(b, null);
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

    builtins["ouro.module"] = new Sit.FunctionValue("ouro.module", [Sit.Argument("path")], &ouro_module);
    builtins["ouro.range"] = new Sit.FunctionValue("ouro.range", [Sit.Argument("li"), Sit.Argument("ui"), Sit.Argument("lv"), Sit.Argument("uv")], &ouro_range);
    builtins["ouro.qqsub"] = new Sit.FunctionValue("ouro.qqsub", [Sit.Argument("ast"), Sit.Argument("subs", true)], &ouro_qqsub);
    builtins["ouro.let"] = new Sit.FunctionValue("ouro.let", [Sit.Argument("bindings", true), Sit.Argument("expr")], &ouro_let);
    builtins["ouro.import"] = new Sit.FunctionValue("ouro.import", [Sit.Argument("scope"), Sit.Argument("symbols"), Sit.Argument("expr")], &ouro_import);
    builtins["ouro.branch"] = new Sit.FunctionValue("ouro.branch", [Sit.Argument("cond"), Sit.Argument("b0"), Sit.Argument("b1")], &ouro_branch);
}
