/**
    Semantic Information Tree nodes.

    Yes, that's what "sit" stands for.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sit.Nodes;

import ouro.Location;
import ouro.util.Repr : reprIdent;

import Ast = ouro.ast.Nodes;

const Nodes =
[
    "Module"[],
    "CallExpr",
    "ArgumentValue",
    "EnclosedValue",
    "DeferredValue",
    "QuantumValue",
    "RuntimeValue",
    "AstQuoteValue",
    "ClosureValue",
    "FunctionValue",
    "ListExpr",
    "ListValue",
    "LogicalValue",
    "MapExpr",
    "MapValue",
    "ModuleValue",
    "NilValue",
    "StringValue",
    "SymbolValue",
    "NumberValue",
    "RangeValue",
    "HostObjectValue",
];

class Scope
{
    Value[char[]] entries;
    Scope parent;

    /**
        This should be set to true for any scope which is enclosed.  That is,
        any scope where values from parent scopes will not be available unless
        specifically stored and provided.

        This is used to denote points in the scope chain outside which
        closures must be used.
     */
    bool enclosed;

    this(Scope parent, bool enclosed)
    {
        this.parent = parent;
        this.enclosed = enclosed;
    }

    void bind(char[] ident, Value value)
    {
        entries[ident] = value;
    }

    void bindArg(Ast.Node node, char[] ident)
    {
        entries[ident] = new ArgumentValue(node, this, ident);
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
        {
            auto v = parent.lookup(astNode, ident, allowFallbacks);

            /*
                The purpose of this bit is to wrap dynamic values which are
                being pulled from outside an enclosure in a closure.
             */

            if( this.enclosed )
            if( auto dv = cast(DynamicValue) v )
                v = dv.enclose;

            return v;
        }

        return null;
    }
}

class PartialScope : Scope
{
    bool complete = false;

    Value[char[]] quantumCache;

    this(Scope parent, bool enclosed)
    {
        super(parent, enclosed);
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
    char[] path;
    Stmt[] stmts;
    Scope scop;
    Scope exportScop;
    bool complete = false;
    size_t numUniques = 0;

    this(Ast.Node astNode, char[] path, Scope scop, Scope exportScop)
    in
    {
        assert( scop !is null );
        assert( exportScop !is null );
    }
    body
    {
        super(astNode);
        this.path = path;
        this.scop = scop;
        this.exportScop = exportScop;
    }

    size_t nextUniqueId()
    {
        assert( numUniques < numUniques.max );
        return (numUniques++);
    }
}

struct Stmt
{
    Ast.Node astNode;
    Expr expr;
    Value value;
    Object ex;
    bool xport;
    bool bind;
    char[] bindIdent;
    bool mergeAll;
    char[][] mergeList;

    static Stmt opCall(Ast.Node astNode)
    {
        Stmt r;
        r.astNode = astNode;
        return r;
    }

    static Stmt opCall(Ast.Node astNode, Expr expr)
    {
        auto r = Stmt(astNode);
        r.expr = expr;
        return r;
    }

    static Stmt opCall(Ast.Node astNode, Expr expr, char[] bindIdent)
    {
        auto r = Stmt(astNode, expr);
        r.bind = true;
        r.bindIdent = bindIdent;
        return r;
    }

    static Stmt opCall(Ast.Node astNode, Expr expr,
            bool mergeAll, char[][] mergeList)
    {
        auto r = Stmt(astNode, expr);
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

enum Order
{
    No = 0b1000,
    Eq = 0b0001,
    Lt = 0b0010,
    Gt = 0b0100,

    Ne = Lt | Gt,
    Le = Lt | Eq,
    Ge = Gt | Eq,
}

interface Formatter
{
    alias void function(void*, void delegate(char[]), size_t,
            char, char[], char[], char[][]) FormatFn;

    void format(void delegate(char[]) emit,
                size_t width,
                char alignment,
                char[] padding,
                char[] precision,
                char[][] options);
}

abstract class Value : Expr
{
    this(Ast.Node astNode)
    {
        super(astNode);
    }

    Order order(Value rhs)
    {
        return (this is rhs) ? Order.Eq : Order.Ne;
    }
}

class UnfixedValue : Value
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

    override Order order(Value rhs)
    {
        assert(false, "cannot compare unfixed values");
    }
}

/**
    This interface is implemented by Value classes which do not have a single,
    fixed value.  For example, the arguments to a function are dynamic.

    Dynamic values must be stored in closures if accessed from inside an
    enclosed scope.
 */
interface DynamicValue
{
    Value enclose();

    template Impl()
    {
        override EnclosedValue enclose()
        {
            return new EnclosedValue(this.astNode, this);
        }
    }
}

class ArgumentValue : UnfixedValue, DynamicValue
{
    mixin DynamicValue.Impl!();

    this(Ast.Node astNode, Scope scop, char[] ident)
    {
        super(astNode, scop, ident);
    }
}

class EnclosedValue : Value
{
    UnfixedValue value;

    this(Ast.Node astNode, UnfixedValue value)
    {
        super(astNode);
        this.value = value;
    }
}

interface Resolvable
{
    Value resolve();
}

class DeferredValue : UnfixedValue, Resolvable
{
    this(Ast.Node astNode, Scope scop, char[] ident)
    {
        super(astNode, scop, ident);
    }

    override Value resolve()
    {
        return scop.lookup(astNode, ident);
    }
}

class QuantumValue : UnfixedValue, Resolvable
{
    this(Ast.Node astNode, Scope scop, char[] ident)
    {
        super(astNode, scop, ident);
    }

    override Value resolve()
    {
        return scop.lookup(astNode, ident);
    }
}

class RuntimeValue : Value, Resolvable
{
    Expr expr;

    protected
    {
        bool evaluating = false;
        Value value;
    }

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

    Value fixValue(Value delegate() fixDg)
    {
        assert( this.value is null );
        assert( ! this.evaluating, "self-referential runtime value" );

        this.evaluating = true;
        scope(exit) this.evaluating = false;

        this.value = fixDg();
        return this.value;
    }

    override Value resolve()
    {
        if( value !is null )
            return value;
        else
        {
            if( this.evaluating )
                assert( false, "self-referential runtime value" );
            return this;
        }
    }
}

class AstQuoteValue : Value, Formatter
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

    override Order order(Value rhsValue)
    {
        auto rhs = cast(AstQuoteValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        return (this.ast == rhs.ast) ? Order.Eq : Order.Ne;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

abstract class CallableValue : Value
{
    this(Ast.Node astNode)
    {
        super(astNode);
    }

    abstract FunctionValue trueFn();
}

class ClosureValue : CallableValue
{
    FunctionValue fn;
    Value[] values;

    this(Ast.Node astNode, FunctionValue fn, Value[] values)
    in
    {
        assert( fn !is null );
    }
    body
    {
        super(astNode);
        this.fn = fn;
        this.values = values;
    }

    override FunctionValue trueFn()
    {
        return fn;
    }
}

/**
    The EvalContext of a function determines when it should be
    evaluated.  This is necessary for functions which depend
    specifically on either the compile-time or run-time environments.

    For example, consider opening a file.  Generally, files should be
    loaded only from the runtime environment.  On the other hand,
    loading files as compile time can be useful.  As such, there is a
    need to distinguish between the two.
 */
enum EvalContext
{
    All     = 0b11,
    None    = 0b00,
    Compile = 0b01,
    Runtime = 0b10,
}

class FunctionValue : CallableValue, Formatter
{
    struct Host
    {
        alias Value function(EvalContext, Value[]) Fn;
        alias Value delegate(EvalContext, Value[]) Dg;

        Fn fn;
        Dg dg;
        EvalContext evalCtx;

        bool evalCtxCompatible(EvalContext evalCtx)
        {
            return (this.evalCtx & evalCtx) != 0;
        }
    }

    Module srcModule;
    char[] srcIdent;

    char[] name;
    Argument[] args;
    Scope scop;
    EnclosedValue[] enclosedValues;

    // Implementations
    Expr expr;
    Host host;

    this(Ast.Node astNode, char[] name, Argument[] args,
            EnclosedValue[] enclosedValues, Scope scop, Expr expr)
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
        this.enclosedValues = enclosedValues;
        this.scop = scop;
        this.expr = expr;
    }

    this(char[] name, Argument[] args,
            Host.Fn fn,
            EvalContext evalCtx = EvalContext.init)
    in
    {
        assert( name != "" );
        assert( fn !is null );
    }
    body
    {
        super(null);
        this.name = name;
        this.args = args;
        this.host.fn = fn;
        this.host.evalCtx = evalCtx;
    }

    this(char[] name, Argument[] args,
            Host.Dg dg,
            EvalContext evalCtx = EvalContext.init)
    in
    {
        assert( name != "" );
        assert( dg !is null );
    }
    body
    {
        super(null);
        this.name = name;
        this.args = args;
        this.host.dg = dg;
        this.host.evalCtx = evalCtx;
    }

    override FunctionValue trueFn()
    {
        return this;
    }

    static FunctionValue compose(CallableValue lhs, CallableValue rhs)
    {
        // (f (.) g)(...) = g(f(...))
        auto scop = new Scope(null, true);
        auto args = lhs.trueFn.args;
        auto callArgs = new CallArg[args.length];

        foreach( i,arg ; args )
        {
            scop.bindArg(null, arg.ident);
            callArgs[i] = CallArg(scop.lookup(null, arg.ident), false);
        }

        auto fc = new FunctionValue(null,
            "(" ~ reprIdent(lhs.trueFn.name) ~ " (.) "
                ~ reprIdent(rhs.trueFn.name) ~ ")",
            args, null, scop,
            new CallExpr(null, rhs, [
                CallArg(new CallExpr(null, lhs, callArgs), false)
            ])
        );

        return fc;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

struct Argument
{
    Location loc;
    char[] ident;
    bool isVararg;

    static Argument opCall(char[] ident, bool isVararg = false)
    {
        return Argument(Location.init, ident, isVararg);
    }

    static Argument opCall(Location loc, char[] ident, bool isVararg = false)
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

class ListExpr : Expr
{
    Expr[] elemExprs;

    this(Ast.Node astNode, Expr[] elemExprs)
    {
        super(astNode);
        this.elemExprs = elemExprs;
    }
}

class ListValue : Value, Formatter
{
    Value[] elemValues;

    this(Ast.Node astNode, Value[] elemValues)
    {
        super(astNode);
        this.elemValues = elemValues;
    }

    static ListValue cons(Value lhs, ListValue rhs)
    {
        return new ListValue(null, (&lhs)[0..1] ~ rhs.elemValues);
    }

    static ListValue join(ListValue lhs, ListValue rhs)
    {
        return new ListValue(null, lhs.elemValues ~ rhs.elemValues);
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(ListValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        if( this.elemValues.length != rhs.elemValues.length )
            return Order.Ne;

        foreach( i,lhsEl ; this.elemValues )
        {
            auto rhsEl = rhs.elemValues[i];
            if( lhsEl.order(rhsEl) != Order.Eq )
                return Order.Ne;
        }

        return Order.Eq;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class LogicalValue : Value, Formatter
{
    bool value;

    this(Ast.Node astNode, bool value)
    {
        super(astNode);
        this.value = value;
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(LogicalValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        return (this.value == rhs.value) ? Order.Eq : Order.Ne;
    }

    static
    {
        LogicalValue instance(bool value)
        {
            auto idx = (cast(size_t) value) & 1;
            if( instance_[idx] is null )
                instance_[idx] = new LogicalValue(null, value);
            return instance_[idx];
        }

        protected LogicalValue[2] instance_;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class MapExpr : Expr
{
    ExprKVP[] kvps;

    this(Ast.Node astNode, ExprKVP[] kvps)
    {
        super(astNode);
        this.kvps = kvps;
    }
}

class MapValue : Value, Formatter
{
    ValueKVP[] kvps;

    this(Ast.Node astNode, ValueKVP[] kvps)
    {
        super(astNode);
        this.kvps = kvps;
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(MapValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        if( this.kvps.length != rhs.kvps.length )
            return Order.Ne;

        foreach( kvpL ; this.kvps )
        {
            bool found = false;

            foreach( kvpR ; rhs.kvps )
            {
                if( kvpL.key.order(kvpR.key) != Order.Eq )
                    continue;

                if( kvpL.value.order(kvpR.value) != Order.Eq )
                    return Order.Ne;

                found = true;
                break;
            }

            if( !found )
                return Order.Ne;
        }

        return Order.Eq;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

struct ExprKVP
{
    Location loc;
    Expr key, value;

    static ExprKVP opCall(Location loc, Expr key, Expr value)
    in
    {
        assert( key !is null );
        assert( value !is null );
    }
    body
    {
        ExprKVP r;
        r.loc = loc;
        r.key = key;
        r.value = value;
        return r;
    }
}

struct ValueKVP
{
    Location loc;
    Value key, value;

    static ValueKVP opCall(Location loc, Value key, Value value)
    in
    {
        assert( key !is null );
        assert( value !is null );
    }
    body
    {
        ValueKVP r;
        r.loc = loc;
        r.key = key;
        r.value = value;
        return r;
    }
}

class ModuleValue : Value, Formatter
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

    override Order order(Value rhsValue)
    {
        auto rhs = cast(ModuleValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        return (this.modul is rhs.modul) ? Order.Eq : Order.Ne;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class NilValue : Value
{
    this(Ast.Node astNode)
    {
        super(astNode);
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(NilValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        return Order.Eq;
    }

    static NilValue instance()
    {
        if( instance_ is null )
            instance_ = new NilValue(null);
        return instance_;
    }

    protected static NilValue instance_;

    char[] toString()
    {
        return "nil";
    }
}

class StringValue : Value, Formatter
{
    char[] value;

    this(Ast.Node astNode, char[] value)
    {
        super(astNode);
        this.value = value;
    }

    static StringValue join(StringValue lhs, StringValue rhs)
    {
        return new StringValue(null, lhs.value ~ rhs.value);
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(StringValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        if( this.value is rhs.value )
            return Order.Eq;

        // TODO: use proper Unicode Canonical equivalence
        return (this.value == rhs.value) ? Order.Eq : Order.Ne;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class SymbolValue : Value, Formatter
{
    char[] value;

    this(Ast.Node astNode, char[] value)
    {
        super(astNode);
        this.value = value;
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(SymbolValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        if( this.value is rhs.value )
            return Order.Eq;

        return Order.Ne;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class NumberValue : Value, Formatter
{
    real value;

    this(Ast.Node astNode, real value)
    {
        super(astNode);
        this.value = value;
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(NumberValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        // TODO: is there a faster way to do this?
        if( this.value == rhs.value )
            return Order.Eq;
        else if( this.value < rhs.value )
            return Order.Lt;
        else if( this.value > rhs.value )
            return Order.Gt;
        else
            return Order.No;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class RangeValue : Value, Formatter
{
    bool incLower, incUpper;
    Value lowerValue, upperValue;

    this(Ast.Node astNode, bool incLower, bool incUpper,
            Value lowerValue, Value upperValue)
    in
    {
        assert( lowerValue !is null );
        assert( upperValue !is null );
    }
    body
    {
        super(astNode);
        this.incLower = incLower;
        this.incUpper = incUpper;
        this.lowerValue = lowerValue;
        this.upperValue = upperValue;
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(RangeValue) rhsValue;
        if( rhs is null ) return Order.Ne;

        return ((this.incLower == rhs.incLower)
             && (this.incUpper == rhs.incUpper)
             && (this.lowerValue.order(rhs.lowerValue) == Order.Eq)
             && (this.upperValue.order(rhs.upperValue) == Order.Eq))
            ? Order.Eq
            : Order.Ne;
    }

    static Formatter.FormatFn formatFn;

    override void format
    (
        void delegate(char[]) emit,
        size_t width,
        char alignment,
        char[] padding,
        char[] precision,
        char[][] options
    )
    {
        assert( formatFn !is null );
        formatFn(cast(void*) this, emit, width,
                alignment, padding, precision, options);
    }
}

class HostObjectValue : Value
{
    Object obj;

    this(Object obj)
    in
    {
        assert( obj !is null );
    }
    body
    {
        super(null);
        this.obj = obj;
    }

    override Order order(Value rhsValue)
    {
        auto rhs = cast(typeof(this)) rhsValue;
        if( rhs is null ) return Order.Ne;

        return (this.obj is rhs.obj) ? Order.Eq : Order.Ne;
    }
}

