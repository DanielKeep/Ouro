/**
    AST nodes.

    Author: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.ast.Nodes;

import ouro.util.EnumCtfe : genEnum_ctfe;
import ouro.Location;

const char[][] Nodes =
[
    "Module"[],
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
    "SymbolExpr",
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
    "BuiltinExpr",
];

abstract class Node
{
    Location loc;

    this(Location loc)
    {
        this.loc = loc;
    }

    override equals_t opEquals(Object rhs)
    {
        return this is rhs;
    }
}

class Module : Node
{
    Statement[] stmts;

    this(Location loc, Statement[] stmts)
    in
    {
        if( stmts != null )
            foreach( stmt ; stmts )
                assert( stmt !is null );
    }
    body
    {
        super(loc);
        this.stmts = stmts;
    }
}

abstract class Statement : Node
{
    this(Location loc)
    {
        super(loc);
    }
}

class ImportStmt : Statement
{
    char[] modulePath;
    char[] ident;
    bool all;
    char[][] symbols;
    bool xport;

    this(Location loc, bool xport, char[] modulePath, char[] ident,
            bool all, char[][] symbols)
    in
    {
        assert( modulePath != "" );
        if( all )
            assert( symbols.length == 0 );
        if( symbols != null )
            foreach( sym ; symbols )
                assert( sym != "" );
    }
    body
    {
        super(loc);
        this.modulePath = modulePath;
        this.ident = ident;
        this.all = all;
        this.symbols = symbols;
        this.xport = xport;
    }
}

class LetStmt : Statement
{
    char[] ident;
    bool xport;
    Expr expr;

    this(Location loc, char[] ident, bool xport, Expr expr)
    in
    {
        assert( ident != "" );
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.ident = ident;
        this.xport = xport;
        this.expr = expr;
    }
}

class LetExprStmt : LetStmt
{
    this(Location loc, char[] ident, bool xport, Expr expr)
    {
        super(loc, ident, xport, expr);
    }
}

class LetFuncStmt : LetStmt
{
    bool isMacro;
    Argument[] args;

    this(Location loc, char[] ident, bool xport, bool isMacro, Argument[] args,
            Expr expr)
    {
        super(loc, ident, xport, expr);
        this.isMacro = isMacro;
        this.args = args;
    }
}

struct Argument
{
    Location loc;
    char[] ident;
    bool isVararg;
    Expr defaultExpr;

    static Argument opCall(Location loc, char[] ident, bool isVararg,
            Expr defaultExpr = null)
    in
    {
        assert( ident != "" );
        if( isVararg ) assert( defaultExpr is null );
    }
    body
    {
        Argument r;
        r.loc = loc;
        r.ident = ident;
        r.isVararg = isVararg;
        r.defaultExpr = defaultExpr;
        return r;
    }

    equals_t opEquals(ref Argument rhs)
    {
        return this.ident == rhs.ident
            && this.isVararg == rhs.isVararg
            && ((this.defaultExpr is null && rhs.defaultExpr is null)
                    || this.defaultExpr == rhs.defaultExpr);
    }
}

class ExprStmt : Statement
{
    Expr expr;

    this(Location loc, Expr expr)
    in
    {
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.expr = expr;
    }
}

abstract class Expr : Node
{
    this(Location loc)
    {
        super(loc);
    }

    override equals_t opEquals(Object rhsObj)
    {
        auto rhs = cast(Node) rhsObj;
        if( rhs is null ) return cast(equals_t) false;

        while(true)
        {
            if( auto rhsRw = cast(RewrittenExpr) rhs )
                rhs = rhsRw.original;
            else
                break;
        }

        auto rhsExpr = cast(Expr) rhs;
        if( rhsExpr is null )
            return cast(equals_t) false;

        return exprEquals(rhsExpr);
    }

    abstract equals_t exprEquals(Expr rhs);
}

class RewrittenExpr : Expr
{
    Node original;
    Expr rewrite;

    this(Location loc, Node original, Expr rewrite)
    in
    {
        assert( original !is null );
        assert( rewrite !is null );
    }
    body
    {
        super(loc);
        this.original = original;
        this.rewrite = rewrite;
    }

    override equals_t exprEquals(Expr rhs)
    {
        auto lhs = cast(Expr) original;
        if( lhs is null ) return cast(equals_t) false;

        return lhs == rhs;
    }
}

class BinaryExpr : Expr
{
    mixin(genEnum_ctfe
    (
        "Op", ["Eq"[], "Ne", "Lt", "LtEq", "Gt", "GtEq",
               "Add", "Sub", "Mul", "Div", "IntDiv", "Mod", "Rem",
               "Exp", "And", "Or", "Comp", "Cons", "Join"],
        "OpStrings", "opToString", "opFromString"
    ));

    Op op;
    Expr lhs, rhs;

    this(Location loc, Op op, Expr lhs, Expr rhs)
    in
    {
        assert( lhs !is null );
        assert( rhs !is null );
    }
    body
    {
        super(loc);
        this.op = op;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    char[] builtin()
    {
        return "ouro.op" ~ opToString(op);
    }

    static char[] opRepr(Op op)
    {
        switch( op )
        {
            case Op.Eq:     return "=";
            case Op.Ne:     return "!=";
            case Op.Lt:     return "<";
            case Op.LtEq:   return "<=";
            case Op.Gt:     return ">";
            case Op.GtEq:   return ">=";
            case Op.Add:    return "+";
            case Op.Sub:    return "-";
            case Op.Mul:    return "*";
            case Op.Div:    return "/";
            case Op.IntDiv: return "//";
            case Op.Mod:    return "mod";
            case Op.Rem:    return "rem";
            case Op.Exp:    return "**";
            case Op.And:    return "and";
            case Op.Or:     return "or";
            case Op.Comp:   return "(.)";
            case Op.Cons:   return "::";
            case Op.Join:   return "++";

            default:        assert(false);
        }
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(BinaryExpr) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.op == rhs.op
            && this.lhs == rhs.lhs
            && this.rhs == rhs.rhs;
    }
}

class TernaryExpr : Expr
{
    mixin(genEnum_ctfe
    (
        "Op", ["LtLt"[], "LeLt", "LtLe", "LeLe",
               "GtGt", "GeGt", "GtGe", "GeGe"],
        "OpStrings", "opToString", "opFromString"
    ));

    Op op;
    Expr lhs, mid, rhs;

    this(Location loc, Op op, Expr lhs, Expr mid, Expr rhs)
    in
    {
        assert( lhs !is null );
        assert( mid !is null );
        assert( rhs !is null );
    }
    body
    {
        super(loc);
        this.op = op;
        this.lhs = lhs;
        this.mid = mid;
        this.rhs = rhs;
    }

    char[] builtin()
    {
        return "ouro.op" ~ opToString(op);
    }

    private alias BinaryExpr.Op BinOp;

    static Op opFromBinary(BinOp lho, BinOp rho)
    {
        switch( lho )
        {
            case BinOp.Lt:
                switch( rho )
                {
                    case BinOp.Lt:      return Op.LtLt;
                    case BinOp.LtEq:    return Op.LtLe;
                    default:            assert(false);
                }

            case BinOp.LtEq:
                switch( rho )
                {
                    case BinOp.Lt:      return Op.LeLt;
                    case BinOp.LtEq:    return Op.LeLe;
                    default:            assert(false);
                }

            case BinOp.Gt:
                switch( rho )
                {
                    case BinOp.Gt:      return Op.GtGt;
                    case BinOp.GtEq:    return Op.GtGe;
                    default:            assert(false);
                }

            case BinOp.GtEq:
                switch( rho )
                {
                    case BinOp.Gt:      return Op.GeGt;
                    case BinOp.GtEq:    return Op.GeGe;
                    default:            assert(false);
                }

            default:
                assert(false);
        }
    }

    static void opRepr(Op op, out char[] lho, out char[] rho)
    {
        switch( op )
        {
            case Op.LtLt:   lho = "<";  rho = "<";  break;
            case Op.LeLt:   lho = "<="; rho = "<";  break;
            case Op.LtLe:   lho = "<";  rho = "<="; break;
            case Op.LeLe:   lho = "<="; rho = "<="; break;
            case Op.GtGt:   lho = ">";  rho = ">";  break;
            case Op.GeGt:   lho = ">="; rho = ">";  break;
            case Op.GtGe:   lho = ">";  rho = ">="; break;
            case Op.GeGe:   lho = ">="; rho = ">="; break;

            default:        assert(false);
        }
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.op == rhs.op
            && this.lhs == rhs.lhs
            && this.mid == rhs.mid
            && this.rhs == rhs.rhs;
    }
}

class InfixFuncExpr : Expr
{
    Expr funcExpr;
    Expr lhs, rhs;

    this(Location loc, Expr funcExpr, Expr lhs, Expr rhs)
    in
    {
        assert( funcExpr !is null );
        assert( lhs !is null );
        assert( rhs !is null );
    }
    body
    {
        super(loc);
        this.funcExpr = funcExpr;
        this.lhs = lhs;
        this.rhs = rhs;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.funcExpr == rhs.funcExpr
            && this.lhs == rhs.lhs
            && this.rhs == rhs.rhs;
    }
}

class PrefixExpr : Expr
{
    mixin(genEnum_ctfe
    (
        "Op", ["Pos"[], "Neg", "Not"],
        "OpStrings", "opToString", "opFromString"
    ));

    Op op;
    Expr subExpr;

    this(Location loc, Op op, Expr subExpr)
    in
    {
        assert( subExpr !is null );
    }
    body
    {
        super(loc);
        this.op = op;
        this.subExpr = subExpr;
    }

    char[] builtin()
    {
        return "ouro.op" ~ opToString(op);
    }

    static char[] opRepr(Op op)
    {
        switch( op )
        {
            case Op.Pos:    return "+";
            case Op.Neg:    return "-";
            case Op.Not:    return "not";

            default:        assert(false);
        }
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.op == rhs.op
            && this.subExpr == rhs.subExpr;
    }
}

class PostfixFuncExpr : Expr
{
    Expr funcExpr;
    Expr subExpr;

    this(Location loc, Expr funcExpr, Expr subExpr)
    in
    {
        assert( funcExpr !is null );
        assert( subExpr !is null );
    }
    body
    {
        super(loc);
        this.funcExpr = funcExpr;
        this.subExpr = subExpr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.funcExpr == rhs.funcExpr
            && this.subExpr == rhs.subExpr;
    }
}

class NumberExpr : Expr
{
    real value;

    this(Location loc, real value)
    {
        super(loc);
        this.value = value;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.value == rhs.value;
    }
}

class StringExpr : Expr
{
    char[] value;

    this(Location loc, char[] value)
    {
        super(loc);
        this.value = value;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.value == rhs.value;
    }
}

class SymbolExpr : Expr
{
    char[] value;

    this(Location loc, char[] value)
    {
        super(loc);
        this.value = value;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.value == rhs.value;
    }
}

class LogicalExpr : Expr
{
    bool value;

    this(Location loc, bool value)
    {
        super(loc);
        this.value = value;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.value == rhs.value;
    }
}

class NilExpr : Expr
{
    this(Location loc)
    {
        super(loc);
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return cast(equals_t) true;
    }
}

class ListExpr : Expr
{
    Expr[] elemExprs;

    this(Location loc, Expr[] elemExprs)
    in
    {
        if( elemExprs != null )
            foreach( expr ; elemExprs )
                assert( expr !is null );
    }
    body
    {
        super(loc);
        this.elemExprs = elemExprs;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        if( this.elemExprs.length != rhs.elemExprs.length )
            return cast(equals_t) false;

        foreach( i,elemExpr ; this.elemExprs )
            if( elemExpr != rhs.elemExprs[i] )
                return cast(equals_t) false;

        return cast(equals_t) true;
    }
}

class MapExpr : Expr
{
    KeyValuePair[] keyValuePairs;

    this(Location loc, KeyValuePair[] keyValuePairs)
    {
        super(loc);
        this.keyValuePairs = keyValuePairs;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        if( this.keyValuePairs.length != rhs.keyValuePairs.length )
            return cast(equals_t) false;

        foreach( i,lkvp ; this.keyValuePairs )
        {
            auto rkvp = &rhs.keyValuePairs[i];
            if( lkvp.key != rkvp.key ) return cast(equals_t) false;
            if( lkvp.value != rkvp.value ) return cast(equals_t) false;
        }

        return cast(equals_t) true;
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

class LambdaExpr : Expr
{
    bool isMacro;
    Argument[] args;
    Expr expr;

    this(Location loc, bool isMacro, Argument[] args, Expr expr)
    in
    {
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.isMacro = isMacro;
        this.args = args;
        this.expr = expr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        foreach( i,arg ; this.args )
            if( arg != rhs.args[i] )
                return cast(equals_t) false;

        return this.isMacro == rhs.isMacro
            && this.expr == rhs.expr;
    }
}

class ExplodeExpr : Expr
{
    Expr subExpr;

    this(Location loc, Expr subExpr)
    in
    {
        assert( subExpr !is null );
    }
    body
    {
        super(loc);
        this.subExpr = subExpr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.subExpr == rhs.subExpr;
    }
}

class CallExpr : Expr
{
    bool isMacro;
    Expr funcExpr;
    Expr[] argExprs;
    Expr[char[]] namedArgExprs;

    this(Location loc, bool isMacro, Expr funcExpr,
            Expr[] argExprs, Expr[char[]] namedArgExprs)
    in
    {
        assert( funcExpr !is null );
        if( argExprs != null )
            foreach( arg ; argExprs )
                assert( arg !is null );
    }
    body
    {
        super(loc);
        this.isMacro = isMacro;
        this.funcExpr = funcExpr;
        this.argExprs = argExprs;
        this.namedArgExprs = namedArgExprs;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        foreach( i,argExpr ; this.argExprs )
            if( argExpr != rhs.argExprs[i] )
                return cast(equals_t) false;

        return this.isMacro == rhs.isMacro
            && this.funcExpr == rhs.funcExpr
            && aaEqual(this.namedArgExprs, rhs.namedArgExprs);
    }

    // HACK
    private equals_t aaEqual(Expr[char[]] a, Expr[char[]] b)
    {
        if( a is b )
            return cast(equals_t) true;

        if( a.length != b.length )
            return cast(equals_t) false;

        foreach( k,va ; a )
            if( va != b[k] )
                return cast(equals_t) false;

        return cast(equals_t) true;
    }
}

class VariableExpr : Expr
{
    char[] ident;

    this(Location loc, char[] ident)
    {
        super(loc);
        this.ident = ident;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.ident == rhs.ident;
    }
}

class RangeExpr : Expr
{
    bool incLower, incUpper;
    Expr lowerExpr, upperExpr;

    this(Location loc, bool incLower, bool incUpper,
            Expr lowerExpr, Expr upperExpr)
    in
    {
        assert( lowerExpr !is null );
        assert( upperExpr !is null );
    }
    body
    {
        super(loc);
        this.incLower = incLower;
        this.incUpper = incUpper;
        this.lowerExpr = lowerExpr;
        this.upperExpr = upperExpr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.incLower == rhs.incLower
            && this.incUpper == rhs.incUpper
            && this.lowerExpr == rhs.lowerExpr
            && this.upperExpr == rhs.upperExpr;
    }
}

class AstQuoteExpr : Expr
{
    Expr expr;

    this(Location loc, Expr expr)
    in
    {
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.expr = expr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.expr == rhs.expr;
    }
}

class AstQuasiQuoteExpr : Expr
{
    Expr expr;

    this(Location loc, Expr expr)
    in
    {
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.expr = expr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.expr == rhs.expr;
    }
}

class AstQQSubExpr : Expr
{
    Expr expr;
    size_t index = ~0;

    this(Location loc, Expr expr)
    in
    {
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.expr = expr;
    }

    this(Location loc, size_t index)
    {
        super(loc);
        this.index = index;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        if( index != ~0 )
            return this.index == rhs.index;

        if( rhs.index != ~0 )
            return cast(equals_t) false;

        return this.expr == rhs.expr;
    }
}

class BuiltinExpr : Expr
{
    char[] ident;

    this(Location loc, char[] ident)
    in
    {
        assert( ident != "" );
    }
    body
    {
        super(loc);
        this.ident = ident;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.ident == rhs.ident;
    }
}

class LetExpr : Expr
{
    Expr[] bindExprs;
    Expr subExpr;

    this(Location loc, Expr[] bindExprs, Expr subExpr)
    in
    {
        assert( subExpr !is null );
    }
    body
    {
        super(loc);
        this.bindExprs = bindExprs;
        this.subExpr = subExpr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        foreach( i,bindExpr ; this.bindExprs )
            if( bindExpr != rhs.bindExprs[i] )
                return cast(equals_t) false;

        return this.subExpr == rhs.subExpr;
    }
}

class ImportExpr : Expr
{
    Expr scopeExpr;
    Expr symbolsExpr;
    Expr subExpr;

    this(Location loc, Expr scopeExpr, Expr symbolsExpr, Expr subExpr)
    in
    {
        assert( scopeExpr !is null );
        assert( symbolsExpr !is null );
        assert( subExpr !is null );
    }
    body
    {
        super(loc);
        this.scopeExpr = scopeExpr;
        this.symbolsExpr = symbolsExpr;
        this.subExpr = subExpr;
    }

    override equals_t exprEquals(Expr rhsExpr)
    {
        auto rhs = cast(typeof(this)) rhsExpr;
        if( rhs is null ) return cast(equals_t) false;

        return this.scopeExpr == rhs.scopeExpr
            && this.symbolsExpr == rhs.symbolsExpr
            && this.subExpr == rhs.subExpr;
    }
}

