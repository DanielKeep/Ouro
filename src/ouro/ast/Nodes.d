/**
    AST nodes.

    Author: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.ast.Nodes;

import ouro.Location;

abstract class Node
{
    Location loc;

    this(Location loc)
    {
        this.loc = loc;
    }
}

class Program : Node
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

    this(Location loc, char[] modulePath, char[] ident,
            bool all, char[][] symbols)
    in
    {
        assert( modulePath != "" );
        assert( ident != "" );
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
    }
}

class LetStmt : Statement
{
    char[] ident;
    Expr expr;

    this(Location loc, char[] ident, Expr expr)
    in
    {
        assert( ident != "" );
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.ident = ident;
        this.expr = expr;
    }
}

class LetExprStmt : LetStmt
{
    this(Location loc, char[] ident, Expr expr)
    {
        super(loc, ident, expr);
    }
}

class LetFuncStmt : LetStmt
{
    bool isMacro;
    Argument[] args;

    this(Location loc, char[] ident, bool isMacro, Argument[] args,
            Expr expr)
    {
        super(loc, ident, expr);
        this.isMacro = isMacro;
        this.args = args;
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
}

/+
class ImportExpr : Expr
{
    char[] modulePath;
    char[][] importList;
    Expr expr;

    this(Location loc, char[] modulePath, char[][] importList,
            Expr expr)
    in
    {
        assert( modulePath != "" );
        assert( expr !is null );
        if( importList != null )
            foreach( sym ; importList )
                assert( sym != "" );
    }
    body
    {
        super(loc);
        this.modulePath = modulePath;
        this.importList = importList;
        this.expr = expr;
    }
}
// +/

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
}

class BinaryExpr : Expr
{
    enum Op
    {
        Eq, Ne,
        Lt, LtEq, Gt, GtEq,
        Add, Sub, Mul, Div, IntDiv, Mod, Rem,
        Exp, And, Or, Comp, Cons, Join,
    }

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

    static char[] opToString(Op op)
    {
        switch( op )
        {
            case Op.Eq:     return "Eq";
            case Op.LtEq:   return "LtEq";
            case Op.Gt:     return "Gt";
            case Op.GtEq:   return "GtEq";
            case Op.Add:    return "Add";
            case Op.Sub:    return "Sub";
            case Op.Mul:    return "Mul";
            case Op.Div:    return "Div";
            case Op.IntDiv: return "IntDiv";
            case Op.Exp:    return "Exp";
            case Op.And:    return "And";
            case Op.Or:     return "Or";
            case Op.Comp:   return "Comp";
            case Op.Cons:   return "Cons";
            case Op.Join:   return "Join";

            default:        assert(false);
        }
    }
}

class InfixFuncExpr : Expr
{
    Expr func;
    Expr lhs, rhs;

    this(Location loc, Expr func, Expr lhs, Expr rhs)
    in
    {
        assert( func !is null );
        assert( lhs !is null );
        assert( rhs !is null );
    }
    body
    {
        super(loc);
        this.func = func;
        this.lhs = lhs;
        this.rhs = rhs;
    }
}

class PrefixExpr : Expr
{
    enum Op
    {
        Pos, Neg, Not
    }

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

    static char[] opToString(Op op)
    {
        switch( op )
        {
            case Op.Pos:    return "Pos";
            case Op.Neg:    return "Neg";
            case Op.Not:    return "Not";

            default:        assert(false);
        }
    }
}

class PostfixFuncExpr : Expr
{
    Expr func;
    Expr subExpr;

    this(Location loc, Expr func, Expr subExpr)
    in
    {
        assert( func !is null );
        assert( subExpr !is null );
    }
    body
    {
        super(loc);
        this.func = func;
        this.subExpr = subExpr;
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
}

class StringExpr : Expr
{
    char[] value;

    this(Location loc, char[] value)
    {
        super(loc);
        this.value = value;
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
}

class NilExpr : Expr
{
    this(Location loc)
    {
        super(loc);
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
}

class MapExpr : Expr
{
    KeyValuePair[] keyValuePairs;

    this(Location loc, KeyValuePair[] keyValuePairs)
    {
        super(loc);
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
}

class CallExpr : Expr
{
    bool isMacro;
    Expr funcExpr;
    Expr[] argExprs;

    this(Location loc, bool isMacro, Expr funcExpr,
            Expr[] argExprs)
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
}

/+
class KeywordExpr : Expr
{
    enum Keyword
    {
        Quote,
        QuasiQuote,
        QqSub,
        Let,
        Import,
    }

    Keyword keyword;

    this(Location loc, Keyword keyword)
    {
        super(loc);
        this.keyword = keyword;
    }

    static char[] keywordToString(Keyword keyword)
    {
        switch( keyword )
        {
            case Keyword.Quote:         return "Quote";
            case Keyword.QuasiQuote:    return "QuasiQuote";
            case Keyword.QqSub:         return "QqSub";
            case Keyword.Let:           return "Let";
            case Keyword.Import:        return "Import";

            default:                    assert(false);
        }
    }
}
+/

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
}

class AstQQSubExpr : Expr
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
}

class EvalSharedExpr : Expr
{
    SharedExpr sharedExpr;
    Expr expr;

    this(Location loc, SharedExpr sharedExpr, Expr expr)
    in
    {
        assert( sharedExpr !is null );
        assert( expr !is null );
    }
    body
    {
        super(loc);
        this.sharedExpr = sharedExpr;
        this.expr = expr;
    }
}

class SharedExpr : Expr
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

