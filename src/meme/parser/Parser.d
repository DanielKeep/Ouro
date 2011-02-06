/**
    Parser.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.parser.Parser;

import ouro.Error : CompilerErrorCode;
import ouro.lexer.Tokens;
import ouro.util.TokenStream;
import ouro.util.Parse : parseReal;

import Ast = ouro.ast.Nodes;

alias CompilerErrorCode CEC;

Ast.Program parseProgram(TokenStream ts)
{
    /*
        <script> = { <statement> };       
    */

    Ast.Statement[] stmts;

    skipEmptyStmts(ts);

    auto loc = ts.peek.loc;

    while( ts.peek.type != TOKnone )
    {
        stmts ~= parseStmt(ts);
        skipEmptyStmts(ts);
    }

    return new Ast.Program(loc, stmts);
}

void skipEmptyStmts(TokenStream ts)
{
    /*
        <term> = <eol> | <eos>;

        <empty statement> = <term>;
    */
    bool isEnd(TOK type)
    {
        return type == TOKeol || type == TOKeos;
    }

    while( isEnd(ts.peek.type) )
        ts.pop;
}

Ast.Statement parseStmt(TokenStream ts)
{
    /*
        <statement> = <empty statement>
                    | <import statement>
                    | <let statement>
                    | <expression statement>
                    ;
    */

    skipEmptyStmts(ts);

    Ast.Statement stmt;

    stmt = tryparseImportStmt(ts);  if( stmt !is null ) goto gotStmt;
    stmt = tryparseLetStmt(ts);     if( stmt !is null ) goto gotStmt;

    // fallback
    stmt = parseExprStmt(ts);

gotStmt:
    return stmt;
}

Ast.ImportStmt tryparseImportStmt(TokenStream ts)
{
    /*
        <import statement> =
            "import", [ <identifier>, "=" ], <string>,
                [ ":", ( <import identifier>, { ",", <import identifier> }
                       | "*"
                       ) ],
                <term>;
    */
    
    if( ts.peek.type != TOKimport ) return null;
    
    auto start = ts.pop.loc;
    char[] ident = null;

    if( ts.peek.type == TOKidentifier )
    {
        ident = ts.pop.value;
        ts.popExpect(TOKeq);
    }

    auto modulePath = ts.popExpect(TOKstring).value;
    bool all = false;
    char[][] symbols;

    if( ts.peek.type == TOKcolon )
    {
        ts.pop;
        if( ts.peek.type == TOKstar )
            all = true;

        else
        {
            symbols ~= ts.popExpect(TOKidentifier).value;

            while( ts.peek.type == TOKcomma )
            {
                ts.pop;
                symbols ~= ts.popExpect(TOKidentifier).value;
            }
        }
    }

    auto end = ts.popExpectAny(TOKeol, TOKeos).loc;

    return new Ast.ImportStmt(start.extendTo(end), modulePath,
            ident, all, symbols);
}

Ast.LetStmt tryparseLetStmt(TokenStream ts)
{
    /*
        <let statement> = (
              "let", <identifier>, "=", <expression>
            | "let", [ "macro" ], <identifier>,
                "(", [ <function argument names> ], ")", "=", <expression>
            ),
            <term>;
    */
    if( ts.peek.type != TOKlet ) return null;

    auto start = ts.pop.loc;
    bool isMacro;

    if( ts.peek.type == TOKmacro )
    {
        isMacro = true;
        ts.pop;
    }

    auto ident = ts.popExpect(TOKidentifier).value;

    if( isMacro || ts.peek.type == TOKlparen )
    {
        Ast.Argument[] args;
        ts.popExpect(TOKlparen);

        if( ts.peek.type != TOKrparen )
            ts.skipEolDo
            ({
                args = parseFuncArgNames(ts, TOKrparen);
            });

        ts.popExpect(TOKrparen);

        ts.popExpect(TOKeq);
        auto expr = parseExpr(ts);
        auto end = ts.popExpectAny(TOKeol, TOKeos).loc;

        return new Ast.LetFuncStmt(start.extendTo(end), ident,
                isMacro, args, expr);
    }
    else
    {
        ts.popExpect(TOKeq);
        auto expr = parseExpr(ts);
        auto end = ts.popExpectAny(TOKeol, TOKeos).loc;

        return new Ast.LetExprStmt(start.extendTo(end), ident, expr);
    }
}

Ast.Argument[] parseFuncArgNames(TokenStream ts, TOK stopType)
{
    /*
        <function argument names> = <argument name>, { ",", <argument name> };
    */
    Ast.Argument[] args;

    args ~= parseArgName(ts);

    while( ts.peek.type != stopType )
    {
        ts.popExpect(TOKcomma);
        args ~= parseArgName(ts);
    }

    return args;
}

Ast.Argument parseArgName(TokenStream ts)
{
    /*
        <argument name> = <identifier>, [ "..." ];
    */
    auto start = ts.peek.loc;
    auto end = start;
    auto ident = ts.popExpect(TOKidentifier).value;
    bool isVararg;

    if( ts.peek.type == TOKperiod3 )
    {
        end = ts.pop.loc;
        isVararg = true;
    }

    return Ast.Argument(start.extendTo(end), ident, isVararg);
}

Ast.ExprStmt parseExprStmt(TokenStream ts)
{
    auto expr = parseExpr(ts);
    auto loc = expr.loc;
    ts.popExpectAny(TOKeol, TOKeos);

    return new Ast.ExprStmt(loc, expr);
}

enum Fixity
{
    Left,
    Right,
}

Fixity fixityOf(Ast.BinaryExpr.Op op)
{
    alias Ast.BinaryExpr.Op Op;
    enum : Fixity { L = Fixity.Left, R = Fixity.Right }

    switch( op )
    {
        case Op.Eq:
        case Op.Ne:
        case Op.Lt:
        case Op.LtEq:
        case Op.Gt:
        case Op.GtEq:
        case Op.Add:
        case Op.Sub:
        case Op.Mul:
        case Op.Div:
        case Op.IntDiv:
        case Op.And:
        case Op.Or:
        case Op.Comp:
        case Op.Join:
            return L;
            
        case Op.Exp:
        case Op.Cons:
            return R;

        default:
            assert(false, "missing binary op fixity");
    }
}

float precOf(Ast.BinaryExpr.Op op)
{
    alias Ast.BinaryExpr.Op Op;

    switch( op )
    {
        case Op.Comp:   return 9.0;
        case Op.Exp:    return 6.7;
        case Op.Mul:    return 6.5;
        case Op.Div:    return 6.5;
        case Op.IntDiv: return 6.5;
        case Op.Mod:    return 6.5;
        case Op.Rem:    return 6.5;
        case Op.Add:    return 6.2;
        case Op.Sub:    return 6.2;
        case Op.Cons:   return 5.6;
        case Op.Join:   return 5.4;
        case Op.Eq:     return 4.0;
        case Op.Ne:     return 4.0;
        case Op.Lt:     return 4.0;
        case Op.LtEq:   return 4.0;
        case Op.Gt:     return 4.0;
        case Op.GtEq:   return 4.0;
        case Op.And:    return 3.9;
        case Op.Or:     return 3.8;

        default:
            assert(false, "missing binary op precedence");
    }
}

struct ExprState
{
    struct Op
    {
        Ast.BinaryExpr.Op op;
        float prec;
    }

    Ast.Expr[] exprs;
    Op[] ops;

    void pushExpr(Ast.Expr expr)
    {
        exprs ~= expr;
    }

    void pushOp(Ast.BinaryExpr.Op binOp)
    {
        Op top;
        top.op = binOp;
        top.prec = precOf(binOp);

        while( ops.length > 0 && ops[$-1].prec > top.prec )
            crushTop;

        ops ~= top;
    }

    Ast.Expr force()
    {
        assert( exprs.length >= 1 );
        assert( ops.length == exprs.length-1 );

        while( ops.length > 0 )
            crushTop;

        assert( exprs.length == 1 );
        return exprs[0];
    }

    void crushTop()
    {
        assert( ops.length >= 1 );
        assert( exprs.length >= 2 );
        assert( ops.length == exprs.length-1 );

        auto prec = ops[$-1].prec;
        auto fix = fixityOf(ops[$-1].op);

        size_t i = ops.length - 1;
        while( i>0 && ops[i-1].prec == prec )
            -- i;

        auto crushOps = ops[i..$];
        auto crushExprs = exprs[$-(crushOps.length+1)..$];

        ops = ops[0..$-crushOps.length];
        exprs = exprs[0..$-crushExprs.length+1];

        if( fix == Fixity.Left )
            exprs[$-1] = crushLtr(crushOps, crushExprs);
        else
            exprs[$-1] = crushRtl(crushOps, crushExprs);
    }

    Ast.Expr crushLtr(Op[] ops, Ast.Expr[] exprs)
    {
        assert( ops.length > 0 );
        assert( exprs.length == ops.length + 1 );

        Ast.Expr lhs = exprs[0];
        exprs = exprs[1..$];

        while( ops.length > 0 )
        {
            lhs = foldBinaryOp(ops[0].op, lhs, exprs[0]);
            ops = ops[1..$];
            exprs = exprs[1..$];
        }

        return lhs;
    }

    Ast.Expr crushRtl(Op[] ops, Ast.Expr[] exprs)
    {
        assert( ops.length > 0 );
        assert( exprs.length == ops.length + 1 );

        Ast.Expr rhs = exprs[$-1];
        exprs = exprs[0..$-1];

        while( ops.length > 0 )
        {
            rhs = foldBinaryOp(ops[$-1].op, exprs[$-1], rhs);
            ops = ops[0..$-1];
            exprs = exprs[0..$-1];
        }

        return rhs;
    }
}

Ast.Expr parseExpr(TokenStream ts)
{
    /*
        <expression> = <expression atom>,
                       { <binary op>, <expression atom> },
                       [ <postfix op> ];
    */

    auto lhs = parseExprAtom(ts);

    if( lhs is null )
        ts.err(CEC.PExExprGotTx, ts.peek.text);

    // Parse the chain of infix operators.
    Ast.BinaryExpr.Op op;
    Ast.Expr expr;
    if( tryparseBinaryOp(ts, op) )
    {
        ExprState st;
        st.pushExpr(lhs);
        st.pushOp(op);

        while( true )
        {
            st.pushExpr(parseExprAtom(ts));

            if( tryparseBinaryOp(ts, op) )
                st.pushOp(op);
            else
                break;
        }

        expr = st.force;
    }
    else
        // No infix chain
        expr = lhs;

    // Handle postfix
    if( auto postfixExpr = tryparsePostfixExpr(ts, expr) )
        expr = postfixExpr;

    return expr;
}

bool tryparsePrefixOp(TokenStream ts, out Ast.PrefixExpr.Op op)
{
    alias Ast.PrefixExpr.Op Op;
    auto t = ts.peek;

    switch( t.type )
    {
        case TOKplus:   op = Op.Pos; ts.pop; return true;
        case TOKhyphen: op = Op.Neg; ts.pop; return true;
        case TOKnot:    op = Op.Not; ts.pop; return true;

        default:        return false;
    }
}

bool tryparseBinaryOp(TokenStream ts, out Ast.BinaryExpr.Op op)
{
    alias Ast.BinaryExpr.Op Op;
    auto t = ts.peek;

    switch( t.type )
    {
        case TOKeq:         op = Op.Eq;     break;
        case TOKhyphen:     op = Op.Sub;    break;
        case TOKnoteq:      op = Op.Ne;     break;
        case TOKslash2:     op = Op.IntDiv; break;
        case TOKslash:      op = Op.Div;    break;
        case TOKstar2:      op = Op.Exp;    break;
        case TOKstar:       op = Op.Mul;    break;
        case TOKlteq:       op = Op.LtEq;   break;
        case TOKltgt:       op = Op.Ne;     break;
        case TOKlt:         op = Op.Lt;     break;
        case TOKgteq:       op = Op.GtEq;   break;
        case TOKgt:         op = Op.Gt;     break;
        case TOKcolon2:     op = Op.Cons;   break;
        case TOKplus2:      op = Op.Join;   break;
        case TOKplus:       op = Op.Add;    break;
        case TOKperiod:     op = Op.Comp;   break;

        case TOKand:        op = Op.And;    break;
        case TOKor:         op = Op.Or;     break;
        case TOKmod:        op = Op.Mod;    break;
        case TOKrem:        op = Op.Rem;    break;

        default:            return false;
    }

    ts.pop;
    return true;
}

Ast.Expr tryparsePostfixExpr(TokenStream ts, Ast.Expr expr)
{
    if( ts.peek.type != TOKlparenperiod )
        return expr;

    auto start = ts.pop.loc;
    Ast.Expr funcExpr;

    if( auto varExpr = tryparseVariableExpr(ts) )
        funcExpr = varExpr;

    else if( auto subExpr = tryparseSubExpr(ts) )
        funcExpr = subExpr;

    else
        return expr;

    auto end = ts.popExpect(TOKperiodrparen).loc;

    return new Ast.PostfixFuncExpr(start.extendTo(end), funcExpr, expr);
}

Ast.Expr tryparseExprAtom(TokenStream ts)
{
    /*
        <expression atom> = [ <prefix op> ],
                                ( <number expression>
                                | <string expression>
                                | <logical expression>
                                | <list expression>
                                | <map expression>
                                | <lambda expression>
                                | <function expression>
                                | <variable expression>
                                | <range expression>
                                | <sub expression>
                                ),
                            [ <explode> ];
    */

    Ast.PrefixExpr.Op prefixOp;
    auto start = ts.peek.loc;
    auto hasPrefix = tryparsePrefixOp(ts, prefixOp);

    auto innerExpr =
    ({
        if( auto e = tryparseNumberExpr(ts) )       return e;
        if( auto e = tryparseStringExpr(ts) )       return e;
        if( auto e = tryparseLogicalExpr(ts) )      return e;
        if( auto e = tryparseListExpr(ts) )         return e;
        if( auto e = tryparseMapExpr(ts) )          return e;
        if( auto e = tryparseLambdaExpr(ts) )       return e;
        if( auto e = tryparseFunctionOrVariableOrSubExpr(ts) )
                                                    return e;
        if( auto e = tryparseRangeExpr(ts) )        return e;
    })();

    Ast.Expr expr;

    if( hasPrefix )
        expr = new Ast.PrefixExpr(start.extendTo(innerExpr.loc),
                prefixOp, innerExpr);
    else
        expr = innerExpr;

    if( ts.peek.type == TOKperiod3 )
    {
        auto end = ts.pop.loc;
        expr = new Ast.ExplodeExpr(start.extendTo(end), expr);
    }

    return expr;
}

Ast.Expr parseExprAtom(TokenStream ts)
{
    auto expr = tryparseExprAtom(ts);
    if( expr is null )
        ts.err(CEC.PExExprGotTx, ts.peek.text);
    return expr;
}

Ast.Expr tryparseNumberExpr(TokenStream ts)
{
    if( ts.peek.type != TOKnumber ) return null;

    auto loc = ts.peek.loc;
    auto value = parseReal(ts.pop.value);
    auto expr = new Ast.NumberExpr(loc, value);

    if( auto rhsExpr = tryparseFunctionOrVariableOrSubExpr(ts) )
        return new Ast.BinaryExpr(
                expr.loc.extendTo(rhsExpr.loc),
                Ast.BinaryExpr.Op.Mul, expr, rhsExpr);

    else
        return expr;
}

Ast.Expr tryparseStringExpr(TokenStream ts)
{
    if( ts.peek.type != TOKstring ) return null;

    auto loc = ts.peek.loc;
    char[] value = ts.pop.value;
    return new Ast.StringExpr(loc, value);
}

Ast.Expr tryparseLogicalExpr(TokenStream ts)
{
    if( ts.peek.type == TOKtrue )
        return new Ast.LogicalExpr(ts.pop.loc, true);

    else if( ts.peek.type == TOKfalse )
        return new Ast.LogicalExpr(ts.pop.loc, false);

    else
        return null;
}

Ast.Expr tryparseListExpr(TokenStream ts)
{
    if( ts.peek.type != TOKlbracket ) return null;

    auto start = ts.popExpect(TOKlbracket).loc;
    Ast.Expr[] elements;

    if( ts.peek.type != TOKrbracket )
        ts.skipEolDo
        ({
            elements = parseCommaExprs(ts, TOKrbracket);
        });
    
    auto end = ts.popExpect(TOKrbracket).loc;

    return new Ast.ListExpr(start.extendTo(end), elements);
}

Ast.Expr[] parseCommaExprs(TokenStream ts, TOK stopType)
{
    Ast.Expr[] elements;

    elements ~= parseExpr(ts);
    while( ts.peek.type != stopType )
    {
        ts.popExpect(TOKcomma);
        elements ~= parseExpr(ts);
    }

    return elements;
}

Ast.Expr tryparseMapExpr(TokenStream ts)
{
    if( ts.peek.type != TOKlbracketcolon ) return null;

    auto start = ts.pop.loc;
    Ast.KeyValuePair[] pairs;

    pairs ~= parseKeyValuePair(ts);
    while( ts.peek.type != TOKcolonrbracket )
    {
        ts.popExpect(TOKcomma);
        pairs ~= parseKeyValuePair(ts);
    }

    auto end = ts.popExpect(TOKcolonrbracket).loc;
    return new Ast.MapExpr(start.extendTo(end), pairs);
}

Ast.KeyValuePair parseKeyValuePair(TokenStream ts)
{
    auto start = ts.peek.loc;

    auto keyExpr = parseExpr(ts);
    ts.popExpect(TOKcolon);
    auto valueExpr = parseExpr(ts);

    return Ast.KeyValuePair(start.extendTo(valueExpr.loc),
            keyExpr, valueExpr);
}

Ast.Expr tryparseLambdaExpr(TokenStream ts)
{
    /*
        <lambda expression> = "\\", [ "macro" ],
            [ <function argument names> ],
            ".", <expression>;
    */
    if( ts.peek.type != TOKbslash ) return null;

    auto start = ts.pop.loc;
    Ast.Argument[] args;

    bool isMacro = false;
    if( ts.peek.type == TOKmacro )
    {
        ts.pop;
        isMacro = true;
    }

    if( ts.peek.type != TOKperiod )
        args = parseFuncArgNames(ts, TOKperiod);

    ts.popExpect(TOKperiod);

    auto expr = parseExpr(ts);

    return new Ast.LambdaExpr(start.extendTo(expr.loc),
            isMacro, args, expr);
}

Ast.Expr tryparseFunctionOrVariableOrSubExpr(TokenStream ts)
{
    /*
        <function expression> = [ "macro" ], <function prefix>,
                                "(", [ <expression>, { ",", <expression> }], ")";

        <function prefix> = <identifier>
                          | <function like keyword>
                          | <sub expression>
                          | <function expression>
                          ;

        <function like keyword> = "#~'"
                                | "#~\""
                                | "#~$"
                                | "let"
                                | "import"
                                ;
    */
    Ast.Expr baseExpr;

    auto start = ts.peek.loc;
    bool isMacro = false;
    TOK keyword;
    bool isKeyword = false;

    if( ts.peek.type == TOKmacro )
    {
        isMacro = true;
        ts.pop;
    }

    if( auto varExpr = tryparseVariableExpr(ts) )
        baseExpr = varExpr;

    else if( tryparseFunctionLikeKeyword(ts, keyword) )
        isKeyword = true;

    else if( auto subExpr = tryparseSubExpr(ts) )
        baseExpr = subExpr;

    else
    {
        if( isMacro )
            ts.err(CEC.PExFuncAftMacro);
        return null;
    }

    if( isMacro && isKeyword )
        ts.err(CEC.PMacroKeyword);

    while( ts.peek.type == TOKlparen )
    {
        ts.popExpect(TOKlparen);

        Ast.Expr[] args;

        if( ts.peek.type != TOKrparen )
            ts.skipEolDo
            ({
                args ~= parseCommaExprs(ts, TOKrparen);
            });
        
        auto end = ts.popExpect(TOKrparen).loc;
        auto loc = start.extendTo(end);

        if( isKeyword )
        {
            isKeyword = false;

            switch( keyword )
            {
                case TOKhashtildequote:
                    if( args.length != 1 )
                        ts.err(CEC.PAstQArgNum, loc);
                    baseExpr = new Ast.AstQuoteExpr(loc, args[0]);
                    break;

                case TOKhashtildedquote:
                    if( args.length != 1 )
                        ts.err(CEC.PAstQQArgNum, loc);
                    baseExpr = new Ast.AstQuasiQuoteExpr(loc, args[0]);
                    break;

                case TOKhashtildedollar:
                    if( args.length != 1 )
                        ts.err(CEC.PAstQQSArgNum, loc);
                    baseExpr = new Ast.AstQQSubExpr(loc, args[0]);
                    break;

                case TOKlet:
                    if( args.length == 0 )
                        ts.err(CEC.PLetArgNum, loc);
                    baseExpr = new Ast.LetExpr(loc, args[0..$-1], args[$-1]);
                    break;

                case TOKimport:
                    if( args.length != 3 )
                        ts.err(CEC.PImpArgNum, loc);
                    baseExpr = new Ast.ImportExpr(loc, args[0], args[1],
                            args[2]);
                    break;

                default:
                    assert(false, "unimplemented keyword-like function");
            }
        }
        else
            baseExpr = new Ast.CallExpr(loc,
                    isMacro, baseExpr, args);

        // Only the first in the chain can be a macro call
        isMacro = false;
    }

    return baseExpr;
}

bool tryparseFunctionLikeKeyword(TokenStream ts, out TOK keyword)
{
    auto t = ts.peek;

    switch( t.type )
    {
        case TOKhashtildequote:
        case TOKhashtildedquote:
        case TOKhashtildedollar:
        case TOKlet:
        case TOKimport:
            break;

        default:
            return false;
    }

    keyword = ts.pop.type;
    return true;
}

Ast.Expr tryparseVariableExpr(TokenStream ts)
{
    if( ts.peek.type != TOKidentifier ) return null;

    auto loc = ts.peek.loc;
    auto ident = ts.pop.value;
    return new Ast.VariableExpr(loc, ident);
}

Ast.Expr tryparseSubExpr(TokenStream ts)
{
    if( ts.peek.type != TOKlparen ) return null;

    Ast.Expr expr;
    ts.skipEolDo
    ({
        ts.pop;
        expr = parseExpr(ts);
        ts.popExpect(TOKrparen);
    });
    return expr;
}

Ast.Expr tryparseRangeExpr(TokenStream ts)
{
    if( ts.peek.type != TOKrange ) return null;

    auto start = ts.pop.loc;
    Ast.Expr le, ue;
    bool li, ui;

    li = (ts.popExpectAny(TOKlparen,TOKlbracket).type == TOKlbracket);
    le = parseExpr(ts);
    ts.popExpect(TOKcomma);
    ue = parseExpr(ts);
    auto end = ts.peek.loc;
    ui = (ts.popExpectAny(TOKrparen,TOKrbracket).type == TOKrbracket);

    return new Ast.RangeExpr(start.extendTo(end), li, ui, le, ue);
}

Ast.Expr foldBinaryOp(Ast.BinaryExpr.Op op, Ast.Expr lhs, Ast.Expr rhs)
{
    alias Ast.BinaryExpr.Op Op;

    if( auto lhsBin = cast(Ast.BinaryExpr) lhs )
    {
        if( ( (lhsBin.op == Op.Lt || lhsBin.op == Op.LtEq)
                    && (op == Op.Lt || op == Op.LtEq) )
            ||
            ( (lhsBin.op == Op.Gt || lhsBin.op == Op.GtEq)
                    && (op == Op.Gt || op == Op.GtEq) )
          )
        {
            auto mid = sharedExpr(lhsBin.rhs);
            return evalSharedExpr(mid,
                new Ast.BinaryExpr(lhs.loc.extendTo(rhs.loc), Op.And,
                    new Ast.BinaryExpr(lhs.loc.extendTo(mid.loc),
                        lhsBin.op, lhsBin.lhs, mid),
                    new Ast.BinaryExpr(mid.loc.extendTo(rhs.loc),
                        op, mid, rhs)));
        }
    }
    else if( auto rhsBin = cast(Ast.BinaryExpr) rhs )
    {
        if( ( (rhsBin.op == Op.Lt || rhsBin.op == Op.LtEq)
                    && (op == Op.Lt || op == Op.LtEq) )
            ||
            ( (rhsBin.op == Op.Gt || rhsBin.op == Op.GtEq)
                    && (op == Op.Gt || op == Op.GtEq) )
          )
        {
            auto mid = sharedExpr(rhsBin.lhs);
            return evalSharedExpr(mid,
                new Ast.BinaryExpr(lhs.loc.extendTo(rhs.loc), op.And,
                    new Ast.BinaryExpr(lhs.loc.extendTo(mid.loc),
                        op, lhs, mid),
                    new Ast.BinaryExpr(mid.loc.extendTo(rhs.loc),
                        rhsBin.op, mid, rhsBin.rhs)));
        }
    }

    return new Ast.BinaryExpr(lhs.loc.extendTo(rhs.loc), op, lhs, rhs);
}

/**
    This wraps "complex" expressions in a SharedExpr node.

    This is used to ensure a given expression is only evaluated once.
*/
Ast.Expr sharedExpr(Ast.Expr expr)
{
    if( cast(Ast.NumberExpr) expr )
        return expr;

    if( cast(Ast.VariableExpr) expr )
        return expr;

    if( cast(Ast.StringExpr) expr )
        return expr;

    if( cast(Ast.LogicalExpr) expr )
        return expr;

    return new Ast.SharedExpr(expr.loc, expr);
}

/**
    Wraps a given expression in a EvalSharedExpr node IF a SharedExpr node is
    given.  If a SharedExpr isn't given, the expression isn't wrapped.
*/
Ast.Expr evalSharedExpr(Ast.Expr sharedExpr, Ast.Expr expr)
{
    if( auto se = cast(Ast.SharedExpr) sharedExpr )
        return new Ast.EvalSharedExpr(expr.loc, se, expr);
    
    else
        return expr;
}

