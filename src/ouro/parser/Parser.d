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

Ast.Module parseModule(TokenStream ts)
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

    return new Ast.Module(loc, stmts);
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
    /*
        <expression statement> = <expression>, <term>;
    */

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
        Ast.Expr func;
        Ast.BinaryExpr.Op op;
        float prec;
        Fixity fixity;
    }

    Ast.Expr[] exprs;
    Op[] ops;

    void pushExpr(Ast.Expr expr)
    {
        exprs ~= expr;
    }

    void pushOpOrInfixFunc(Ast.BinaryExpr.Op binOp, Ast.Expr func)
    {
        if( func !is null )
            pushInfixFunc(func);
        else
            pushOp(binOp);
    }

    void pushOp(Ast.BinaryExpr.Op binOp)
    {
        Op top;
        top.op = binOp;
        top.prec = precOf(binOp);
        top.fixity = fixityOf(binOp);

        while( ops.length > 0 && ops[$-1].prec > top.prec )
            crushTop;

        ops ~= top;
    }

    void pushInfixFunc(Ast.Expr func)
    {
        while( ops.length > 0 )
            crushTop;

        Op top;
        top.func = func;

        /*
            Infix functions have the lowest possible precedence, and are
            always left-associative.
        */

        top.prec = -float.infinity;
        top.fixity = Fixity.Left;
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
        auto fix = ops[$-1].fixity;

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
            if( ops[0].func !is null )
                lhs = new Ast.InfixFuncExpr(lhs.loc.extendTo(exprs[0].loc),
                        ops[0].func, lhs, exprs[0]);

            else
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
            if( ops[$-1].func !is null )
                rhs = new Ast.InfixFuncExpr(exprs[$-1].loc.extendTo(rhs.loc),
                        ops[$-1].func, exprs[$-1], rhs);

            else
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
    Ast.Expr expr, infixFunc;
    if( tryparseBinaryOp(ts, op, infixFunc) )
    {
        ExprState st;
        st.pushExpr(lhs);
        st.pushOpOrInfixFunc(op, infixFunc);

        while( true )
        {
            st.pushExpr(parseExprAtom(ts));

            if( tryparseBinaryOp(ts, op, infixFunc) )
                st.pushOpOrInfixFunc(op, infixFunc);

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
    /*
        <prefix op> = "+" | "-" | "not";
    */

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

bool tryparseBinaryOp(TokenStream ts, out Ast.BinaryExpr.Op op,
        out Ast.Expr infixFunc)
{
    /*
        <binary op> = "=" | "!=" | "<>"
                    | "<" | "<=" | ">" | ">="
                    | "+" | "-" | "*" | "/" | "//"
                    | "mod" | "rem"
                    | "**"
                    | "and" | "or"
                    | "." | "::" | "++"
                    | "(.", <infix function>, ".)"
                    ;
    */

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
        case TOKlparenperiodrparen: op = Op.Comp;   break;

        case TOKand:        op = Op.And;    break;
        case TOKor:         op = Op.Or;     break;
        case TOKmod:        op = Op.Mod;    break;
        case TOKrem:        op = Op.Rem;    break;

        case TOKlparenperiod:
            {
                auto start = ts.pop.loc;
                infixFunc = parseInfixFunction(ts);
                auto end = ts.popExpect(TOKperiodrparen).loc;

                return true;
            }
            break;

        default:
            return false;
    }

    ts.pop;
    return true;
}

Ast.Expr parseInfixFunction(TokenStream ts)
{
    /*
        <infix function> = <identifier>
                         | <sub expression>;
    */

    if( auto expr = tryparseVariableExpr(ts) )
        return expr;

    if( auto expr = tryparseSubExpr(ts) )
        return expr;

    ts.err(CEC.PExInfFunc);
}

Ast.Expr tryparsePostfixExpr(TokenStream ts, Ast.Expr expr)
{
    /*
        <postfix op> = "(.", <postfix function>, ")";

        <postfix function> = <infix function>;
    */

    if( ts.peek.type != TOKlparenperiod )
        return expr;

    auto start = ts.pop.loc;
    Ast.Expr funcExpr = parseInfixFunction(ts);

    auto end = ts.popExpect(TOKrparen).loc;

    return new Ast.PostfixFuncExpr(start.extendTo(end), funcExpr, expr);
}

Ast.Expr tryparseExprAtom(TokenStream ts)
{
    /*
        <expression atom> = [ <prefix op> ],
                                ( <number expression>
                                | <string expression>
                                | <logical expression>
                                | <nil expression>
                                | <list expression>
                                | <map expression>
                                | <lambda expression>
                                | <range expression>
                                | <function expression>
                                | <variable expression>
                                | <sub expression>
                                ),
                            [ <explode> ];
    */

    Ast.PrefixExpr.Op prefixOp;
    auto start = ts.peek.loc;
    auto hasPrefix = tryparsePrefixOp(ts, prefixOp);

    auto innerExpr =
    (delegate Ast.Expr(){
        if( auto e = tryparseNumberExpr(ts) )       return e;
        if( auto e = tryparseStringExpr(ts) )       return e;
        if( auto e = tryparseLogicalExpr(ts) )      return e;
        if( auto e = tryparseNilExpr(ts) )          return e;
        if( auto e = tryparseListExpr(ts) )         return e;
        if( auto e = tryparseMapExpr(ts) )          return e;
        if( auto e = tryparseLambdaExpr(ts) )       return e;
        if( auto e = tryparseRangeExpr(ts) )        return e;
        if( auto e = tryparseFunctionOrVariableOrSubExpr(ts) )
                                                    return e;

        return null;
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
    /*
        <number expression> = <number>;
    */

    if( ts.peek.type != TOKnumber ) return null;

    auto loc = ts.peek.loc;
    auto value = parseReal(ts.pop.value);
    auto expr = new Ast.NumberExpr(loc, value);

    return expr;
}

Ast.Expr tryparseStringExpr(TokenStream ts)
{
    /*
        <string expression> = <string>;
    */

    if( ts.peek.type != TOKstring ) return null;

    auto loc = ts.peek.loc;
    char[] value = ts.pop.value;
    return new Ast.StringExpr(loc, value);
}

Ast.Expr tryparseLogicalExpr(TokenStream ts)
{
    /*
        <logical expression> = "true" | "false";
    */

    if( ts.peek.type == TOKtrue )
        return new Ast.LogicalExpr(ts.pop.loc, true);

    else if( ts.peek.type == TOKfalse )
        return new Ast.LogicalExpr(ts.pop.loc, false);

    else
        return null;
}

Ast.Expr tryparseNilExpr(TokenStream ts)
{
    /*
        <nil expression> = "nil";
    */

    if( ts.peek.type != TOKnil ) return null;

    return new Ast.NilExpr(ts.pop.loc);
}

Ast.Expr tryparseListExpr(TokenStream ts)
{
    /*
        <list expression> = "[", [ <expression>, { ",", <expression> } ], "]";
    */

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
    /*
        Helper method.  Not actually in the grammar.  If it was, it'd look
        like this:

        <comma exprs (stop)> =
            <expression>, { ",", <expression> }, <stop>;
    */

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
    /*
        <map expression> = "[:",
            [ <key value pair>, { ",", <key value pair> } ], ":]";
    */

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
    /*
        <key value pair> = <expression>, ":", <expression>;
    */

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
        <function expression> = <function prefix>, (
            "(", [ <expression>, { ",", <expression> } ], ")"
            | "{", [ <expression>, { ",", <expression> } ], "}" );

        <function prefix> = <identifier>
                          | <function like keyword>
                          | <sub expression>
                          | <function expression>
                          ;
    */

    Ast.Expr baseExpr;

    auto start = ts.peek.loc;
    TOK keyword;
    bool isKeyword = false;

    if( auto varExpr = tryparseVariableExpr(ts) )
        baseExpr = varExpr;

    else if( tryparseFunctionLikeKeyword(ts, keyword) )
        isKeyword = true;

    else if( auto subExpr = tryparseSubExpr(ts) )
        baseExpr = subExpr;

    bool isLparenOrLbrace(TOK t)
    {
        return t == TOKlparen || t == TOKlbrace;
    }

    bool isRparenOrRbrace(TOK t)
    {
        return t == TOKrparen || t == TOKrbrace;
    }

    while( isLparenOrLbrace(ts.peek.type) )
    {
        auto isMacro = (ts.pop.type == TOKlbrace);
        auto closer = (isMacro ? TOKrbrace : TOKrparen);

        Ast.Expr[] args;

        if( ! isRparenOrRbrace(ts.peek.type) )
            ts.skipEolDo
            ({
                args ~= parseCommaExprs(ts, closer);
            });
        
        // Remember: we need to skip over EOLs until we're out of the argument
        // list.
        typeof(ts.peek.loc) end;
        ts.skipEolDo
        ({
            end = ts.popExpect(closer).loc;
        });

        auto loc = start.extendTo(end);

        if( isKeyword )
        {
            isKeyword = false;

            switch( keyword )
            {
                case TOKhashquote:
                    if( args.length != 1 )
                        ts.err(CEC.PAstQArgNum, loc);
                    if( ! isMacro )
                        ts.err(CEC.PExBraceForKw, loc, `#'`);
                    baseExpr = new Ast.AstQuoteExpr(loc, args[0]);
                    break;

                case TOKhashdquote:
                    if( args.length != 1 )
                        ts.err(CEC.PAstQQArgNum, loc);
                    if( ! isMacro )
                        ts.err(CEC.PExBraceForKw, loc, `#"`);
                    baseExpr = new Ast.AstQuasiQuoteExpr(loc, args[0]);
                    break;

                case TOKhashdollar:
                    if( args.length != 1 )
                        ts.err(CEC.PAstQQSArgNum, loc);
                    if( ! isMacro )
                        ts.err(CEC.PExBraceForKw, loc, `#$`);
                    baseExpr = new Ast.AstQQSubExpr(loc, args[0]);
                    break;

                case TOKlet:
                    if( args.length == 0 )
                        ts.err(CEC.PLetArgNum, loc);
                    if( ! isMacro )
                        ts.err(CEC.PExBraceForKw, loc, `let`);
                    baseExpr = new Ast.LetExpr(loc, args[0..$-1], args[$-1]);
                    break;

                case TOKimport:
                    if( args.length != 3 )
                        ts.err(CEC.PImpArgNum, loc);
                    if( ! isMacro )
                        ts.err(CEC.PExBraceForKw, loc, `import`);
                    baseExpr = new Ast.ImportExpr(loc, args[0], args[1],
                            args[2]);
                    break;

                case TOKbuiltin:
                    if( args.length != 1 )
                        ts.err(CEC.PBiArgNum, loc);
                    if( isMacro )
                        ts.err(CEC.PExParenForKw, loc, `__builtin__`);

                    if( auto arg0 = cast(Ast.StringExpr) args[0] )
                        baseExpr = new Ast.BuiltinExpr(loc, arg0.value);
                    
                    else
                        ts.err(CEC.PBiArgType, loc);

                    break;

                default:
                    assert(false, "unimplemented keyword-like function");
            }
        }
        else
            baseExpr = new Ast.CallExpr(loc,
                    isMacro, baseExpr, args);
    }

    return baseExpr;
}

bool tryparseFunctionLikeKeyword(TokenStream ts, out TOK keyword)
{
    /*
        <function like keyword> = "#'"
                                | "#\""
                                | "#$"
                                | "let"
                                | "import"
                                | "__builtin__"
                                ;
    */

    auto t = ts.peek;

    switch( t.type )
    {
        case TOKhashquote:
        case TOKhashdquote:
        case TOKhashdollar:
        case TOKlet:
        case TOKimport:
        case TOKbuiltin:
            break;

        default:
            return false;
    }

    keyword = ts.pop.type;
    return true;
}

Ast.Expr tryparseVariableExpr(TokenStream ts)
{
    /*
        <variable expression> = <identifier>;
    */

    if( ts.peek.type != TOKidentifier ) return null;

    auto loc = ts.peek.loc;
    auto ident = ts.pop.value;
    return new Ast.VariableExpr(loc, ident);
}

Ast.Expr tryparseSubExpr(TokenStream ts)
{
    /*
        <sub expression> = "(", <treat eol as whitespace( expression )>, ")";
    */

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
    /*
        <range expression> = "range",
            ( "[" | "(" ), <expression>, ",",
            <expression>, ( "]" | ")" );
    */

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

/**
    Used to perform folds like (a < x < b) --> (a < x) and (x < b).
*/
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
            return new Ast.TernaryExpr(lhs.loc.extendTo(rhs.loc),
                    Ast.TernaryExpr.opFromBinary(lhsBin.op, op),
                    lhsBin.lhs, lhsBin.rhs, rhs);
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
            return new Ast.TernaryExpr(lhs.loc.extendTo(rhs.loc),
                    Ast.TernaryExpr.opFromBinary(op, rhsBin.op),
                    lhs, rhsBin.lhs, rhsBin.rhs);
        }
    }

    return new Ast.BinaryExpr(lhs.loc.extendTo(rhs.loc), op, lhs, rhs);
}

