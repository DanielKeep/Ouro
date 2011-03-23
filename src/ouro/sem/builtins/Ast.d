/**
    AST related builtins.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.Ast;

private:

import ouro.sem.builtins.Util; // many things

NodeType chkArgAstNodeType(NodeType)(Value args[], size_t i)
{
    auto ast = chkArgAstNode(args, i);
    auto r = cast(NodeType) ast;
    assert( r !is null, "expected ast "~NodeType.stringof~"; got "
            ~ ast.classinfo.name );
    return r;
}

char[] genNodeDispatch_ctfe(char[] nodeName, char[] stmts)
{
    char[] r;

    r ~= "if( false ) {}\n";

    foreach( name ; Ast.Nodes )
    {
        r ~= "else if( auto _node = cast(Ast."~name~") "~nodeName~" ) {\n";
        r ~= "const _nodeName = \""~name~"\";\n";
        r ~= stmts;
        r ~= "}\n";
    }

    r ~= "else assert( false, \"missing visit for \"~"~nodeName
        ~".classinfo.name );";

    return r;
}

static this() { Builtins.register("ouro.ast.new_LambdaExpr",
        new Sit.FunctionValue("ouro.ast.new_LambdaExpr",
            [Sit.Argument("loc"),
             Sit.Argument("isMacro"),
             Sit.Argument("args"),
             Sit.Argument("expr")],
            &ast_new_LambdaExpr, EC.All, true
            )); }

Value ast_new_LambdaExpr(EC ec, Value[] args)
{
    chkArgNum(args, 4);
    chkArgNil(args, 0); // don't support Locations yet.
    auto isMacro = chkArgLogical(args, 1);
    auto argExprs = chkArgList(args, 2).elemValues;
    auto expr = chkArgAst(args, 3);

    auto largs = new Ast.Argument[argExprs.length];

    foreach( i,argExpr ; argExprs )
    {
        if( auto n = cast(Sit.SymbolValue) argExpr )
            largs[i] = Ast.Argument(Location.init, n.value, false);

        else if( auto n = cast(Sit.ListValue) argExpr )
        {
            assert( n.elemValues.length == 2 );
            auto ni = cast(Sit.SymbolValue) n.elemValues[0];
            auto nv = cast(Sit.LogicalValue) n.elemValues[1];
            assert( ni !is null );
            assert( nv !is null );
            largs[i] = Ast.Argument(Location.init, ni.value, nv.value);
        }
        else if( auto nast = cast(Sit.AstQuoteValue) argExpr )
        {
            if( auto n = cast(Ast.VariableExpr) nast.ast )
                largs[i] = Ast.Argument(n.loc, n.ident, false);

            else if( auto n = cast(Ast.ExplodeExpr) nast.ast )
            {
                auto ni = cast(Ast.VariableExpr) n.subExpr;
                assert( ni !is null );
                largs[i] = Ast.Argument(n.loc, ni.ident, true);
            }
            else if( auto n = cast(Ast.ListExpr) nast.ast )
            {
                assert( n.elemExprs.length == 2 );
                auto ni = cast(Ast.VariableExpr) n.elemExprs[0];
                auto nv = cast(Ast.LogicalExpr) n.elemExprs[1];
                assert( ni !is null );
                assert( nv !is null );
                largs[i] = Ast.Argument(n.loc, ni.ident, nv.value);
            }
            else
                assert( false, "unexpected argument ast type " ~
                        nast.ast.classinfo.name );
        }
        else
            assert( false, "unexpected argument type " ~
                    argExpr.classinfo.name );
    }

    return new Sit.AstQuoteValue(null,
            new Ast.LambdaExpr(Location.init, isMacro, largs, expr));
}

static this() { Builtins.register("ouro.ast.new_ListExpr",
        new Sit.FunctionValue("ouro.ast.new_ListExpr",
            [Sit.Argument("loc"),
             Sit.Argument("exprs")],
            &ast_new_ListExpr, EC.All, true
            )); }

Value ast_new_ListExpr(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    chkArgNil(args, 0); // don't support Locations yet.
    auto listValues = chkArgList(args, 1).elemValues;

    auto listExprs = new Ast.Expr[listValues.length];
    foreach( i,listValue ; listValues )
    {
        auto aqv = cast(Sit.AstQuoteValue) listValue;
        assert( aqv !is null );
        auto expr = cast(Ast.Expr) aqv.ast;
        assert( expr !is null );
        listExprs[i] = expr;
    }

    return new Sit.AstQuoteValue(null,
            new Ast.ListExpr(Location.init, listExprs));
}

static this() { Builtins.register("ouro.ast.new_VariableExpr",
        new Sit.FunctionValue("ouro.ast.new_VariableExpr",
            [Sit.Argument("loc"),
             Sit.Argument("exprs")],
            &ast_new_VariableExpr, EC.All, true
            )); }

Value ast_new_VariableExpr(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    chkArgNil(args, 0); // don't support Locations yet.
    auto ident = chkArgStringOrSymbol(args, 1);

    return new Sit.AstQuoteValue(null,
        new Ast.VariableExpr(Location.init, ident)
    );
}

static this() { Builtins.register("ouro.ast.binaryExprLhs",
        new Sit.FunctionValue("ouro.ast.binaryExprLhs",
            [Sit.Argument("ast")],
            &ast_binaryExprLhs, EC.All, true
            )); }

Value ast_binaryExprLhs(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto ast = chkArgAstNodeType!(Ast.BinaryExpr)(args, 0);
    
    return new Sit.AstQuoteValue(null, ast.lhs);
}

static this() { Builtins.register("ouro.ast.binaryExprOp",
        new Sit.FunctionValue("ouro.ast.binaryExprOp",
            [Sit.Argument("ast")],
            &ast_binaryExprOp, EC.All, true
            )); }

Value ast_binaryExprOp(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto ast = chkArgAstNodeType!(Ast.BinaryExpr)(args, 0);
    
    return new Sit.SymbolValue(null, ast.opToString(ast.op));
}

static this() { Builtins.register("ouro.ast.binaryExprRhs",
        new Sit.FunctionValue("ouro.ast.binaryExprRhs",
            [Sit.Argument("ast")],
            &ast_binaryExprRhs, EC.All, true
            )); }

Value ast_binaryExprRhs(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto ast = chkArgAstNodeType!(Ast.BinaryExpr)(args, 0);
    
    return new Sit.AstQuoteValue(null, ast.rhs);
}

static this() { Builtins.register("ouro.ast.nodeType",
        new Sit.FunctionValue("ouro.ast.nodeType",
            [Sit.Argument("ast")],
            &ast_nodeType, EC.All, true
            )); }

Value ast_nodeType(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto ast = chkArgAstNode(args, 0);

    Sit.SymbolValue sym;

    mixin(genNodeDispatch_ctfe
    (
        "ast",
        `
            sym = new Sit.SymbolValue(null, _nodeName);
        `
    ));

    return sym;
}

static this() { Builtins.register("ouro.ast.listExprElems",
        new Sit.FunctionValue("ouro.ast.listExprElems",
            [Sit.Argument("ast")],
            &ast_listExprElems, EC.All, true
            )); }

Value ast_listExprElems(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto ast = chkArgAstNodeType!(Ast.ListExpr)(args, 0);
    auto elems = new Value[ast.elemExprs.length];

    foreach( i,elemExpr ; ast.elemExprs )
        elems[i] = new Sit.AstQuoteValue(null, elemExpr);

    return new Sit.ListValue(null, elems);
}

static this() { Builtins.register("ouro.ast.variableIdent",
        new Sit.FunctionValue("ouro.ast.variableIdent",
            [Sit.Argument("ast")],
            &ast_variableIdent, EC.All, true
            )); }

Value ast_variableIdent(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto ast = chkArgAstNodeType!(Ast.VariableExpr)(args, 0);
    
    return new Sit.SymbolValue(null, ast.ident);
}

