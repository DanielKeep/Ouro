/**
    Sample program which parses a file into an AST.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.parser.ParseFile;

import tango.io.Stdout;
import tango.io.device.File;
import tango.text.Util : trimr;

import tango.core.tools.TraceExceptions;

import Ast      = ouro.ast.Nodes;
import Lexer    = ouro.lexer.Lexer;
import Parser   = ouro.parser.Parser;
import StmtRewrite = ouro.ast.StmtRewrite;

import ouro.Error : CompilerException;
import ouro.Source;
import ouro.ast.ReprVisitor;
import ouro.ast.ExprSimplify;
import ouro.util.StructuredOutput;
import ouro.util.TokenStream;

int main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    bool rewriteStmt = false;
    bool simplifyExpr = false;
    bool throwExc = false;

    scope so = new StructuredOutput(Stdout.stream);
    scope repr = new ReprVisitor(so);

    foreach( path ; args )
    {
        if( path == "--rewrite-stmt" )
            rewriteStmt = true;

        else if( path == "--simplify-expr" )
        {
            rewriteStmt = true;
            simplifyExpr = true;
        }
        else if( path == "--throw" )
            throwExc = true;

        else
        {
            scope src = new Source(path, cast(char[]) File.get(path));
            scope ts = new TokenStream(src, &Lexer.lexNext);

            try
            {
                auto node = cast(Ast.Node) Parser.parseModule(ts);

                if( ! rewriteStmt )
                {
                    repr.visitBase(node);
                }
                else
                {
                    struct Stmt
                    {
                        Ast.Expr expr;
                        bool bind;
                        char[] bindIdent;
                        bool mergeAll;
                        char[][] mergeList;
                    }

                    auto mod = cast(Ast.Module) node;
                    Stmt[] stmts;

                    foreach( modStmt ; mod.stmts )
                    {
                        Stmt stmt;
                        stmt.expr = StmtRewrite.rewriteStmt(mod, modStmt,
                                stmt.bind, stmt.bindIdent,
                                stmt.mergeAll, stmt.mergeList);
                        stmts ~= stmt;
                    }

                    if( simplifyExpr )
                    {
                        scope se = new SimplifyExpr;

                        foreach( ref stmt ; stmts )
                            stmt.expr =
                                cast(Ast.Expr) se.visitBase(stmt.expr);
                    }

                    foreach( stmt ; stmts )
                    {
                        if( stmt.bind )
                        {
                            Stdout("<bind ")(stmt.bindIdent)("> ");
                        }
                        else if( stmt.mergeAll )
                        {
                            Stdout("<merge *> ");
                        }
                        else if( stmt.mergeList.length > 0 )
                        {
                            Stdout("<merge");
                            foreach( i,sym ; stmt.mergeList )
                                Stdout(i==0 ? " " : ", ")
                                    (sym);
                            Stdout("> ");
                        }
                        repr.visitBase(stmt.expr);
                        Stdout.newline;
                    }
                }
            }
            catch( CompilerException e )
            {
                Stderr(e.toString).newline().flush();
                Stderr
                    .formatln("{,6}: {}", e.loc.line,
                            trimr(src.line(e.loc.line)))
                    ("        ");

                auto startCol = src.startCol;

                for( size_t i=0; i<e.loc.column - startCol; ++i )
                    Stderr(' ');
                for( size_t i=0; i<e.loc.length; ++i )
                    Stderr('^');

                Stderr.newline;

                if( throwExc )
                    throw e;

                else
                    return 1;
            }
        }
    }

    return 0;
}

