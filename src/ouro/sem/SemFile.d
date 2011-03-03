/**
    Sample program which produces a semantic tree for a given file.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.parser.SemFile;

import tango.io.Stdout;
import tango.io.device.File;
import tango.text.Util : trimr;

import tango.core.tools.TraceExceptions;

import Ast      = ouro.ast.Nodes;
import Builtins = ouro.sem.Builtins;
import Lexer    = ouro.lexer.Lexer;
import Parser   = ouro.parser.Parser;
import Sem      = ouro.sem.Semantic;
import SemCtx   = ouro.sem.Context;
import Sit      = ouro.sit.Nodes;

import ouro.Error : CompilerException;
import ouro.Source;
import ouro.sit.ReprVisitor;
import ouro.util.StructuredOutput;
import ouro.util.TokenStream;

int main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    bool throwExc = false;

    auto repr = ReprVisitor.forStdout;
    auto errRepr = ReprVisitor.forStderr;

    Sit.Value builtin(char[] name)
    {
        return Builtins.lookupBuiltin(name);
    }

    foreach( path ; args )
    {
        if( path == "--throw" )
            throwExc = true;

        else
        {
            scope src = new Source(path, cast(char[]) File.get(path));
            scope ts = new TokenStream(src, &Lexer.lexNext);
            scope siv = new Sem.SemInitialVisitor;

            try
            {
                auto astModule = Parser.parseModule(ts);
                auto ctx = SemCtx.Context(null, &builtin);
                ctx.dumpNode = (Sit.Node node)
                {
                    errRepr.visitBase(node, true);
                };
                auto sitModule = siv.visitBase(astModule, &ctx);
                repr.visitBase(sitModule, false);
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

