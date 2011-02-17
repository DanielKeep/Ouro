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

import ouro.Error : CompilerException;
import ouro.Source;
import ouro.ast.ReprVisitor;
import ouro.util.StructuredOutput;
import ouro.util.TokenStream;

int main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    bool throwExc = false;

    scope so = new StructuredOutput(Stdout.stream);
    scope repr = new ReprVisitor(so);

    foreach( path ; args )
    {
        if( path == "--throw" )
            throwExc = true;

        else
        {
            scope src = new Source(path, cast(char[]) File.get(path));
            scope ts = new TokenStream(src, &Lexer.lexNext);

            try
            {
                auto node = cast(Ast.Node) Parser.parseModule(ts);
                repr.visitBase(node);
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

