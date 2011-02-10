/**
    Sample program which parses a file into an AST.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.parser.ParseFile;

import tango.io.Stdout;
import tango.io.device.File;

import tango.core.tools.TraceExceptions;

import Ast      = ouro.ast.Nodes;
import Lexer    = ouro.lexer.Lexer;
import Parser   = ouro.parser.Parser;

import ouro.Source;
import ouro.ast.ReprVisitor;
import ouro.util.StructuredOutput;
import ouro.util.TokenStream;

void main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    scope so = new StructuredOutput(Stdout.stream);
    scope repr = new ReprVisitor(so);

    foreach( path ; args )
    {
        Stdout.formatln("Parsing \"{}\"...", path);
        scope src = new Source(path, cast(char[]) File.get(path));
        scope ts = new TokenStream(src, &Lexer.lexNext);

        auto progNode = Parser.parseModule(ts);
        repr.visitBase(progNode);
        Stdout.newline;
    }
}

