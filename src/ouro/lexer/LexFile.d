/**
    Sample program which lexes a file into a sequence of tokens.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.lexer.LexFile;

import tango.io.Stdout;
import tango.io.device.File;

import tango.core.tools.TraceExceptions;

import ouro.lexer.Lexer : lexIter;

void main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];

    foreach( path ; args )
    {
        foreach( token ; lexIter(path, cast(char[]) File.get(path)) )
            Stdout("  ")(token.toString).newline;
    }
}

