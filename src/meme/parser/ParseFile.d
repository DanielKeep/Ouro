/**
    Sample program which parses a file into an AST.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.parser.ParseFile;

import tango.io.Stdout;
import tango.io.device.File;

import tango.core.tools.TraceExceptions;

import Parse = ouro.parser.Parser;

void main(char[][] argv)
{
    auto exec = argv[0];
    auto args = argv[1..$];
}

