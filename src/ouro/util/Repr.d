/**
    Formatting code.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.Repr;

import tango.text.convert.Format;

import Float    = tango.text.convert.Float;
import Utf      = tango.text.convert.Utf;

import ouro.util.TokenTest : isIdentStart, isIdent;

char[] reprNil()
{
    return "nil";
}

char[] reprLogical(bool value)
{
    return (value ? "true" : "false");
}

char[] reprReal(real value)
{
    return Float.truncate(Float.toString(value, 30));
}

char[] reprIdent(char[] ident)
{
    assert( ident != "" );

    // First, check to see whether we can just output it verbatim.
    bool basic = true;
    foreach( i, dchar cp ; ident )
    {
        if( i == 0 )
            basic = isIdentStart(cp);
        else
            basic = isIdent(cp);

        if( !basic ) break;
    }

    if( basic )
        return ident;

    // Not basic; we'll just have to quote it.
    return "$" ~ reprString(ident);
}

char[] reprString(char[] text)
{
    // TODO: make this code not slow and horrible
    char[] r = "\"";

    foreach( dchar cp ; text )
    {
        if( cp < ' ' )
            r ~= Format("\\x{:x,02}", cast(int) cp);
        
        else if( cp == '"' )
            r ~= "\\\"";

        else
        {
            char[8] buffer;
            r ~= Utf.encode(buffer, cp);
        }
    }

    r ~= "\"";

    return r;
}

char[] reprSymbol(char[] text)
{
    auto ident = reprIdent(text);
    if( ident.length >= 2 && ident[0..2] == "$\"" )
    {
        ident[0] = '\'';
        return ident;
    }
    else
        return '\'' ~ ident;
}

