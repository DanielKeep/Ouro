/**
    Useful token test functions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.TokenTest;

public import tango.text.Unicode : isWhitespace, isSpace, isDigit, isLetter;

bool isIdentStart(dchar c)
{
    return (c == '_') || isLetter(c);
}

bool isIdent(dchar c)
{
    switch( c )
    {
        case '\'':
        case '$':
        case '|':
        case '?':
        case '!':
        case '~':
            return true;

        default:
            return isIdentStart(c) || isDigit(c);
    }
}

bool isInnerDigit(dchar c)
{
    return (c == '_') || isDigit(c);
}

