/**
    Parse functions for specific types.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.util.Parse;

import Float    = tango.text.convert.Float;
import Integer  = tango.text.convert.Integer;
import Utf      = tango.text.convert.Utf;

real parseReal(char[] text)
{
    return Float.parse(text);
}

char[] parseString(char[] text)
{
    auto r = text.dup;
    size_t r_i = 0;
    auto s = text;

    void putcu(char cu)
    {
        if( r_i >= r.length )
            r.length = r.length + (r.length / 2);
        r[r_i++] = cu;
    }

    void puts(char[] s)
    {
        if( s.length+r_i >= r.length )
            r.length = r.length + s.length + (r.length / 2);
        r[r_i..r_i+s.length] = s;
        r_i += s.length;
    }

    while( s.length > 0 )
    {
        if( s[0] == '\\' )
        {
            auto c = s[1];
            dchar ech;
            s = s[2..$];
            switch( c )
            {
                case 'a':   ech = '\a'; break;
                case 'b':   ech = '\b'; break;
                case 'f':   ech = '\f'; break;
                case 'n':   ech = '\n'; break;
                case 'r':   ech = '\r'; break;
                case 't':   ech = '\t'; break;
                case 'v':   ech = '\v'; break;
                case '\'':  ech = '\''; break;
                case '"':   ech = '"'; break;
                case '?':   ech = '\x1b'; break;
                case '\\':  ech = '\\'; break;

                case 'x':
                    ech = cast(dchar) Integer.convert(s[0..2], 16);
                    s = s[2..$];
                    break;

                case 'u':
                    ech = cast(dchar) Integer.convert(s[0..4], 16);
                    s = s[4..$];
                    break;

                case 'U':
                    ech = cast(dchar) Integer.convert(s[0..8], 16);
                    s = s[8..$];
                    break;

                default:
                    assert(false, "invalid string escape sequence");
            }
            char[8] buffer;
            puts(Utf.encode(buffer, ech));
        }
        else
        {
            size_t i=1;
            while( i < s.length && s[i] != '\\' )
                ++i;

            puts(s[0..i]);
            s = s[i..$];
        }
    }

    return r;
}

