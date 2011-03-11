/**
    Code relating to formatting.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.Formatting;

import Integer = tango.text.convert.Integer;
import Utf = tango.text.convert.Utf;

struct UtfTruncator
{
    void delegate(char[]) emitFn;
    size_t l;           // number of code points left
    size_t over = 0;    // number of code points over limit

    static UtfTruncator opCall(void delegate(char[]) emitFn, size_t l)
    {
        UtfTruncator r;
        r.emitFn = emitFn;
        r.l = l;
        return r;
    }

    void emit(char[] s)
    {
        if( l == 0 )
            return;

        if( s.length > l )
        {
            // The string *might* be longer than the limit.  We don't know for
            // sure since UTF-8 is variable width.

            size_t cpl = 0;     // code point length
            size_t cul = ~0;    // code unit length
            foreach( i,dchar c ; s )
            {
                if( cpl == l )
                {
                    cul = i;
                    break;
                }
                cpl ++;
            }
            if( cul == ~0 ) cul = s.length;
            emitFn(s[0..cul]);
            l -= cul;
        }
        else
        {
            // More than wide enough.  We still need to count the code points,
            // though.
            emitFn(s);
            foreach( dchar c ; s )
                -- l;
        }
    }
}

bool parseTruncate(char[] s, char c, out size_t l)
{
    if( s == "" ) return false;
    if( s[0] != c ) return false;
    size_t ate;
    l = Integer.parse(s[1..$], 10, &ate);
    return (ate == s[1..$].length);
}

struct UtfPadder
{
    void delegate(char[]) emitFn;
    size_t w = 0;
    char a = '>';
    char[] p = " ";

    char[] buffer;
    size_t cpc = 0;

    static UtfPadder opCall(void delegate(char[]) emitFn,
            size_t w, char a, char[] p)
    {
        UtfPadder r;
        r.emitFn = emitFn;
        r.w = w;
        r.a = a;
        r.p = p;
        return r;
    }

    void init()
    {
    }

    void lock()
    {
        if( w == 0 )
            // Pass-through
            emit = emitFn, flush = &flushNop;

        else if( a == '<' )
            emit = &emitLeft, flush = &flushLeft;

        else if( a == '|' )
            emit = &emitBuffered, flush = &flushCentre;

        else // if( a == '>' )
            emit = &emitBuffered, flush = &flushRight;
    }

    void delegate(char[]) emit;
    void delegate() flush;

    void flushNop() {}

    void emitLeft(char[] s)
    {
        foreach( dchar c ; s )
            cpc ++;
        emitFn(s);
    }

    void flushLeft()
    {
        if( cpc > w )
            return;

        auto n = (w-cpc);

        emitPadding(n);
    }

    void emitBuffered(char[] s)
    {
        // TODO: use a static backing buffer for small strings
        if( cpc >= w )
            emitFn(s);
        else
            buffer ~= s;

        // Count number of code points
        foreach( i,dchar c ; s )
            cpc ++;

        if( cpc >= w && buffer !is null )
        {
            emitFn(buffer);
            buffer = null;
        }
    }

    void flushCentre()
    {
        if( cpc >= w ) return;

        if( buffer.length == 0 )
            emitPadding(w);

        else
        {
            size_t diff = (w - cpc);
            auto l = diff / 2;
            auto r = diff - l;

            emitPadding(l);
            emitFn(buffer);
            emitPadding(r);
        }
    }

    void flushRight()
    {
        if( cpc >= w ) return;

        if( buffer.length == 0 )
            emitPadding(w);

        else
        {
            size_t n = (w - cpc);
            emitPadding(n);
            emitFn(buffer);
        }
    }

    void emitPadding(size_t n)
    {
        if( p.length == 1 )
        {
            while( n > 0 )
            {
                emitFn(p);
                --n;
            }
        }
        else
        {
            assert( p.length > 0 );
            while( n > 0 )
            {
                auto s = p;
                uint ate;
                while( s != "" && n > 0 )
                {
                    Utf.decode(s, ate);
                    emitFn(s[0..ate]);
                    s = s[ate..$];
                    --n;
                }
            }
        }
    }
}

