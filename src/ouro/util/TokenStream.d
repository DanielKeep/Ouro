/**
    Token Stream.

    This provides a high-level interface to a stream of Tokens being generated
    by a callback function.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.util.TokenStream;

import ouro.Error;
import ouro.Location;
import ouro.Source;
import ouro.lexer.Tokens;

private
{
    alias CompilerErrorCode CEC;
}

final class TokenStream
{
    alias bool function(Source, out Token) NextToken;

    Source src;
    NextToken next;

    uint skipEolCounter = 0;

    this(Source src, NextToken next)
    {
        this.src = src;
        this.next = next;

        this.cache = new Token[](BaseCacheSize);
        this.cached = 0;
    }

    void err(CEC code, char[] arg0 = null, char[] arg1 = null)
    {
        err(code, peek.loc, arg0, arg1);
    }

    void err(CEC code, Location loc, char[] arg0 = null, char[] arg1 = null)
    {
        throw new CompilerException(code, loc, arg0, arg1);
    }

    struct Checkpoint
    {
        uint skipEolCounter = 0;
        Source.Mark mark;

        static Checkpoint opCall(TokenStream ts)
        {
            Checkpoint r;
            r.skipEolCounter = ts.skipEolCounter;
            r.mark = ts.src.save;
            return r;
        }
    }

    Checkpoint save()
    {
        return Checkpoint(this);
    }

    void restore(ref Checkpoint cp)
    {
        this.skipEolCounter = cp.skipEolCounter;
        this.src.restore(cp.mark);
    }

    void pushSkipEol()
    {
        assert( skipEolCounter < skipEolCounter.max );
        ++ skipEolCounter;
    }

    void popSkipEol()
    {
        assert( skipEolCounter > 0 );
        -- skipEolCounter;
    }

    void skipEolDo(void delegate() dg)
    {
        pushSkipEol;
        dg();
        popSkipEol;
    }

    void unskipEolDo(void delegate() dg)
    {
        auto oldCounter = skipEolCounter;
        skipEolCounter = 0;
        dg();
        skipEolCounter = oldCounter;
    }

    bool skipEol()
    {
        return skipEolCounter > 0;
    }

    Token peek()
    {
        return peek(0);
    }

    Token peek(size_t n)
    {
        if( skipEol )
        {
            size_t i = 0, j = 0;
            Token t;
            unskipEolDo
            ({
                do
                {
                    t = peek(i++);
                    if( t.type == TOKeos )
                        return;
                    else if( t.type != TOKeol )
                        ++ j;

                    if( j == n+1 )
                        return;
                }
                while(true);
            });
            return t;
        }

        if( cached > n )
            return cache[n];

        assert( cached <= n );

        if( next is null )
            return Token.init;

        assert( next !is null );

        if( cache.length <= n )
        {
            size_t newSize = cache.length*2;
            while( newSize <= n )
                newSize *= 2;

            auto newCache = new Token[](newSize);
            newCache[0..cache.length] = cache;
            delete cache;
            cache = newCache;
        }

        assert( cache.length > n );

        foreach( ref cacheEl ; cache[cached..n+1] )
        {
            getNext(src, cacheEl);

            ++ cached;

            if( cacheEl.type == TOKeos )
            {
                next = null;
                break;
            }
        }

        if( cached > n )
            return cache[n];

        else
            return Token.init;
    }

    Token pop()
    {
        if( skipEol )
        {
            Token t;
            unskipEolDo
            ({
                t = pop;
                while( t.type == TOKeol )
                    t = pop;
            });
            return t;
        }

        if( cached > 0 )
        {
            auto r = cache[0];
            foreach( i, ref dst ; cache[0..$-1] )
                dst = cache[i+1];
            -- cached;
            return r;
        }
        else if( next !is null )
        {
            Token token;
            getNext(src, token);

            if( token.type == TOKeos )
                next = null;

            return token;
        }
        else
            err(CEC.PUnexEos, src.loc);
    }

    Token popExpect(TOK type, CEC code = CEC.Unknown)
    {
        auto actual = pop();
        if( actual.type == type )
            return actual;

        err((code != CEC.Unknown ? code : CEC.PExGot), actual.loc,
                tokToName(type), tokToName(actual.type));
    }

    Token popExpectAny(TOK[] types...)
    {
        auto actual = pop();
        foreach( type ; types )
            if( actual.type == type )
                return actual;

        char[] exp;
        foreach( type ; types )
            exp ~= (exp.length == 0 ? "" : ", ") ~ tokToName(type);

        err(CEC.PExManyGot, actual.loc, exp, tokToName(actual.type));
    }

private:
    enum { BaseCacheSize = 2 }

    Token[] cache;
    size_t cached;

    void getNext(Source src, out Token token)
    {
        do
        {
            auto f = next(src, token);
            if( !f )
                err(CEC.PUnexpected, src.loc, src[0..1]);
        }
        while( token.type == TOKcomment );
    }
}

