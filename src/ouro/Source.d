/**
    Source code class.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.Source;

import Utf = tango.text.convert.Utf;

import ouro.Location;

import tango.io.Stdout;

final class Source
{
    char[] name, src;
    char[][] lines;
    size_t startLine, startCol;
    
    this(char[] name, char[] src)
    {
        reset(name, src);
    }

    /**
    Params:
        line    = natural line number; 1 is the first line.
        col     = natural column number; 1 is the first column;
    */
    this(char[] name, char[] src, uint line, uint col)
    {
        reset(name, src, line, col);
    }

    void reset()
    {
        reset(name, src);
    }

    void reset(char[] name, char[] src)
    {
        this.name = name;
        this.src = src;
        this.lines = null;
        this.startLine = 1;
        this.startCol = 1;
        this.mark = Mark.init;
    }

    void reset(char[] name, char[] src, uint line, uint col)
    {
        // TODO: enforce
        assert( line >= 1 );
        assert( col >= 1 );

        reset(name, src);
        this.startLine = line;
        this.startCol = col;
        this.mark.line = line;
        this.mark.column = col;
    }

    Source dup()
    {
        auto r = new Source(name, src);
        r.mark = this.mark;
        return r;
    }

    char[] line(size_t n)
    {
        auto ind = n - startLine;
        if( ind == lines.length )
        {
            auto m = save;
            while( length > 0 && ind >= lines.length )
                advance;
            restore(m);
        }

        return lines[n - startLine];
    }

    Location loc()
    {
        Location r;
        r.file = name;
        r.line = mark.line;
        r.column = mark.column;
        return r;
    }

    Location locSpan(size_t n)
    {
        Location r;
        r.file = name;
        r.line = mark.line;
        r.column = mark.column;
        r.length = n;
        return r;
    }

    Location locSpanFrom(Mark mark)
    {
        auto r = this.locFrom(mark);
        r.extendTo(this.loc);
        return r;
    }

    dchar get(size_t i)
    {
        dchar r;
        auto src = this.src[mark.offset..$];

        if( src.length > 0 )
            do
            {
                uint ate;
                r = Utf.decode(src, ate);
                src = src[ate..$];
            }
            while( i --> 0 && src.length > 0 );

        // +1 to undo the last i--; consider the case of i=0
        if( (i+1) > 0 )
            return dchar.init;

        return r;
    }

    alias get opIndex;

    char[] slice(size_t i, size_t j)
    {
        auto m = this.save;
        scope(exit) this.restore(m);

        this.advance(i);
        return this.slice(j-i);
    }

    char[] slice(size_t n)
    {
        size_t len = 0;
        auto src = this.src[mark.offset..$];
        while( n --> 0 && src.length > 0 )
        {
            uint ate;
            Utf.decode(src, ate);
            src = src[ate..$];
            len += ate;
        }
        return this.src[mark.offset..mark.offset + len];
    }

    alias slice opSlice;

    char[] sliceFrom(Mark mark)
    {
        return this.src[mark.offset..this.mark.offset];
    }

    char[] sliceBetween(Mark start, Mark end)
    {
        return this.src[start.offset..end.offset];
    }

    size_t length()
    {
        return src[mark.offset..$].length;
    }

    char advanceCu()
    {
        return cast(char) advanceCp;
    }

    dchar advanceCp()
    {
        dchar cp;
        advance(1, cp);
        return cp;
    }

    char[] advanceLine()
    {
        char[2] nlBuffer;
        char[] nl = nlBuffer[];
        char[] line;
        advanceLine(line, nl);
        return line;
    }

    bool advanceLine(out char[] line, ref char[] nl)
    {
        // Return null for an empty source.
        if( this.length == 0 )
        {
            line = null;
            return false;
        }

        // Handle empty lines
        auto cp0 = this[0];
        if( cp0 == '\r' )
        {
            if( this[1] == '\n' )
                nl = nl[0..2] = src[0..2];
            else
                nl = nl[0..1] = src[0..1];
            return true;
        }
        else if( cp0 == '\n' )
        {
            nl = nl[0..1] = src[0..1];
            return true;
        }

        // Find the eol
        auto start = this.save;
        while( this.length > 0 && this[0] != '\r' && this[0] != '\n' )
            advance;

        // We're either at eol or eos.
        line = sliceFrom(start);
        if( this.length == 0 )
        {
            nl = null;
        }
        else if( this[0] == '\r' )
        {
            if( this[1] == '\n' )
                nl = nl[0..2] = advance(2)[];
            else
                nl = nl[0..1] = advance(1)[];
        }
        else if( this[0] == '\n' )
            nl = nl[0..1] = advance(1)[];

        return true;
    }

    char[] advance()
    {
        return advance(1);
    }
    
    char[] advance(size_t n)
    {
        dchar cp;
        return advance(n, cp);
    }

    char[] advance(size_t n, out dchar lastCp)
    {
        size_t lineInc = 0;
        auto col = mark.column;
        auto cr = mark.hangingCR;

        auto src = this.src[mark.offset..$];
        size_t bytes = 0; // actual bytes consumed

        void endOfLine()
        {
            auto lineNum = (mark.line + lineInc) - 1;
            if( lineNum < lines.length )
                return;

            auto line = this.src[mark.lineOffset..mark.offset+bytes];
            mark.lineOffset = mark.offset+bytes;
            lines ~= line;
        }

        while( n --> 0 && src.length > 0 )
        {
            uint ate;
            auto cp = Utf.decode(src, ate);
            lastCp = cp;
            src = src[ate..$];
            bytes += ate;

            switch( cp )
            {
                case '\r':
                    cr = true;
                    ++ lineInc;
                    col = 1;
                    break;

                case '\n':
                    endOfLine;
                    if( cr )
                        cr = false;
                    else
                    {
                        ++ lineInc;
                        col = 1;
                    }
                    break;

                default:
                    if( cr )
                        endOfLine;
                    cr = false;
                    ++ col;
            }
        }

        if( src.length == 0 /*&& mark.offset+bytes != mark.lineStart*/ )
        {
            assert( src.ptr !is null );
            endOfLine;
        }

        auto slice = this.src[mark.offset..mark.offset + bytes];

        mark.offset += bytes;
        mark.line += lineInc;
        mark.column = col;
        mark.hangingCR = cr;

        return slice;
    }
    
    struct Mark
    {
    private:
        size_t offset, lineOffset;
        uint line = 1,
             column = 1;
        bool hangingCR = false;
    }

    Mark save()
    {
        return mark;
    }

    void restore(Mark mark)
    {
        this.mark = mark;
    }

    Location locFrom(Mark mark)
    {
        Location r;
        r.file = name;
        r.line = mark.line;
        r.column = mark.column;
        return r;
    }

private:
    Mark mark;
}

version( Unittest )
{
    import tango.text.convert.Format;

    unittest
    {
        // Test newline handling of advance
        {
            scope src = new Source(__FILE__,"\r\n\n\r \n\r\r\n");
            auto start = src.save;

            void next() { src.advance(1); }

            bool locis(uint l, uint c)
            {
                auto loc = src.loc;
                return (loc.line == l) && (loc.column == c);
            }

            char[] les() /* location error string */
            {
                return "got: "~src.loc.toString;
            }

            bool hcr() { return src.mark.hangingCR; }

            src.restore(start);
                    assert( locis(1,1), les );
            next;   assert( locis(2,1), les ); assert( hcr );
            next;   assert( locis(2,1), les ); assert( !hcr );
            next;   assert( locis(3,1), les ); assert( !hcr );
            next;   assert( locis(4,1), les ); assert( hcr );
            next;   assert( locis(4,2), les ); assert( !hcr );
            next;   assert( locis(5,1), les ); assert( !hcr );
            next;   assert( locis(6,1), les ); assert( hcr );
            next;   assert( locis(7,1), les ); assert( hcr );
            next;   assert( locis(7,1), les ); assert( !hcr );
            next;   assert( src.length == 0, Format("got: {}",src.length) );

            src.restore(start);
            src.advance(10);
                    assert( locis(7,1) ); assert( !hcr );
        }
    }
}

