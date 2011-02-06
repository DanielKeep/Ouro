/**
    Source Location.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.Location;

import tango.text.convert.Format;

struct Location
{
    char[] file;
    uint line = 0;      /// natural line number; 1 is the first line
    uint column = 0;    /// natural column number; 1 is the first column
    uint length = 0;    /// can be greater than the line's length

    static Location opCall(char[] file, uint line, uint column, uint length)
    {
        Location r;
        r.file = file;
        r.line = line;
        r.column = column;
        r.length = length;
        return r;
    }

    Location extendTo(Location other)
    {
        Location r = *this;
        
        if( this.line == other.line && other.column > this.column )
            this.length = (other.column - this.column) + other.length;

        else if( other.line > this.line )
            this.length = uint.max;

        else
            assert(false, "tried to extend Location backwards: from "
                    ~ this.toString ~ " to " ~ other.toString);

        return r;
    }

    char[] toString()
    {
        return Format("{}({},{})", file, line, column);
    }
}

