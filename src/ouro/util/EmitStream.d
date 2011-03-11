/**
    Wraps an emit delegate in an OutputStream interface.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.EmitStream;

import tango.io.device.Conduit;

class EmitStream : OutputStream
{
    void delegate(char[]) emit;

    this(void delegate(char[]) emit)
    {
        this.emit = emit;
    }

    long seek(long offset, Anchor anchor = Anchor.Begin)
    {
        assert( false, "seek not supported" );
    }

    IConduit conduit()
    {
        return null;
    }

    IOStream flush()
    {
        return this;
    }

    void close()
    {
    }

    override size_t write(void[] src)
    {
        emit( cast(char[]) src );
        return src.length;
    }

    OutputStream copy(InputStream src, size_t max = -1)
    {
        Conduit.transfer(src, this, max);
        return this;
    }

    OutputStream output()
    {
        return this;
    }
}

