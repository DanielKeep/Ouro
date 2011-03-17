/**
    I/O related builtins.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.Io;

private:

import tango.io.Console;
import tango.io.device.File;
import tango.io.model.IConduit;

import ouro.sem.builtins.Util; // many things

bool forceWrite(OutputStream outs, char[] v)
{
    size_t l;

    while( v.length > 0 )
    {
        l = outs.write(v);
        if( l is IOStream.Eof )
            return false;
        v = v[l..$];
    }

    return true;
}

Value eof, nil;

static this()
{
    eof = new Sit.SymbolValue(null, "eof");
    nil = Sit.NilValue.instance;
}

Value eofOrNil(bool eof)
{
    return eof ? .eof : .nil;
}

IOStream chkArgStream(Value[] args, size_t i)
{
    auto obj = chkArgObject(args, i, /*silent*/true);
    auto stream = cast(IOStream) obj;
    if( stream is null )
        assert( false, "expected stream; got " ~ obj.classinfo.name );
    return stream;
}

InputStream chkArgInStream(Value[] args, size_t i)
{
    auto obj = chkArgObject(args, i, /*silent*/true);
    auto stream = cast(InputStream) obj;
    if( stream is null )
        assert( false, "expected input stream; got " ~ obj.classinfo.name );
    return stream;
}

OutputStream chkArgOutStream(Value[] args, size_t i)
{
    auto obj = chkArgObject(args, i, /*silent*/true);
    auto stream = cast(OutputStream) obj;
    if( stream is null )
        assert( false, "expected output stream; got " ~ obj.classinfo.name );
    return stream;
}

static this() { Builtins.register("ouro.io.closeStream!",
        new Sit.FunctionValue("ouro.io.closeStream!",
            [Sit.Argument("stream!")],
            &io_closeStream,
            EC.Runtime, false)); }

Value io_closeStream(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto stream = chkArgStream(args, 0);

    stream.close();
    return Sit.NilValue.instance;
}

static this() { Builtins.register("ouro.io.copyIntoStream!",
        new Sit.FunctionValue("ouro.io.copyIntoStream!",
            [Sit.Argument("stream_dst!"),
             Sit.Argument("stream_src!")],
            &io_copyIntoStream,
            EC.Runtime, false)); }

Value io_copyIntoStream(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto stream_dst = chkArgOutStream(args, 0);
    auto stream_src = chkArgInStream(args, 1);

    stream_dst.copy(stream_src);
    return Sit.NilValue.instance;
}

static this() { Builtins.register("ouro.io.flushStream!",
        new Sit.FunctionValue("ouro.io.flushStream!",
            [Sit.Argument("stream!")],
            &io_flushStream,
            EC.Runtime, false)); }

Value io_flushStream(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto stream = chkArgStream(args, 0);

    stream.flush();
    return nil;
}

static this() { Builtins.register("ouro.io.hasCursor?",
        new Sit.FunctionValue("ouro.io.hasCursor?",
            [Sit.Argument("stream")],
            &io_hasCursor,
            EC.Runtime, false)); }

Value io_hasCursor(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto stream = chkArgStream(args, 0);

    return Sit.LogicalValue.instance((cast(IConduit.Seek) stream) !is null);
}

static this() { Builtins.register("ouro.io.openFile~",
        new Sit.FunctionValue("ouro.io.openFile~",
            [Sit.Argument("path"),
             Sit.Argument("flags", true)],
            &io_openFile,
            EC.Runtime, false)); }

Value io_openFile(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto path = chkArgString(args, 0);
    auto flags = chkArgList(args, 1).elemValues;

    // Unpack list of flags
    if( flags.length == 1 )
        if( auto l = cast(Sit.ListValue) flags[0] )
            flags = l.elemValues;

    // Process flags
    alias File.Style    Style;
    alias File.Access   Access;
    alias File.Open     Open;
    alias File.Share    Share;
    alias File.Cache    Cache;

    Style style;
    
    if( flags.length == 0 )
        style = File.ReadExisting;

    else
    {
        foreach( flagValue ; flags )
        {
            auto flag = cast(Sit.SymbolValue) flagValue;
            assert( flag !is null, "expected symbol, got a "
                    ~flagValue.classinfo.name );

            switch( flag.value )
            {
                // Shorthand
                case "r":
                case "re":      style = File.ReadExisting; break;
                case "res":     style = File.ReadShared; break;
                case "w":
                case "wc":      style = File.WriteCreate; break;
                case "wa":      style = File.WriteAppending; break;
                case "we":      style = File.WriteExisting; break;
                case "rw":      style = File.ReadWriteOpen; break;
                case "rwe":     style = File.ReadWriteExisting; break;

                // Access
                case "read":    style.access = Access.Read; break;
                case "write":   style.access = Access.Write; break;
                case "readWrite": style.access = Access.ReadWrite; break;

                // Open
                case "exists":  style.open = Open.Exists; break;
                case "create":  style.open = Open.Create; break;
                case "sedate":  style.open = Open.Sedate; break;
                case "append":  style.open = Open.Append; break;
                case "new":     style.open = Open.New; break;

                // Share
                case "snone":
                case "shareNone":
                                style.share = Share.None; break;
                case "sread":
                case "shareRead":
                                style.share = Share.Read; break;
                case "swrite":
                case "shareWrite":
                case "sall":
                case "shareAll":
                                style.share = Share.ReadWrite; break;

                // Cache
                case "noOpt":   style.cache = Cache.None; break;
                case "random":  style.cache = Cache.Random; break;
                case "stream":  style.cache = Cache.Stream; break;
                case "thru":
                case "writeThru":
                                style.cache = Cache.WriteThru; break;

                default:
                    assert( false, "unknown file flag: "~flag.value );
            }
        }
    }

    // Do the open!
    return new Sit.HostObjectValue(new File(path, style));
}

static this() { Builtins.register("ouro.io.openStdStream~",
        new Sit.FunctionValue("ouro.io.openStdStream~",
            [Sit.Argument("id")],
            &io_openStdStream,
            EC.Runtime, false)); }

Value io_openStdStream(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto id = chkArgSymbol(args, 0).value;

    IOStream stream;

    switch( id )
    {
        case "STDIN":   stream = Cin.stream; break;
        case "STDOUT":  stream = Cout.stream; break;
        case "STDERR":  stream = Cerr.stream; break;
        default:
    }

    if( stream is null )
        assert( false, "unknown standard stream: "~id );

    return new Sit.HostObjectValue(cast(Object) stream);
}

static this() { Builtins.register("ouro.io.seekStream!",
        new Sit.FunctionValue("ouro.io.seekStream!",
            [Sit.Argument("stream!"),
             Sit.Argument("anchor"),
             Sit.Argument("offset")],
            &io_seekStream,
            EC.Runtime, false)); }

Value io_seekStream(EC ec, Value[] args)
{
    chkArgNum(args, 3);
    auto stream = chkArgStream(args, 0);
    auto anchorSym = chkArgSymbol(args, 1);
    auto offset = cast(long) chkArgNumber(args, 2);

    alias IOStream.Anchor Anchor;
    Anchor anchor;

    switch( anchorSym.value )
    {
        case "<":
        case "b":
        case "begin":   anchor = Anchor.Begin; break;
        case "|":
        case "c":
        case "current": anchor = Anchor.Current; break;
        case ">":
        case "e":
        case "end":     anchor = Anchor.End; break;
        
        default:
            assert( false, "invalid seek anchor: "~anchorSym.value );
    }

    stream.seek(offset, anchor);
    return Sit.NilValue.instance;
}

static this() { Builtins.register("ouro.io.writeToStream!",
        new Sit.FunctionValue("ouro.io.writeToStream!",
            [Sit.Argument("stream!"),
             Sit.Argument("values", true)],
            &io_writeToStream,
            EC.Runtime, false)); }

Value io_writeToStream(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto outs = chkArgOutStream(args, 0);
    auto values = chkArgList(args, 1).elemValues;

    foreach( value ; values )
    {
        if( auto v = cast(Sit.StringValue) value )
            if( forceWrite(outs, v.value) )
                return eof;
        
        else if( auto v = cast(Sit.CallableValue) value )
        {
            auto fn = v.trueFn;
            if( fn.args.length == 0 )
            {
                auto fnr = invoke(v, null, ec);
                auto r = io_writeToStream(ec, (&fnr)[0..1]);
                if( cast(Sit.NilValue) r is null )
                    return r;
            }
            else if( fn.args.length == 1 )
            {
                auto r = invoke(v, args[0..1], ec);
                if( cast(Sit.NilValue) r is null )
                    return r;
            }
            else
                assert( false, "expected function of either one or zero "
                        "arguments" );
        }
        else
            assert( false, "expected string or function; got a "
                    ~ value.classinfo.name );
    }

    return nil;
}

