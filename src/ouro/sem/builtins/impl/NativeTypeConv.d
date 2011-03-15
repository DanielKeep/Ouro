/**
    Contains code for converting between native types and values.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.impl.NativeTypeConv;

import Sz = tango.stdc.stringz;
import Utf = tango.text.convert.Utf;

import ouro.util.invoke.Type : Type, HandleType;
import Sit = ouro.sit.Nodes;

alias Sit.Value Value;

class Handle
{
    HandleType type;
    void* handle;

    this(HandleType type, void* handle)
    {
        this.type = type;
        this.handle = handle;
    }
}

private void[] copyValue(T)(T v, void[] buffer)
{
    if( buffer.length < T.sizeof )
        buffer = new ubyte[T.sizeof];
    (cast(ubyte[]) buffer)[0..T.sizeof] = cast(ubyte[]) (&v)[0..1];
    return buffer[0..T.sizeof];
}

void[] valueToNative(Value value, Type ty, void[] buffer = null)
{
    alias Type.Id Id;

    switch( ty.id )
    {
        case Id.Void:
            assert( cast(Sit.NilValue) value !is null );
            return null;

        case Id.Word:
            auto v = cast(Sit.NumberValue) value;
            assert( v !is null );
            return copyValue(cast(size_t) v.value, buffer);

        case Id.Handle:
            if( null !is cast(Sit.NilValue) value )
                return copyValue(cast(void*) null, buffer);
            auto hov = cast(Sit.HostObjectValue) value;
            assert( hov !is null, "expected "~ty.toString~"; got "
                    ~ value.classinfo.name );
            auto v = cast(Handle) hov.obj;
            assert( v !is null );
            assert( v.type == ty );
            return copyValue(v.handle, buffer);

        case Id.Bool:
            auto v = cast(Sit.LogicalValue) value;
            assert( v !is null );
            return copyValue(cast(ubyte) v.value ? 1 : 0, buffer);

        case Id.Char:
            auto v = cast(Sit.NumberValue) value;
            assert( v !is null );
            switch( ty.size )
            {
                case 1: return copyValue(cast(ubyte) v.value, buffer);
                case 2: return copyValue(cast(ushort)v.value, buffer);
                case 4: return copyValue(cast(uint)  v.value, buffer);
                default: assert( false );
            }

        case Id.SInt:
            auto v = cast(Sit.NumberValue) value;
            assert( v !is null );
            switch( ty.size )
            {
                case 1: return copyValue(cast(byte) v.value, buffer);
                case 2: return copyValue(cast(short)v.value, buffer);
                case 4: return copyValue(cast(int)  v.value, buffer);
                case 8: return copyValue(cast(long) v.value, buffer);
                default: assert( false );
            }

        case Id.UInt:
            auto v = cast(Sit.NumberValue) value;
            assert( v !is null );
            switch( ty.size )
            {
                case 1: return copyValue(cast(ubyte) v.value, buffer);
                case 2: return copyValue(cast(ushort)v.value, buffer);
                case 4: return copyValue(cast(uint)  v.value, buffer);
                case 8: return copyValue(cast(ulong) v.value, buffer);
                default: assert( false );
            }

        case Id.Float:
            auto v = cast(Sit.NumberValue) value;
            assert( v !is null );
            switch( ty.size )
            {
                case 4: return copyValue(cast(float) v.value, buffer);
                case 8: return copyValue(cast(double)v.value, buffer);
                default: assert( false );
            }

        case Id.Pointer:
            assert( false, "pointers nyi" );

        case Id.Array:
            assert( false, "arrays nyi" );

        case Id.ZeroTerm:
            switch( ty.subType.id )
            {
                case Id.Char:
                    if( auto v = cast(Sit.NilValue) value )
                        return copyValue(cast(void*) null, buffer);

                    auto v = cast(Sit.StringValue) value;
                    switch( ty.subType.size )
                    {
                        case 1: return copyValue(Sz.toStringz(v.value), buffer);

                        case 2:
                            auto s16 = Utf.toString16(v.value);
                            return copyValue(Sz.toString16z(s16), buffer);

                        case 4:
                            auto s32 = Utf.toString32(v.value);
                            return copyValue(Sz.toString32z(s32), buffer);

                        default: assert( false );
                    }

                default:
                    assert( false );
            }

        default:
            assert( false );
    }
}

T exValue(T)(void[] v)
{
    assert( v.length == T.sizeof );
    return *cast(T*) v.ptr;
}

Value nativeToValue(void[] v, Type ty)
{
    alias Type.Id Id;

    switch( ty.id )
    {
        case Id.Void:
            return Sit.NilValue.instance;

        case Id.Word:
            return new Sit.NumberValue(null, exValue!(size_t)(v));

        case Id.Handle:
            return new Sit.HostObjectValue(
                new Handle(cast(HandleType) ty, exValue!(void*)(v)));

        case Id.Bool:
            return new Sit.LogicalValue(null, exValue!(ubyte)(v) != 0);

        case Id.Char:
            switch( ty.size )
            {
                case 1: return new Sit.NumberValue(null, exValue!(ubyte)(v));
                case 2: return new Sit.NumberValue(null, exValue!(ushort)(v));
                case 4: return new Sit.NumberValue(null, exValue!(uint)(v));
                default: assert( false );
            }

        case Id.SInt:
            switch( ty.size )
            {
                case 1: return new Sit.NumberValue(null, exValue!(byte)(v));
                case 2: return new Sit.NumberValue(null, exValue!(short)(v));
                case 4: return new Sit.NumberValue(null, exValue!(int)(v));
                case 8: return new Sit.NumberValue(null, exValue!(long)(v));
                default: assert( false );
            }

        case Id.UInt:
            switch( ty.size )
            {
                case 1: return new Sit.NumberValue(null, exValue!(ubyte)(v));
                case 2: return new Sit.NumberValue(null, exValue!(ushort)(v));
                case 4: return new Sit.NumberValue(null, exValue!(uint)(v));
                case 8: return new Sit.NumberValue(null, exValue!(ulong)(v));
                default: assert( false );
            }

        case Id.Float:
            switch( ty.size )
            {
                case 4: return new Sit.NumberValue(null, exValue!(float)(v));
                case 8: return new Sit.NumberValue(null, exValue!(double)(v));
                default: assert( false );
            }

        case Id.Pointer:
            assert( false, "pointers nyi" );

        case Id.Array:
            assert( false, "arrays nyi" );

        case Id.ZeroTerm:
            switch( ty.subType.id )
            {
                case Id.Char:
                    if( exValue!(void*)(v) is null )
                        return Sit.NilValue.instance;

                    switch( ty.subType.size )
                    {
                        case 1: return new Sit.StringValue(null,
                                        Sz.fromStringz(exValue!(char*)(v)));

                        case 2:
                            auto s16 = Sz.fromString16z(exValue!(wchar*)(v));
                            return new Sit.StringValue(null, Utf.toString(s16));

                        case 4:
                            auto s32 = Sz.fromString32z(exValue!(dchar*)(v));
                            return new Sit.StringValue(null, Utf.toString(s32));

                        default:
                            assert( false );
                    }

                default:
                    assert( false );
            }

        default:
            assert( false );
    }
}

