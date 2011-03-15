/**
    Contains code for converting between native types and values.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.impl.NativeTypeConv;

import ouro.util.invoke.Type : Type;
import Sit = ouro.sit.Nodes;

alias Sit.Value Value;

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
            assert( false, "zeroterm nyi" );

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
            assert( false, "zeroterm nyi" );

        default:
            assert( false );
    }
}

