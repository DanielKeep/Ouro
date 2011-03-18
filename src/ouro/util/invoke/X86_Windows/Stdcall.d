/**
    Implementation of the stdcall calling convention for x86 Windows.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.invoke.X86_Windows.Stdcall;

import Integer = tango.text.convert.Integer;

import ouro.util.invoke.CallBuilder;
import ouro.util.invoke.CallConv : CallConv;
import ouro.util.invoke.Type : Type, Variadic;

class StdcallBuilder : CallBuilder
{
    static CallBuilder create()
    {
        return new StdcallBuilder;
    }

    override CallConv callConv() { return CallConv.Stdcall; }

    override char[] mangle(char[] name, Type rt, Type[] args, Variadic va)
    {
        if( va != Variadic.None )
            assert( false, "stdcall does not allow variadic functions" );

        size_t paramSize = 0;

        foreach( arg ; args )
            paramSize += arg.size;

        return "_" ~ name ~ "@" ~ Integer.toString(paramSize);
    }

    override void[] doCall(void* fp, void[] res,
            Type rt, Type[] args, Variadic va,
            Block block)
    {
        reset(rt, args, va);
        block();
        void[] ret;
        {
            uint eax, edx;
            double st0;
            double* st0p = null;

            if( rt.id == Type.Id.Float )
                st0p = &st0;

            auto sp = stack.ptr;
            auto ss = stackSize;

            invokeStdcall(fp, ss, sp, &eax, &edx, st0p);

            auto rts = rt.size;
            if( res.length < rts )
                res = new ubyte[rts];
            auto resb = (cast(ubyte[]) res)[0..rts];

            if( rt.id == Type.Id.Void )
            {
                // no op
            }
            else if( retViaSt0(rt.id) )
            {
                if( rts == 4 )
                {
                    float v = st0;
                    resb[] = cast(ubyte[]) (&v)[0..1];
                }
                else
                {
                    assert( rts == 8 );
                    resb[] = cast(ubyte[]) (&st0)[0..1];
                }
            }
            else if( retViaEaxEdx(rt.id) )
            {
                auto w = rts;
                if( w <= 4 )
                {
                    resb[] = (cast(ubyte[]) (&eax)[0..1])[0..w];
                }
                else
                {
                    assert( w <= 8 );
                    resb[0..4] = (cast(ubyte[]) (&eax)[0..1]);
                    resb[4..w] = (cast(ubyte[]) (&edx)[0..1])[0..w-4];
                }
            }
            else
                assert( false, "return of "~rt.toString~" nyi" );

            ret = resb;
        }

        return ret;
    }

    override void pushArg(Type ty, void[] value)
    {
        assert( value.length == ty.size );
        push( cast(ubyte[]) value );
    }

private:
    ubyte[] stack;
    size_t stackSize;
    Type retType;
    Type[] argTypes;

    const ubyte[3] padding = [0,0,0];

    void reset(Type rt, Type[] args, Variadic va)
    {
        if( va != Variadic.None )
            assert( false, "stdcall does not allow for variadic functions" );

        stack = null;
        stackSize = 0;
        this.retType = rt;
        this.argTypes = args;

        // Compute size of of stack needed
        size_t ss = 0;
        assert( rt.size <= 8, "large return types nyi" );
        foreach( arg ; args )
            ss += alignSize(arg.size);

        stack = new ubyte[ss];
    }

    void push(ubyte[] data)
    {
        pushBytes(data);

        // Keep stack aligned to 4 bytes
        auto rem = data.length % 4;
        if( rem != 0 )
            pushBytes(padding[0..4-rem]);

        assert( stack.length % 4 == 0 );
    }

    void pushBytes(ubyte[] data)
    {
        if( data.length <= (stack.length - stackSize) )
        {
            auto stail = stack[stackSize..$];
            stail[0..data.length] = data[];
            stackSize += data.length;
        }
        else
        {
            stack = stack[0..stackSize] ~ data;
            stackSize += data.length;
        }
    }
}

private:

bool retViaSt0(Type.Id id)
{
    return id == Type.Id.Float;
}

bool retViaEaxEdx(Type.Id id)
{
    switch( id )
    {
        case Type.Id.Word:
        case Type.Id.Handle:
        case Type.Id.Bool:
        case Type.Id.Char:
        case Type.Id.SInt:
        case Type.Id.UInt:
        case Type.Id.Pointer:
        case Type.Id.Array:
        case Type.Id.ZeroTerm:
            return true;

        default:
            return false;
    }
}

size_t alignSize(size_t n)
{
    return (n+3) & (~0b11);
}

/**
    Invokes the given function pointer.

    Params:
        fp =    Function pointer.
        ss =    Size of argument stack.
        sp =    Pointer to bottom of argument stack.
        eax =   Pointer to storage for result eax register.
        edx =   Pointer to storage for result edx register.
        st0 =   Pointer to storage for result ST(0) register.
                Note that this argument may be null, in which case ST(0) is
                not popped.
 */
extern(C) void invokeStdcall(void* fp, uint ss, void* sp,
        uint* eax, uint* edx, double* st0)
{
    asm
    {
        naked;

        push    EBP;
        mov     EBP, ESP;

        // Save registers.  We can probably drop some of these.
        //push    EAX;
        push    EBX;
        push    ECX;
        //push    EDX;
        push    ESI;
        push    EDI;

        // The arguments to f(a, b, c) get pushed in reverse order.  However,
        // since the stack grows downwards, they end up in order in memory.
        //
        // We need to reduce ESP by ss to make room for them, then copy them
        // onto the stack from sp.
        mov     ECX, ss;
        sub     ESP, ECX;
        mov     ESI, sp;
        mov     EDI, ESP;
        cld;
loop:   movsb;
        dec     ECX;
        cmp     ECX, 0;
        jne     loop;

        // Ready to make the call.
        mov     EAX, fp;
        call    EAX;

        // No need to clean up the stack for stdcall.  ESP should be back to
        // what it was before we made space for the stack.

        // We need to save EAX and EDX.
        mov     EDI, eax;
        mov     EBX, edx;
        mov     [EDI], EAX;
        mov     [EBX], EDX;

        // Save ST(0) if we have a non-zero pointer.
        mov     EDI, st0;
        cmp     EDI, 0;
        je      noFloat;
        fstp    qword ptr [EDI];

noFloat:
        pop     EDI;
        pop     ESI;
        //pop     EDX;
        pop     ECX;
        pop     EBX;
        //pop     EAX;
        pop     EBP;
        ret;
    }
}

version( TestStdcall ):

import tango.io.Stdout;

extern(Windows) int add(int a, int b)
{
    return a+b;
}

extern(Windows) int addBytes(byte a, byte b)
{
    return a+b;
}

extern(Windows) int sub(int a, int b)
{
    return a-b;
}

extern(Windows) float fsub(float a, float b)
{
    Stdout.formatln("fsub({:x,8}, {:x,8})",
            *cast(size_t*)&a, *cast(size_t*)&b);
    return a-b;
}

int call_addBytes()
{
    return addBytes(1,2);
}

float call_fsub()
{
    return fsub(1.50, 2.20);
}

void main()
{
    call_addBytes;
    Stdout.formatln("fsub(1.5, 2.2) = {}", call_fsub);

    scope stdcall = new StdcallBuilder;
    auto cc = cast(CallBuilder) stdcall;

    auto t_byte = Type.t_sint8;
    auto t_int  = Type.t_sint32;
    auto t_float= Type.t_float32;

    {
        int a = 1, b = 2;
        int r;
        Stdout.formatln("add({}, {})", a, b);
        cc.doCall(&add, (&r)[0..1], t_int, [t_int, t_int], Variadic.None,
            {
                cc.pushArg(t_int, (&a)[0..1]);
                cc.pushArg(t_int, (&b)[0..1]);
            }
        );
        Stdout.formatln(" = {}", r);
    }
    {
        byte a = 1, b = 2;
        int r;
        Stdout.formatln("addBytes({}, {})", a, b);
        cc.doCall(&addBytes, (&r)[0..1], t_int, [t_byte, t_byte], Variadic.None,
            {
                cc.pushArg(t_byte, (&a)[0..1]);
                cc.pushArg(t_byte, (&b)[0..1]);
            }
        );
        Stdout.formatln(" = {}", r);
    }
    {
        int a = 1, b = 2;
        int r;
        Stdout.formatln("sub({}, {})", a, b);
        cc.doCall(&sub, (&r)[0..1], t_int, [t_int, t_int], Variadic.None,
            {
                cc.pushArg(t_int, (&a)[0..1]);
                cc.pushArg(t_int, (&b)[0..1]);
            }
        );
        Stdout.formatln(" = {}", r);
    }
    {
        float a = 1.5, b = 2.2;
        float r;
        Stdout.formatln("a = {:x,8}, b = {:x,8}",
                *cast(size_t*)&a, *cast(size_t*)&b);
        Stdout.formatln("fsub({}, {})", a, b);
        cc.doCall(&fsub, (&r)[0..1], t_float, [t_float, t_float], Variadic.None,
            {
                cc.pushArg(t_float, (&a)[0..1]);
                cc.pushArg(t_float, (&b)[0..1]);
            }
        );
        Stdout.formatln(" = {}", r);
    }
}

