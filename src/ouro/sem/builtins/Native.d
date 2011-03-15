/**
    Builtins for accessing native methods.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.Native;

private:

import tango.stdc.stringz : toStringz;
import tango.sys.SharedLib;

import ouro.sem.builtins.impl.NativeTypeConv : valueToNative, nativeToValue;
import ouro.sem.builtins.Util;
import ouro.util.invoke.CallConv : CallConv,
       callConvToString, callConvFromString;
import ouro.util.invoke.Platform : getCallBuilder, releaseCallBuilder;
import ouro.util.invoke.Type : Type, Variadic;

class NativeFn
{
    void* ptr;
    CallConv callConv;
    Type returnType;
    Type[] argTypes;
    Variadic variadic;

    this(void* ptr, CallConv cc, Type rt, Type[] argTys, Variadic va)
    {
        this.ptr = ptr;
        this.callConv = cc;
        this.returnType = rt;
        this.argTypes = argTys;
        this.variadic = va;
    }
}

SharedLib chkArgSharedLib(Value[] args, size_t i)
{
    auto obj = chkArgObject(args, i, /*silent*/true);
    auto r = cast(SharedLib) obj;
    assert( r !is null, "expected SharedLib; got a "
            ~ args[i].classinfo.name);
    return r;
}

Type chkArgType(Value[] args, size_t i)
{
    if( auto v = cast(Sit.SymbolValue) args[i] )
    {
        switch( v.value )
        {
            case "void":        return Type.t_void;
            case "word":        return Type.t_word;

            case "bool":        return Type.t_bool;

            case "char8":       return Type.t_char8;
            case "char16":      return Type.t_char16;
            case "char32":      return Type.t_char32;

            case "sint8":       return Type.t_sint8;
            case "sint16":      return Type.t_sint16;
            case "sint32":      return Type.t_sint32;
            case "sint64":      return Type.t_sint64;

            case "uint8":       return Type.t_uint8;
            case "uint16":      return Type.t_uint16;
            case "uint32":      return Type.t_uint32;
            case "uint64":      return Type.t_uint64;

            case "float32":     return Type.t_float32;
            case "float64":     return Type.t_float64;

            case "void_p":      return Type.t_void_p;
            case "sz":          return Type.t_sz;

            default:
                assert( false, "unknown Native Type "~v.value );
        }
    }
    else
    {
        auto obj = chkArgObject(args, i, /*silent*/true);
        auto r = cast(Type) obj;
        assert( r !is null, "expected Native Type; got a "
                ~ args[i].classinfo.name);
        return r;
    }
}

static this()
{
    Builtins.register("ouro.native.loadLibrary",
            new Sit.FunctionValue("ouro.native.loadLibrary", [
                    Sit.Argument("path")
                ], &native_loadLibrary, EC.Runtime));
}

Value native_loadLibrary(EC ec, Value[] args)
{
    chkArgNum(args, 1);
    auto path = chkArgString(args, 0);

    auto lib = SharedLib.loadNoThrow(path);
    if( lib is null )
        return Sit.NilValue.instance;
    else
        return new Sit.HostObjectValue(lib);
}

static this()
{
    Builtins.register("ouro.native.loadSymbol",
            new Sit.FunctionValue("ouro.native.loadSymbol", [
                    Sit.Argument("lib"),
                    Sit.Argument("name"),
                    Sit.Argument("callConv"),
                    Sit.Argument("returnType"),
                    Sit.Argument("argTypes"),
                    Sit.Argument("variadic")
                ], &native_loadSymbol, EC.Runtime));
}

Value native_loadSymbol(EC ec, Value[] args)
{
    // loadSymbol(lib, name, callConv, return, args, variadic)
    chkArgNum(args, 6);
    auto lib = chkArgSharedLib(args, 0);
    auto symName = chkArgStringOrSymbol(args, 1);
    auto callConv = callConvFromString(chkArgSymbol(args, 2).value);
    auto returnTy = chkArgType(args, 3);
    auto argTyExprs = chkArgList(args, 4).elemValues;
    auto variadicBool = chkArgLogical(args, 5);

    auto variadic = (variadicBool ? Variadic.Untyped : Variadic.None);

    auto argTys = new Type[](argTyExprs.length);
    foreach( i,argTyExpr ; argTyExprs )
        argTys[i] = chkArgType(argTyExprs, i);

    char[20] buffer;

    auto cc = getCallBuilder(callConv);
    if( cc is null )
        return Sit.NilValue.instance;

    // Mangle if name is a symbol.
    char[] mangle;
    if( null !is cast(Sit.StringValue) args[1] )
        mangle = symName;
    else
        mangle = cc.mangle(symName, returnTy, argTys, variadic);

    auto sym = lib.getSymbolNoThrow(toStringz(mangle, buffer));
    if( sym is null )
        return Sit.NilValue.instance;

    auto nfn = new NativeFn(sym, callConv, returnTy, argTys, variadic);
    return new Sit.HostObjectValue(nfn);
}

static this()
{
    Builtins.register("ouro.native.invoke",
            new Sit.FunctionValue("ouro.native.invoke", [
                    Sit.Argument("sym"),
                    Sit.Argument("args")
                ], &native_invoke, EC.Runtime));
}

Value native_invoke(EC ec, Value[] args)
{
    chkArgNum(args, 2);
    auto fn = chkArgObjectT!(NativeFn)(args, 0);
    auto argExprs = chkArgList(args, 1).elemValues;

    auto cc = getCallBuilder(fn.callConv);
    ubyte[20] retBuffer;
    ubyte[20] argBuffer;
    auto ret = cc.doCall
    (
        fn.ptr, retBuffer, fn.returnType, fn.argTypes, fn.variadic,
        {
            foreach( i, argExpr ; argExprs )
                cc.pushArg(fn.argTypes[i],
                    valueToNative(argExpr, fn.argTypes[i], argBuffer));
        }
    );

    return nativeToValue(ret, fn.returnType);
}

