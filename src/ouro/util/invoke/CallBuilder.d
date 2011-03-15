/**
    Interface for building a function call for a specific calling convention.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.invoke.CallBuilder;

import ouro.util.invoke.CallConv : CallConv;
import ouro.util.invoke.Type : Type, Variadic;

interface CallBuilder
{
    alias CallBuilder function() CreateFn;
    alias void delegate() Block;

    CallConv callConv();
    char[] mangle(char[] name, Type, Type[], Variadic);
    void[] doCall(void* fn, void[] res, Type, Type[], Variadic, Block);
    void pushArg(Type ty, void[] value);
}

