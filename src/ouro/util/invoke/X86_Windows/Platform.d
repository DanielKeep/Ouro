/**
    Platform imports.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.invoke.X86_Windows.Platform;

import ouro.util.invoke.CallBuilder;
import ouro.util.invoke.CallConv : CallConv;
import ouro.util.invoke.X86_Windows.Cdecl : CdeclBuilder;

CallBuilder.CreateFn callConvBuilder(CallConv cc)
{
    if( auto cfnp = cc in createFns )
        return *cfnp;
    return null;
}

private:

CallBuilder.CreateFn[CallConv] createFns;

static this()
{
    createFns[CallConv.Cdecl] = &CdeclBuilder.create;
}

