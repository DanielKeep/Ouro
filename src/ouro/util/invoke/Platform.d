/**
    Platform imports.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.invoke.Platform;

import ouro.util.invoke.CallBuilder;
import ouro.util.invoke.CallConv : CallConv;

version( X86 )
{
    version( Windows )
    {
        import ouro.util.invoke.X86_Windows.Platform : callConvBuilder;
    }
    else
        version = NoImpl;
}
else
    version = NoImpl;

version( NoImpl )
{
    const HasImpl = false;
}
else
    const HasImpl = true;

CallBuilder getCallBuilder(CallConv cc)
{
    CallBuilder r;
    if( auto cbp = cc in cache )
    {
        r = *cbp;
        *cbp = null;
    }
    if( r is null )
        r = callConvBuilder(cc)();

    return r;
}

void releaseCallBuilder(CallBuilder cb)
{
    if( !(cb.callConv in cache) )
        cache[cb.callConv] = cb;
}

private:

version( NoImpl )
    CallBuilder.CreateFn callConvBuilder(CallConv cc)
    {
        return null;
    }

CallBuilder[CallConv] cache;

