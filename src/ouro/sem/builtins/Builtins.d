/**
    Provides centralised access to builtins.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.Builtins;

import ouro.sit.Nodes : Value;

Value lookup(char[] name)
{
    if( auto v = name in builtins )
        return *v;
    return null;
}

private
{
    Value[char[]] builtins;
}

package
{
    void register(char[] name, Value builtin, bool force = false)
    {
        if( !!(name in builtins) && ! force )
            assert( false, name ~ " already registered as builtin" );
        builtins[name] = builtin;
    }
}

