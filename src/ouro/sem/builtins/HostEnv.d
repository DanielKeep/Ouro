/**
    Host Environment builtins.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.HostEnv;

private:

import ouro.sem.builtins.Util; // many things

template SymbolValue(char[] name)
{
    static this()
    {
        Builtins.register("ouro.hostenv."~name,
                new Sit.SymbolValue(null, mixin(name)));
    }
}

version( X86 )
    const Cpu = "x86";
else version( X86_64 )
    const CPU = "x64";
else
    const Cpu = "other";

version( Windows )
    const OS = "Windows";
else version( linux )
    const OS = "Linux";
else
    const OS = "other";

version( BigEndian )
    const Endianess = "big";
else
    const Endianess = "little";

mixin SymbolValue!("Cpu");
mixin SymbolValue!("OS");
mixin SymbolValue!("Endianess");

