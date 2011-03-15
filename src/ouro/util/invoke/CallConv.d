/**
    Calling conventions.

    Note that this module provides a list of all *possible* calling
    conventions, whether they are supported or not.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.invoke.CallConv;

import ouro.util.EnumCtfe;

mixin(genEnum_ctfe
(
    "CallConv",
    [
        "Cdecl"
    ],
    /*map*/         null,
    /*toString*/    "callConvToString",
    /*fromString*/  "callConvFromString"
));

