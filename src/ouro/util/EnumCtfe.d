/**
    CTFE code for generating enumerations.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.EnumCtfe;

char[] genEnum_ctfe(char[] name, char[][] members,
        char[] map, char[] toString, char[] fromString)
{
    char[] r;

    r ~= "enum " ~ name ~ " { ";
    foreach( member ; members )
        r ~= member ~ ", ";
    r ~= "}\n";

    if( map != "" )
    {
        r ~= "static const "~map~" = [";
        foreach( member ; members )
            r ~= "\""~member~"\"[],";
        r ~= "];\n";
    }

    if( toString != "" )
    {
        r ~= "static char[] "~toString~"("~name~" v) { switch( v ) { ";
        foreach( member ; members )
            r ~= "case "~name~"."~member~": return \""~member~"\"; ";
        r ~= "default: assert(false); } }\n";
    }

    if( fromString != "" )
    {
        r ~= "static "~name~" "~fromString~"(char[] v) { switch( v ) { ";
        foreach( member ; members )
            r ~= "case \""~member~"\": return "~name~"."~member~"; ";
        r ~= "default: assert(false); } }\n";
    }

    return r;
}

