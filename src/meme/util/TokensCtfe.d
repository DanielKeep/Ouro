/**
    Token support CTFE.

    Used to generate the TOKx constants and conversion functions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.util.TokensCtfe;

char[] generateTokens_ctfe
(
    char[][2][] symbolTokens,
    char[][] literalTokens,
    char[][] otherTokens,
)
{
    char[] r = "";

    // Token enums
    r ~= lineSpec_ctfe(__LINE__) ~ "
        enum : TOK {
          TOKnone,
    ";
    foreach( pair ; symbolTokens )
        r ~= lineSpec_ctfe(__LINE__)~"TOK" ~ pair[1] ~ ",\n";
    foreach( name ; literalTokens )
        r ~= lineSpec_ctfe(__LINE__)~"TOK" ~ name ~ ",\n";
    foreach( name ; otherTokens )
        r ~= lineSpec_ctfe(__LINE__)~"TOK" ~ name ~ ",\n";
    r ~= "}\n";

    // TOK -> string
    r ~= lineSpec_ctfe(__LINE__) ~ "
        char[][TOK] tokToNameMap;
        
        static this()
        {
            alias tokToNameMap m;
    ";
    foreach( pair ; symbolTokens )
        r ~= lineSpec_ctfe(__LINE__)~"m[TOK" ~ pair[1] ~ "] = \"" ~ pair[1] ~ "\";\n";
    foreach( name ; literalTokens )
        r ~= lineSpec_ctfe(__LINE__)~"m[TOK" ~ name ~ "] = \"" ~ name ~ "\";\n";
    foreach( name ; otherTokens )
        r ~= lineSpec_ctfe(__LINE__)~"m[TOK" ~ name ~ "] = \"" ~ name ~ "\";\n";
    
    r ~= lineSpec_ctfe(__LINE__)~"
        m.rehash;
    }
    ";

    r ~= lineSpec_ctfe(__LINE__)~"
        char[] tokToName(TOK tok)
        {
            switch(tok) {
    ";
    foreach( pair ; symbolTokens )
        r ~= lineSpec_ctfe(__LINE__)~"case TOK" ~ pair[1] ~ ": return \"" ~ pair[1] ~ "\";\n";
    foreach( name ; literalTokens )
        r ~= lineSpec_ctfe(__LINE__)~"case TOK" ~ name ~ ": return \"" ~ name ~ "\";\n";
    foreach( name ; otherTokens )
        r ~= lineSpec_ctfe(__LINE__)~"case TOK" ~ name ~ ": return \"" ~ name ~ "\";\n";

    r ~= lineSpec_ctfe(__LINE__)~"
        default: return \"\";
        }
    }
    ";

    // string -> TOK
    r ~= lineSpec_ctfe(__LINE__)~"
        TOK[char[]] nameToTokMap;
        static this()
        {
            alias nameToTokMap m;
    ";
    foreach( pair ; symbolTokens )
        r ~= lineSpec_ctfe(__LINE__)~"m[\"" ~ pair[1] ~ "\"] = TOK" ~ pair[1] ~ ";\n";
    foreach( name ; literalTokens )
        r ~= lineSpec_ctfe(__LINE__)~"m[\"" ~ name ~ "\"] = TOK" ~ name ~ ";\n";
    foreach( name ; otherTokens )
        r ~= lineSpec_ctfe(__LINE__)~"m[\"" ~ name ~ "\"] = TOK" ~ name ~ ";\n";

    r ~= lineSpec_ctfe(__LINE__)~"
        m.rehash;
    }
    ";

    r ~= lineSpec_ctfe(__LINE__)~"
        TOK nameToTok(char[] name)
        {
            switch(name)
            {
    ";
    foreach( pair ; symbolTokens )
        r ~= lineSpec_ctfe(__LINE__)~"case \"" ~ pair[1] ~ "\": return TOK" ~ pair[1] ~ ";\n";
    foreach( name ; literalTokens )
        r ~= lineSpec_ctfe(__LINE__)~"case \"" ~ name ~ "\": return TOK" ~ name ~ ";\n";
    foreach( name ; otherTokens )
        r ~= lineSpec_ctfe(__LINE__)~"case \"" ~ name ~ "\": return TOK" ~ name ~ ";\n";

    r ~= lineSpec_ctfe(__LINE__)~"
        default: return TOKnone;
        }
    }
    ";

    return r;
}

private:

char[] lineSpec_ctfe(long line)
{
    char[] r = "#line "~format_ctfe(line)~" \""~__FILE__~"\"\n";
    return r;
}

/**
 * Formats an integer as a string.  You can optionally specify a different
 * base; any value between 2 and 16 inclusive is supported.
 * 
 * Params:
 *     v = value to format.
 *     base = base to use; defaults to 10.
 * Returns:
 *      integer formatted as a string.
 */

char[] format_ctfe(intT)(intT v, int base = 10)
{
    static if( !is( intT == ulong ) ) 
    {
        return (v < 0)
            ? "-" ~ format_ctfe(cast(ulong) -v, base)
            : format_ctfe(cast(ulong) v, base);
    }
    else
    {
        assert( 2 <= base && base <= 16,
                "base must be between 2 and 16; got " ~ format_ctfe(base, 10) );
        
        char[] r = "";
        do
        {
            r = INT_CHARS[v % base] ~ r;
            v /= base;
        }
        while( v > 0 );
        return r;
    }
}

const INT_CHARS = "0123456789abcdef";

