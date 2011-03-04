/**
    Tokens.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.lexer.Tokens;

import tango.text.convert.Format;

import ouro.Location;

typedef size_t TokenType;
alias TokenType TOK;

const char[][2][] SymbolTokens =
[
    ["="[], "eq"[]],
    ["(", "lparen"],
    [")", "rparen"],
    ["[", "lbracket"],
    ["]", "rbracket"],
    ["{", "lbrace"],
    ["}", "rbrace"],
    ["(.)", "lparenperiodrparen"],
    ["(.", "lparenperiod"],
    [".)", "periodrparen"],
    ["[:", "lbracketcolon"],
    [":]", "colonrbracket"],
    [",", "comma"],
    ["-", "hyphen"],
    ["\\", "bslash"],
    ["!=", "noteq"],
    ["//", "slash2"],
    ["/", "slash"],
    ["**", "star2"],
    ["*", "star"],
    ["<=", "lteq"],
    ["<>", "ltgt"],
    ["<", "lt"],
    [">=", "gteq"],
    [">", "gt"],
    ["::", "colon2"],
    [":", "colon"],
    ["++", "plus2"],
    ["+", "plus"],
    ["...", "period3"],
    [".", "period"],
    ["#'", "hashquote"],
    ["#\"", "hashdquote"],
    ["#$", "hashdollar"],
];

const char[][] LiteralTokens =
[
    "and",
    "let",
    "not",
    "or",
    "mod",
    "rem",
    "true",
    "false",
    "nil",
    "import",
    "macro",
    "range",
];

const char[][] OtherTokens =
[
    "eol",
    "eos",
    "comment",
    "identifier",
    "number",
    "string",
    "builtin",
];

/*
    Generates the following:

    TOKxxx constants (including TOKnone)
    tokToNameMap : char[][TOK]
    tokToName(TOK) : char[]
    nameToTokMap : TOK[char[]]
    nameToTok(char[]) : TOK
*/

private import ouro.util.TokensCtfe : generateTokens_ctfe;
mixin(generateTokens_ctfe(SymbolTokens, LiteralTokens, OtherTokens));

struct Token
{
    Location loc;
    TOK type = TOKnone;
    char[] text;
    char[] value;

    static Token opCall(Location loc, TOK type, char[] text, char[] value)
    {
        alias type a_fun_shooter;

        Token r;
        r.loc = loc;
        r.type = a_fun_shooter;
        r.text = text;
        r.value = value;
        return r;
    }

    char[] toString()
    {
        return Format("<{}@{} ({}), ({})>", tokToName(type), loc, text, value);
    }
}

