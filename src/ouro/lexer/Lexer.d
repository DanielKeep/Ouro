/**
    Lexer.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.lexer.Lexer;

import tango.text.convert.Format;

debug(TraceLexer) import tango.io.Stdout : Stderr;

import ouro.Location : Location;
import ouro.Source : Source;
import ouro.Error : CompilerException;
import ouro.lexer.Tokens; // Token, TOKx, ...
import ouro.util.Parse : parseString;
import ouro.util.TokenTest : isWhitespace, isSpace, isDigit, isLetter,
       isIdentStart, isIdent, isInnerDigit;

private
{
    alias CompilerException.Code CEC;

    void err(CEC code, Location loc, char[] arg0 = null)
    {
        throw new CompilerException(code, loc, arg0);
    }
}

void skipWhitespace(Source src)
{
    while( true )
    {
        auto cp = src[0];

        // newlines don't count as whitespace; they lex as TOKeol
        if( cp == '\r' || cp == '\n' )
            return;

        else if( isWhitespace(cp) )
            src.advance;
        
        else if( isSpace(cp) )
            err(CEC.LUnusWs, src.loc);

        else
            return;
    }
}

/**
    popToken is used to construct a new Token, assign it,
    and advance the Source object.

    It always returns true.
*/
bool popToken(out Token dest, Source src, TOK type, size_t n)
{
    auto text = src.slice(n);
    dest = Token(src.locSpan(n), type, text, text);
    src.advance(n);
    return true;
}

bool popToken(out Token dest, Source src, TOK type, Source.Mark mark)
{
    auto loc = src.locSpanFrom(mark);
    auto text = src.sliceFrom(mark);
    dest = Token(loc, type, text, text);
    return true;
}

bool popToken(out Token dest, Source src, TOK type, Source.Mark mark,
        char[] value)
{
    auto loc = src.locSpanFrom(mark);
    auto text = src.sliceFrom(mark);
    dest = Token(loc, type, text, value);
    return true;
}

bool popToken(out Token dest, Source src, TOK type, Source.Mark mark,
        Source.Mark valBegMark, Source.Mark valEndMark)
{
    auto loc = src.locSpanFrom(mark);
    auto text = src.sliceFrom(mark);
    auto value = src.sliceBetween(valBegMark, valEndMark);
    dest = Token(loc, type, text, value);
    return true;
}

/*
   All lex* functions take a Source and return a flag indicating success and
   (potentially) a Token.  The Token is passed out via an out argument.

   Because a Source is a reference type, lexer functions need to be careful to
   preserve the state of the Source if they don't produce a token.

   If anything goes wrong, the lex function throws a LexerException via the
   err function.
*/

size_t isEol(Source src)
{
    auto cp0 = src[0];
    size_t l = 0;

    if( cp0 == '\r' )
    {
        if( src[1] == '\n' )
            l = 2;
        else
            l = 1;
    }
    else if( cp0 == '\n' )
        l = 1;

    return l;
}

bool lexEol(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexEol @ {})", src.loc.toString);
    auto l = isEol(src);

    if( l == 0 )
        return false;

    return popToken(token, src, TOKeol, l);
}

bool lexEos(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexEos @ {})", src.loc.toString);
    if( src.length != 0 )
        return false;

    return popToken(token, src, TOKeos, 0);
}

bool lexComment(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexComment @ {})", src.loc.toString);
    auto sl3 = src.slice(3);
    auto mark = src.save;

    switch( sl3 )
    {
        case "|--":
            src.advance(3);
            auto valMark = src.save;
            size_t eol = isEol(src);
            while( eol == 0 )
            {
                src.advance(1);
                eol = isEol(src);
            }
            src.advance(eol);
            return popToken(token, src, TOKcomment, mark, valMark, src.save);

        case "(--":
        {
            src.advance(3);
            auto valBegMark = src.save;
            auto valEndMark = src.save;
            size_t depth = 1;
            while( depth > 0 )
            {
                if( src.length < 3 )
                    err(CEC.LUntermBlockCmt, src.locFrom(mark));
                sl3 = src.slice(3);
                switch( sl3 )
                {
                    case "(--":
                        ++ depth;
                        if( depth == 0 )
                            err(CEC.LBlockCmtOvfl, src.locSpan(3));
                        src.advance(3);
                        break;

                    case "--)":
                        -- depth;
                        valEndMark = src.save;
                        src.advance(3);
                        break;

                    default:
                        src.advance(1);
                }
            }

            return popToken(token, src, TOKcomment, mark,
                    valBegMark, valEndMark);
        }
        case "(++":
        {
            src.advance(3);
            auto valBegMark = src.save;
            auto valEndMark = src.save;
            size_t depth = 1;
            while( depth > 0 )
            {
                if( src.length < 3 )
                    err(CEC.LUntermBlockCmt, src.locFrom(mark));
                sl3 = src.slice(3);
                switch( sl3 )
                {
                    case "(++":
                        ++ depth;
                        if( depth == 0 )
                            err(CEC.LBlockCmtOvfl, src.locSpan(3));
                        src.advance(3);
                        break;

                    case "++)":
                        -- depth;
                        valEndMark = src.save;
                        src.advance(3);
                        break;

                    default:
                        src.advance(1);
                }
            }

            return popToken(token, src, TOKcomment, mark,
                    valBegMark, valEndMark);
        }

        default:
            return false;
    }
}

bool lexSymbol(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexSymbol @ {})", src.loc.toString);
    auto cp0_2 = src[0..3];
    dchar cp0, cp1, cp2;
    if( src.length > 0 ) cp0 = cp0_2[0];
    if( src.length > 1 ) cp1 = cp0_2[1];
    if( src.length > 2 ) cp2 = cp0_2[2];

    size_t l = 0;
    TOK tok;

    switch( cp0 )
    {
        // Unique prefix single-cp symbols
        case '=': l = 1; tok = TOKeq; break;
        case ')': l = 1; tok = TOKrparen; break;
        case ']': l = 1; tok = TOKrbracket; break;
        case '{': l = 1; tok = TOKlbrace; break;
        case '}': l = 1; tok = TOKrbrace; break;
        case ',': l = 1; tok = TOKcomma; break;
        case '-': l = 1; tok = TOKhyphen; break;
        case '\\':l = 1; tok = TOKbslash; break;

        // multi-cp symbols
        case '(':
            switch( cp1 )
            {
                case '.':   l = 2; tok = TOKlparenperiod; break;
                default:    l = 1; tok = TOKlparen; break;
            }
            break;

        case '[':
            switch( cp1 )
            {
                case ':':   l = 2; tok = TOKlbracketcolon; break;
                default:    l = 1; tok = TOKlbracket; break;
            }
            break;

        case '!':
            switch( cp1 )
            {
                case '=':   l = 2; tok = TOKnoteq; break;
                default:    {}
            }
            break;

        case '/':
            switch( cp1 )
            {
                case '/':   l = 2; tok = TOKslash2; break;
                default:    l = 1; tok = TOKslash; break;
            }
            break;

        case '*':
            switch( cp1 )
            {
                case '*':   l = 2; tok = TOKstar2; break;
                default:    l = 1; tok = TOKstar; break;
            }
            break;

        case '<':
            switch( cp1 )
            {
                case '=':   l = 2; tok = TOKlteq; break;
                case '>':   l = 2; tok = TOKltgt; break;
                default:    l = 1; tok = TOKlt; break;
            }
            break;

        case '>':
            switch( cp1 )
            {
                case '=':   l = 2; tok = TOKgteq; break;
                default:    l = 1; tok = TOKgt; break;
            }
            break;

        case ':':
            switch( cp1 )
            {
                case ']':   l = 2; tok = TOKcolonrbracket; break;
                case ':':   l = 2; tok = TOKcolon2; break;
                default:    l = 1; tok = TOKcolon; break;
            }
            break;

        case '+':
            switch( cp1 )
            {
                case '+':   l = 2; tok = TOKplus2; break;
                default:    l = 1; tok = TOKplus; break;
            }
            break;

        case '.':
            if( cp1 == '.' && cp2 == '.' )
            {
                l = 3;
                tok = TOKperiod3;
            }
            else if( cp1 == ')' )
            {
                l = 2;
                tok = TOKperiodrparen;
            }
            else
            {
                l = 1;
                tok = TOKperiod;
            }
            break;

        case '#':
            switch( cp1 )
            {
                case '~':
                    switch( cp2 )
                    {
                        case '\'':  l = 3; tok = TOKhashtildequote; break;
                        case '"':   l = 3; tok = TOKhashtildedquote; break;
                        case '$':   l = 3; tok = TOKhashtildedollar; break;
                        default:    {}
                    }
                    break;
                    
                default:
                    {}
            }

        default:
            {}
    }

    if( l == 0 )
        return false;

    return popToken(token, src, tok, l);
}

bool lexIdentifierOrKeyword(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexIdentifierOrKeyword @ {})", src.loc.toString);
    auto cp0 = src[0];

    if( cp0 == '$' )
    {
        auto mark = src.save;
        src.advance(1);
        if( lexString(src, token) )
        {
            token.type = TOKidentifier;
            token.text = src.sliceFrom(mark);
            token.loc = src.locSpanFrom(mark);
            return true;
        }
        else
            src.restore(mark);

        return lexExternalIdentifier(src, token);
    }

    return lexBasicIdentifierOrKeyword(src, token);
}

bool lexBasicIdentifierOrKeyword(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexBasicIdentifierOrKeyword @ {})", src.loc.toString);
    if( ! isIdentStart(src[0]) )
        return false;

    auto mark = src.save;
    src.advance;

    while( src.length > 0 )
    {
        if( !isIdent(src[0]) )
            break;

        src.advance;
    }

    auto name = src.sliceFrom(mark);

    TOK type = TOKidentifier;

    switch( name )
    {
        case "and":     type = TOKand; break;
        case "let":     type = TOKlet; break;
        case "not":     type = TOKnot; break;
        case "or":      type = TOKor; break;
        case "mod":     type = TOKmod; break;
        case "rem":     type = TOKrem; break;
        case "true":    type = TOKtrue; break;
        case "false":   type = TOKfalse; break;
        case "nil":     type = TOKnil; break;
        case "import":  type = TOKimport; break;
        case "macro":   type = TOKmacro; break;
        case "range":   type = TOKrange; break;
        case "__builtin__": type = TOKbuiltin; break;
        default:
    }

    return popToken(token, src, type, mark);
}

bool lexExternalIdentifier(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexExternalIdentifier @ {})", src.loc.toString);
    // TODO
    // I feel like the syntax needs more thinking about.
    return false;
}

bool lexNumber(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexNumber @ {})", src.loc.toString);
    /*
    number
        >>─┬─'+'───╢ number value ╟─┐
           ├─'-'─┘                  ╧
           └─────┘

    number value
        >>─┬─╢ digit seq ╟─┬─'.'─┬─╢ digit seq ╟─┐
           │               │     └───────────────│
           │               └─────────────────────│
           └─'.'─╢ digit seq ╟─────────────────────┬─╢ exponent ╟─┐
                                                   └────────────────┐
                                                                    ╧

    digit seq
        >>─╢ digit ╟─┬───┬─╢ digit ╟───┬───┐
                     │ │ └────'_'────┘ │ │ ╧
                     │ └───────────────┘ │
                     └───────────────────┘

    exponent
        >>─┬─'e'───┬─'+'─────╢ digit ╟─┬─┐
           └─'E'─┘ ├─'-'─┘ └───────────┘ ╧
                   └─────┘
    */

    auto cp0 = src[0];
    auto mark = src.save;
    
    switch( cp0 )
    {
        case '+':
        case '-':
            src.advance;
            cp0 = src[0];
            break;

        default:
    }

    if( !( isDigit(cp0) || cp0 == '.' ) )
        return false;

    void eatDigitSeq()
    {
        auto cp = src[0];
        if( ! isDigit(cp) )
            err(CEC.LExDecDigit, src.loc, src[0..1]);

        src.advance;

        while( isInnerDigit(src[0]) )
            src.advance;
    }

    void eatExponent()
    {
        auto cp = src[0];
        if( !( cp == 'e' || cp == 'E' ) )
            return;

        src.advance;

        cp = src[0];
        if( cp == '+' || cp == '-' )
        {
            src.advance;
            cp = src[0];
        }

        if( !isDigit(cp) )
            err(CEC.LExDecDigit, src.loc, src[0..1]);

        src.advance;

        while( isDigit(src[0]) )
            src.advance;
    }

    if( cp0 == '.' )
    {
        if( ! isDigit(src[1]) )
            return false;

        src.advance;
        eatDigitSeq;
    }
    else
    {
        eatDigitSeq;

        cp0 = src[0];
        if( cp0 == '.' )
        {
            src.advance;
            cp0 = src[0];
            if( isDigit(cp0) )
                eatDigitSeq;
        }
    }

    cp0 = src[0];
    eatExponent;

    return popToken(token, src, TOKnumber, mark);
}

bool lexString(Source src, out Token token)
{
    debug(TraceLexer) Stderr.format("(lexString @ {})", src.loc.toString);
    if( src[0] != '"' )
        return false;

    void eatHexDigits(size_t n)
    {
        while( n --> 0 )
        {
            auto cp = src[0];
            switch( cp )
            {
                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
                case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
                    break;

                default:
                    err(CEC.LExHexDigit, src.loc, src[0..1]);
            }
            src.advance;
        }
    }

    void eatEscape()
    {
        auto cp0 = src[0];
        switch( cp0 )
        {
            case 'a':
            case 'b':
            case 'f':
            case 'n':
            case 'r':
            case 't':
            case 'v':
            case '\'':
            case '"':
            case '?':
            case '\\':
                src.advance;
                break;

            case 'x':
                src.advance;
                eatHexDigits(2);
                break;

            case 'u':
                src.advance;
                eatHexDigits(4);
                break;

            case 'U':
                src.advance;
                eatHexDigits(8);
                break;

            default:
                err(CEC.LInvStrEsc, src.loc, src[0..1]);
        }
    }

    auto mark = src.save;

    src.advance;

    auto valBegMark = src.save;

    while( src[0] != '"' )
    {
        if( src[0] == '\\' )
        {
            src.advance;
            eatEscape;
        }
        else if( src[0] == dchar.init )
            err(CEC.LUntermString, src.locSpanFrom(mark));

        else
            src.advance;
    }

    auto valEndMark = src.save;

    src.advance;

    return popToken(token, src, TOKstring, mark,
            parseString(src.sliceBetween(valBegMark, valEndMark)));
}

const Lexers = [
    &lexEol,
    &lexEos,
    &lexComment,
    &lexSymbol,
    &lexIdentifierOrKeyword,
    &lexNumber,
    &lexString,
];

bool lexNext(Source src, out Token token)
{
    skipWhitespace(src);

    foreach( lexer ; Lexers )
        if( lexer(src, token) )
            return true;

    return false;
}

void lexUnexpected(Source src)
{
    debug(TraceLexer) Stderr.format("(Unexpected '\\x{:x,02}')", cast(uint) src[0]);
    err(CEC.LUnexpected, src.loc, src[0..1]);
}

struct LexIter
{
    Source src;

    int opApply(int delegate(ref Token) dg)
    {
        int r = 0;
        Token token;
        
        while( true )
        {
            auto f = lexNext(src, token);
            if( !f )
                lexUnexpected(src);

            r = dg(token);
            if( r || token.type == TOKeos )
                break;
        }

        return r;
    }
}

LexIter lexIter(char[] name, char[] src)
{
    return lexIter(new Source(name, src));
}

LexIter lexIter(Source src)
{
    LexIter r;
    r.src = src;
    return r;
}

