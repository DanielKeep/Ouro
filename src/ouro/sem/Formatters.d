/**
    Formatters for value types.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Formatters;

import tango.io.device.BitBucket;
import tango.io.stream.Snoop : SnoopOutput;

import Float = tango.text.convert.Float;

import ouro.util.Repr : reprIdent, reprString;
import ouro.util.StructuredOutput;

import AstRepr = ouro.ast.ReprVisitor;
import Sit = ouro.sit.Nodes;

void format(Object obj, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    if( auto fmt = cast(Sit.Formatter) obj )
        fmt.format(emit, width, alignment, padding, precision, options);
    
    else
    {
        // TODO: handle alignment
        emit(obj.toString);
    }
}

void format(char[] fmt, void delegate(char[]) emit, Sit.Value values)
{
    if( auto v = cast(Sit.ListValue) values )
        format(fmt, emit, v);
    else if( auto v = cast(Sit.MapValue) values )
        format(fmt, emit, v);
    else
        assert( false, "can't format with a "~values.classinfo.name );
}

void format(char[] fmt, void delegate(char[]) emit, Sit.ListValue values)
{
    bool failOnErr = false;
    auto vs = values.elemValues;
    int nextIdx = 0;

    void err(char[] msg)
    {
        if( failOnErr ) assert( false, msg );
        else
        {
            emit("{");
            emit(msg);
            emit("}");
        }
    }

fmtLoop:
    while( fmt != "" )
    {
        bool inSub = false;

        // Look for a $
        foreach( i,dchar c ; fmt )
        {
            if( c == '$' )
            {
                inSub = true;
                emit(fmt[0..i]);
                fmt = fmt[i+1..$];
                break;
            }
        }

        // If we didn't find a $, it's because we hit the end of the template.
        if( !inSub )
        {
            emit(fmt);
            break;
        }

        // We're in a substitution.
        if( fmt == "" )
        {
            err("unterminated substitution");
            break;
        }

        // c is the next character, whilst fmt is everything left in the
        // format string.  fmt1 is the previous fmt.
        char c = fmt[0];
        auto fmt1 = fmt;
        fmt = fmt[1..$];

        bool popc()
        {
            if( fmt.length == 0 )
                return false;
            c = fmt[0];
            fmt1 = fmt;
            fmt = fmt[1..$];
            return true;
        }

        bool popcx()
        {
            if( ! popc )
            {
                err("unterminated substitution");
                return false;
            }
            return true;
        }

        // $$ - escaped $.
        if( c == '$' )
        {
            emit("$");
            continue;
        }

        // $n - shortcut for ${n}
        if( '0' <= c && c <= '9' )
        {
            size_t idx = 0;
            do
            {
                idx = idx*10 + (c-'0');
                if( ! popc ) break;
            }
            while( '0' <= c && c <= '9' )

            format(vs[idx], emit, 0, '>', " ", "", null);
            fmt = fmt1;
            continue;
        }

        // $* - shortcut for ${}
        if( c == '*' )
        {
            format(vs[nextIdx++], emit, 0, '>', " ", "", null);
            continue;
        }

        // This means we got a $ followed by something else
        if( c != '{' )
        {
            err("malformed substitution");
            break;
        }

        if( fmt == "" )
        {
            err("unterminated substitution");
            break;
        }

        popc;

        // ${...

        {
            // idx will contain the index of the argument the user wants
            // substituted.
            auto idx = size_t.max;

            // These contain extra formatting data.
            size_t w = 0;
            char a = '>';
            char[] pa = " ";
            char[] pr = "";
            const OptBufferLen = 10;
            char[][OptBufferLen] optBuffer;
            char[][] opt = optBuffer;
            size_t optIdx = 0;

            void addOpt(char[] s)
            {
                if( optIdx < OptBufferLen )
                    opt[optIdx++] = s;
                else
                    opt ~= s;
            }

            char[][] optSlice()
            {
                if( optIdx < OptBufferLen )
                    return opt[0..optIdx];
                else
                    return opt;
            }

            // Are we expecting an index?
            bool gotIdx;
            switch( c )
            {
                case '0': case '1': case '2': case '3': case '4':
                case '5': case '6': case '7': case '8': case '9':
                    gotIdx = true;
                    break;

                case ',': case ';': case ':': case '}':
                    gotIdx = false;
                    break;

                default:
                    err("malformed substitution");
                    continue fmtLoop;
            }

            if( gotIdx )
            {
                idx = 0;
                do
                {
                    idx = idx*10 + (c-'0');
                    if( ! popcx )
                        continue fmtLoop;
                }
                while( '0' <= c && c <= '9' )
            }
            else
            {
                idx = nextIdx++;
            }

            // c == ',': parse alignment
            if( c == ',' )
            {
                // width
                if( ! popcx )
                    continue fmtLoop;

                if( !( '0' <= c && c <= '9' ) )
                {
                    err("malformed substitution alignment");
                    continue fmtLoop;
                }
                w = 0;
                do
                {
                    w = w*10 + (c-'0');
                    if( ! popcx )
                        continue fmtLoop;
                }
                while( '0' <= c && c <= '9' )

                // align
                switch( c )
                {
                    case '<': case '|': case '>':
                        a = c;
                        if( ! popcx )
                            continue fmtLoop;
                        break;

                    default:
                }

                // padding
                pa = fmt1;
                size_t pal = 0;

                while( c != '}' && c != ';' && c != ':' )
                {
                    pal ++;
                    if( ! popcx )
                        continue fmtLoop;
                }

                pa = pa[0..pal];

                if( pa == "" )
                    pa = " ";
            }

            // c == ';': parse precision
            if( c == ';' )
            {
                pr = fmt;
                size_t prl = 0;

                do
                {
                    prl ++;
                    if( ! popcx )
                        continue fmtLoop;
                }
                while( c != '}' && c != ':' )

                prl --;

                pr = pr[0..prl];
            }

            // c == ':': parse options
            if( c == ':' )
            {
                if( ! popcx )
                    continue fmtLoop;

                while( c != '}' )
                {
                    if( c == '{' )
                    {
                        if( ! popcx )
                            continue fmtLoop;

                        auto o = fmt1;
                        size_t ol = 0;

                        while( c != '}' )
                        {
                            ol ++;
                            if( ! popcx )
                                continue fmtLoop;
                        }

                        if( ! popcx )
                            continue fmtLoop;

                        o = o[0..ol];
                        addOpt(o);
                    }
                    else
                    {
                        addOpt(fmt1[0..1]);
                        if( ! popcx )
                            continue fmtLoop;
                    }
                }
            }

            // Expecting '}'
            if( c != '}' )
            {
                err("malformed substitution");
                continue fmtLoop;
            }

            // Do the format
            format(vs[idx], emit, w, a, pa, pr, optSlice);
        }
    }
}

void format(char[] fmt, void delegate(char[]) emit, Sit.MapValue values)
{
    assert( false, "nyi" );
}

private:

BitBucket bb;

static this()
{
    bb = new BitBucket;
}

static this()
{
    Sit.AstQuoteValue.formatFn = &format_AstQuoteValue;
}

void format_AstQuoteValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.AstQuoteValue) ptr;
    scope snoop = new SnoopOutput(bb, emit);
    scope so = new StructuredOutput(snoop);
    scope repr = new AstRepr.ReprVisitor(so);
    repr.visitBase(node.ast);
}

static this()
{
    Sit.FunctionValue.formatFn = &format_FunctionValue;
}

void format_FunctionValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.FunctionValue) ptr;

    void emitArgs()
    {
        foreach( i,arg ; node.args )
        {
            if( i > 0 ) emit(",");
            emit(reprIdent(arg.ident));
            if( arg.isVararg ) emit("...");
        }
    }

    if( node.name == "" || node.name[0.."λ".length] == "λ" )
    {
        emit("λ");
        emitArgs;
        emit(".");
    }
    else if( node.expr !is null )
    {
        emit(reprIdent(node.name));
        emit("(");
        emitArgs;
        emit(")");
    }
    else
    {
        emit("__builtin__(");
        emit(reprString(node.name));
        emit(")(");
        emitArgs;
        emit(")");
    }
}

static this()
{
    Sit.ListValue.formatFn = &format_ListValue;
}

void format_ListValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.ListValue) ptr;
    emit("[");
    foreach( i,elemValue ; node.elemValues )
    {
        if( i > 0 ) emit(", ");
        format(elemValue, emit, width, alignment, padding, precision, options);
    }
    emit("]");
}

static this()
{
    Sit.LogicalValue.formatFn = &format_LogicalValue;
}

void format_LogicalValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.LogicalValue) ptr;
    emit(node.value ? "true" : "false");
}

static this()
{
    Sit.MapValue.formatFn = &format_MapValue;
}

void format_MapValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.MapValue) ptr;
    emit("[:");
    foreach( i,kvp ; node.kvps )
    {
        if( i > 0 ) emit(", ");
        format(kvp.key, emit, width, alignment, padding, precision, options);
        emit(":");
        format(kvp.value, emit, width, alignment, padding, precision, options);
    }
    emit(":]");
}

static this()
{
    Sit.ModuleValue.formatFn = &format_ModuleValue;
}

void format_ModuleValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = (cast(Sit.ModuleValue) ptr).modul;
    emit("module(");
    emit(reprString(node.path));
    emit(")");
}

static this()
{
    Sit.StringValue.formatFn = &format_StringValue;
}

void format_StringValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.StringValue) ptr;
    emit(node.value);
}

static this()
{
    Sit.SymbolValue.formatFn = &format_SymbolValue;
}

void format_SymbolValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.SymbolValue) ptr;
    emit("'");
    emit(node.value);
}

static this()
{
    Sit.NumberValue.formatFn = &format_NumberValue;
}

void format_NumberValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.NumberValue) ptr;
    emit(Float.toString(node.value));
}

static this()
{
    Sit.RangeValue.formatFn = &format_RangeValue;
}

void format_RangeValue(void* ptr, void delegate(char[]) emit,
        size_t width, char alignment, char[] padding,
        char[] precision, char[][] options)
{
    auto node = cast(Sit.RangeValue) ptr;
    emit("range ");
    emit(node.incLower ? "[" : "(");
    format(node.lowerValue, emit, width, alignment, padding, precision, options);
    emit(", ");
    format(node.upperValue, emit, width, alignment, padding, precision, options);
    emit(node.incUpper ? "]" : ")");
}

