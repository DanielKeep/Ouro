/**
    This module allows the specification of native types.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.invoke.Type;

import Integer = tango.text.convert.Integer;

import ouro.util.EnumCtfe;

mixin(genEnum_ctfe
(
    "Variadic",
    [
        "None",
        "Untyped"
    ],
    /*map*/         null,
    /*toString*/    null,
    /*fromString*/  null
));

mixin(genEnum_ctfe
(
    "BasicTypeId",
    [
        // Basic types
        "Void"[],
        "Word",
        "Handle",

        "Bool",
        "Char",
        "SInt", "UInt",
        "Float",

        // Composite types
        "Pointer",      // T*
        "Array",        // {word, T*}
        "ZeroTerm"      // T*, terminated by zero element

        // Future:
        /+
        "Tuple",        // T[width]
        "Optional",     // Value passed via pointer, null if omitted
        "Out",          // Result passed via pointer
        "InOut",        // Argument mutated via pointer
        // +/
    ],
    /*map*/         null,
    /*toString*/    "basicTypeIdToString",
    /*fromString*/  "basicTypeIdFromString"
));

enum
{
    MaxId = BasicTypeId.ZeroTerm + 1,
}

struct Properties
{
    char[] name;
    size_t size0, size1;
    bool isComposite;

    size_t size()
    {
        if( size1 == 0 )
            return size0;
        else
            return 0;
    }

    size_t minWidth()
    {
        if( size1 == 0 )
            return 0;
        else
            return size0;
    }

    size_t maxWidth()
    {
        return size1;
    }

    bool isSized() { return size != 0; }
}

const WordSize = size_t.sizeof;

Properties[MaxId] TypeProperties =
[
// Basic types
{ "void",   0, 0,           false   },
{ "word",   WordSize, 0,    false   },
{ "handle", WordSize, 0,    false   },
{ "bool",   1, 0,           false   },
{ "char",   1, 4,           false   },
{ "sint",   1, 8,           false   },
{ "uint",   1, 8,           false   },
{ "float",  4, 8,           false   },

// Composite types
{ "ptr",    WordSize, 0     true    },
{ "array",  WordSize*2, 0,  true    },
{ "zt",     WordSize, 0,    true    }
];

class Type
{
    alias BasicTypeId Id;
    alias TypeProperties Prop;

    Id id;
    Type subType = null;
    size_t width = 0;

    this(Id id)
    {
        auto size = Prop[id].size;
        assert( size != 0 || id == Id.Void || id == Id.Handle );
        assert( ! Prop[id].isComposite );

        this.id = id;
    }

    this(Id id, size_t width)
    {
        auto minw = Prop[id].minWidth;
        auto maxw = Prop[id].maxWidth;
        assert( maxw != 0 );
        assert( minw <= width && width <= maxw );
        assert( ! Prop[id].isComposite );

        this.id = id;
        this.width = width;
    }

    this(Id id, Type subType)
    {
        assert( subType !is null );
        assert( Prop[id].size != 0 );
        assert( Prop[id].isComposite );

        this.id = id;
        this.subType = subType;
    }

    size_t size()
    {
        auto s = TypeProperties[id].size;
        if( s == 0 )
            return width;
        else
            return s;
    }

    char[] toString()
    {
        auto s = Prop[id].name;
        if( width != 0 )
            s ~= "(" ~ Integer.toString(size*8) ~ ")";

        if( subType !is null )
            s ~= " " ~ subType.toString;

        return s;
    }

    equals_t opEquals(Object rhsObj)
    {
        auto rhs = cast(Type) rhsObj;
        if( rhs is null ) return cast(equals_t) false;

        // Subclasses cannot be equal
        if( this.classinfo !is rhs.classinfo ) return false;

        if( this.subType !is null && rhs.subType !is null
                && this.subType != rhs.subType )
            return false;
        else if( this.subType !is null || rhs.subType !is null )
            return false;

        return this.id == rhs.id
            && this.width == rhs.width;
    }

    static
    {
        static this()
        {
            t_void = new Type(Id.Void);
            t_word = new Type(Id.Word);

            t_bool = new Type(Id.Bool);
            
            t_char8  = new Type(Id.Char, 1);
            t_char16 = new Type(Id.Char, 2);
            t_char32 = new Type(Id.Char, 4);

            t_sint8  = new Type(Id.SInt, 1);
            t_sint16 = new Type(Id.SInt, 2);
            t_sint32 = new Type(Id.SInt, 4);
            t_sint64 = new Type(Id.SInt, 8);

            t_uint8  = new Type(Id.UInt, 1);
            t_uint16 = new Type(Id.UInt, 2);
            t_uint32 = new Type(Id.UInt, 4);
            t_uint64 = new Type(Id.UInt, 8);

            t_float32 = new Type(Id.Float, 4);
            t_float64 = new Type(Id.Float, 8);

            t_void_p    = new Type(Id.Pointer, t_void);
            t_sz        = new Type(Id.ZeroTerm, t_char8);

            version( Windows )
                t_wsz   = new Type(Id.ZeroTerm, t_char16);
            else
                t_wsz   = new Type(Id.ZeroTerm, t_char32);
        }

        Type t_void, t_word,
             t_bool,
             t_char8, t_char16, t_char32,
             t_sint8, t_sint16, t_sint32, t_sint64,
             t_uint8, t_uint16, t_uint32, t_uint64,
             t_float32, t_float64,
             t_void_p, t_sz,
             t_wsz;
    }
}

class HandleType : Type
{
    char[] ident;

    this(char[] ident)
    {
        super(Id.Handle);
        this.ident = ident;
    }

    char[] toString()
    {
        return Prop[id].name~" "~ident;
    }

    equals_t opEquals(Object rhsObj)
    {
        auto rhs = cast(HandleType) rhsObj;
        if( rhs is null ) return cast(equals_t) false;

        return this.ident == rhs.ident;
    }
}

