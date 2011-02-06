
Compiling Meme
==============

:author:    Daniel Keep <daniel.keep@gmail.com>

- Tokenise source.

- Parse into AST.

- Rewrite statements into equivalent expressions.  Entire module should be
  inside a 'do' call.

- Expand macros.

  TODO: Need more detail.

- Every expression needs to be associated with a lookup context.

  - There's a context created for each import call and every let call.

- Continuation transform deferred for now.

- Perform bytecode generation.

Thoughts on Bytecode
====================

Register machine with a sliding stack of registers: easy to implement, better
information on function stack size.  When a function is called, the stack
looks like this::

    ... rt (a0 a1 ...) (l0 l1 ...) r0

``rt`` is return target information for the VM.  It will probably consist of
a (``Function``, ``offset``) tuple.  ``a0``, ``a1``, ... are the arguments to
the function.  ``l0``, ``l1``, ... are the local variables for the function.
``r0`` is the result of any calls this function makes.

If this function was to call another function, (let's say the ``abs``
function), then the stack would look like this during the execution of the
callee::

    ... rt (a0 a1 ...) (l0 l1 ...) rt' a0' ...

Where ``rt'`` and ``a0'`` are the callee's registers.  When the callee
returns, the stack will be adjusted back to its original state, except that
``r0`` will (probably) contain a different value.

However, let's say that the function uses a tail call instead.  In that case,
the stack will look like::

    ... rt a0' ...

Note that the positioning of the return target register is still somewhat
unsure.  Placing it before the arguments allows for arguments and locals to be
addressed as a single, contiguous range of registers.

On the other hand, this requires that arguments to a function not only be
arranged in the caller such that they are in order and contiguous, but
requires that they be copied up the stack.

However, storing locals such that arguments "end up" in the correct spot such
that they do not need to be copied sounds like extra work.  For now, we should
just forbid register ranges that cross between arguments and locals and do it
the easy way.  Changing to the "better" way shouldn't break any bytecode.

Functions will need a table of values it uses but which are too large to fit
into the bytecode.  This includes things like (possible) real values, lists,
other functions, lambda code, etc.

Note that there will need to be a way to specify that a given entry is a
deferred symbol lookup.  In this case, the symbol is resolved the first time
the VM needs it, and the result replaces the deferred lookup in the table.

Or maybe that could be done in a pre-processing step.  Whatever.

Some functions are actually closures.  A closure is an object that binds the
function code with a set of values.  At a later date, escape analysis might be
used to limit creation of closures since they require heap allocation.

We could turn comparisons directly into a logical value, store it in a local,
then base conditional instructions on a register.  This doesn't really allow
for a "spaceship" operator.

Alternately, we could have comparisons store their result in a special
"comparison register" which could then be used for several different jump
types.

The first is probably the most orthogonal.

::
    let poly(a,b,c,x) = a*x**2 + b*x + c

::
    Function
    {
        name: "poly"
        args: ["a","b","c","x"]
        closureValues: 0
        layout: 4.5
        lookup: --
        valueTable: []
        code:
        {
            exp.rri l0 a3 2
            mul.rrr l1 a0 l0
            mul.rrr l2 a1 a3
            add.rrr l3 l1 l2
            add.rrr l4 l3 a2
            ret.r   l4
        }
    }

::
    let blah(x) = poly(1,2,3,x)

::
    Function
    {
        name: "blah"
        args: ["x"]
        closureValues: 0
        layout: 1.5
        lookup: --
        valueTable:
        [
            0: deferred poly
        ]
        code:
        {
            mov.ri  l0 1
            mov.ri  l1 2
            mov.ri  l2 3
            mov.rr  l3 a0
            ldt.ri  l4 0
            tail.rR l4 l0 l4
        }
    }

::
    let fact(n) = let(
        [acc, \i,n': if(i <= 1, n', acc(i-1, n'*i))],
        acc(n, 1))

::
    Function
    {
        name: "face"
        args: ["n"]
        closureValues: 0
        layout: 1 3
        lookup: --
        valueTable:
        [
            0: Function
            {
                name: "acc"
                args: ["i", "n'"]
                closureValues: 0
                layout: 2 4
                lookup: --
                valueTable:
                [
                    0: deferred acc
                ]
                code:
                {
                    cle.rri l3 a0 1
                    jf.or   :0 l3
                    ret.r   a1
                0:  sub.rri l0 a0 1
                    mul.rrr l1 a1 a0
                    ldt.ri  l2 0
                    tail.rR l2 l0 l1
                }
            }
        ]
        code:
        {
            mov.rr  l0 a0
            mov.ri  l1 1
            ldt.ri  l2 0
            tail.rR l2 l0 l1
        }
    }

::
    let blah(n) = \:n

::
    Function
    {
        name: "blah"
        args: ["n"]
        closureValues: 0
        lookup: --
        layout: 1.2
        valueTable:
        [
            0: Function
            {
                name: null
                args: []
                closureValues: 1
                layout: 0.1
                lookup: --
                valueTable: []
                code:
                {
                    ; load n into l0 from closure table
                    ldc.ri  l0 0
                    ret.r   l0
                }
            }
        ]
        code:
        {
            ; load closure code into l0 from value table
            ldt.ri  l0 0
            ; bind n to code to create closure in l1
            cls.rrR l1 l0 a0 -
            ; return closure
            ret.r   l1
        }
    }

::
    let even?(n) = if(n == 0, true, odd?(abs(n-1)))
    let odd?(n) = if(n == 0, false, even?(abs(n-1)))

::
    Function "abs"
    {
        name: "abs"
        args: ["n"]
        layout: 1.0
        lookup: nil
        code: nil
        opcodes:
        [
            OpcodeBinding
            {
                argTypes: [ArgType.Register]
                result: true
                opcode: Opcode.Abs_r
            }
        ]
        native:
        {
            result: true
            method: native_abs
        }
    }

    Function "even?"
    {
        name: "even?"
        args: ["n"]
        layout: 1.6
        lookup: --
        valueTable:
        [
            0: deferred odd?
            1: deferred abs
        ]
        code:
        {
            ceq.rri l0 a0 0
            jf.or   :0 l0
            ret.l   true
        0:  sub.rri l2 a0 1
            ldt.ri  l3 1
            call.rR l3 l2 1
            mov.rr  l4 r0
            ldt.ri  l5 0
            tail.rR l5 l4 1
        }
    }

    Function "odd?"
    {
        name: "odd?"
        args: ["n"]
        layout: 1.6
        lookup: --
        valueTable:
        [
            0: deferred even?
            1: deferred abs
        ]
        code:
        {
            ceq.rri l0 a0 0
            jt.or   :0 l0
            ret.l   false
        0:  sub.rri l2 a0 1
            ldt.ri  l3 1
            call.rR l3 l2 1
            mov.rr  l4 r0
            ldt.ri  l5 0
            tail.rR l5 l4 1
        }
    }

Opcodes
-------

Arithmetic
~~~~~~~~~~

::
    add.rrr     ; dest = a + b
    sub.rrr     ; dest = a - b
    mul.rrr     ; dest = a * b
    div.rrr     ; dest = a / b
    idv.rrr     ; dest = a // b = floor(a / b)
    mod.rrr     ; dest = a mod b = a - b * floor(a / b)
    rem.rrr     ; dest = a rem b = a - b * trunc(a / b)
    exp.rrr     ; dest = a ** b
    neg.rr      ; dest = -a

Built-in Functions
~~~~~~~~~~~~~~~~~~

Algebraic::

    sqrt.rr

Transcendental::

    erf.rr
    erfc.rr
    loge.rr
    log2.rr
    log10.rr

Trigonometric::

    cos.rr
    sin.rr
    tan.rr
    acos.rr
    asin.rr
    atan.rr
    atan2.rr
    cosh.rr
    sinh.rr
    tanh.rr
    acosh.rr
    asinh.rr
    atanh.rr

Miscellaneous Numerical::

    abs.rr
    clamp.rrrr
    max.rrr
    min.rrr

Probability::

    unirii.rrr
    unirix.rrr
    unirxi.rrr
    unirxx.rrr
    unii.rrr
    norm.rrr
    pois.rr
    poisc.rrrr

Comparison
~~~~~~~~~~

::
    ceq.rrr     ; dest = (a = b)
    cne.rrr     ; dest = (a <> b)
    clt.rrr     ; dest = (a < b)
    cgt.rrr     ; dest = (a > b)
    cle.rrr     ; dest = (a <= b)
    cge.rrr     ; dest = (a >= b)

Loads
~~~~~

::
    ldc.ri      ; dest = closureValues[a]
    ldl.ri      ; dest = a (integer literal)
    ldl.rl      ; dest = a (logical literal)
    ldt.ri      ; dest = values[a]
    ldnil.r     ; dest = nil

Logical
~~~~~~~

::
    not.rr      ; dest = not a
    and.rrr     ; dest = a and b
    or.rrr      ; dest = a or b

Jumps
~~~~~

::
    j.o         ; dest
    jf.or       ; dest flag
    jt.or       ; dest flag

Subroutines
~~~~~~~~~~~

::
    call.rR     ; fn args
    tail.rR     ; fn args
    ret.r       ; value

Misc
~~~~

::
    cls.rrR     ; dest fn values
    mov.rr      ; dest src
    comp.rrr    ; dest = (a.b)(...) = (b(a(...)))(...)

Prelude
=======

Macro functions
---------------

``ast|*``
    Ast functions.

``ast|assert(ast, test, message)``
    Raises a compile-time error if the given ``test`` is false.  The ``ast``
    passed is used to indicate the location of the error.

``ast|listHead(l)``
``ast|listTail(l)``
``ast|listLength(l)``
    List manipulation.

``ast|isList(ast)``
``ast|typeOf(ast)``
    Type of an ast expression.

``#~"(expr)``, ``ast|quote(expr)``
    Replaces the given ``expr`` with its ast.

``#~'(expr)``, ``ast|qquote(expr)``
    Replaces the given ``expr`` with its ast, allowing substitution.

``#~$(ast)``, ``ast|qqSub(ast)``
    Causes the given ``ast`` to be substituted directly into the containing
    quasiquotation.

Examples
--------

``case``::

    let macro case(a, cs...) = let'(
        [_,     ast|assert(cs, ast|isList(cs),
                    format("expected list, got a $$", [ast|typeOf(cs)]))],
        [c,     ast|listHead(cs)],
        [cl,    ast|listLength(c)],
        [_,     ast|assert(c, ast|isList(c),
                    format("expected two-element list for case, "
                        "got a $$", [ast|typeOf(c)]))],
        [_,     ast|assert(c, cl = 2,
                    format("expected two-element list for case, "
                        "got $$ element$$",
                        [cl, if(cl=1,"","s")]))],
        [cc,    head(c)],
        [ce,    nth(c, 1)],
        [lv,    ast|uniqueIdent!()],
        [ct,    if(ast|listLength(cs) == 1 and cc = #~"(else),
                    #~"(true),
                    #~'( #~$(lv) = #~$(cc) ))],
        [tail,  if(ast|listLength(cs) > 1,
                    #~'(case(#~$(lv), ... #~$(ast|listTail(cs)))),
                    #~"(nil))],
        #~'(
            (\#~$(lv): if(#~$(ct), #~$(ce), #~$(tail)))(#~$(a))
        )
    )

Example::

    case(1, [0, "foo"], [1, "bar"], [else, "?"])

    (\__unique_0: if((__unique_0 = 0),
            "foo",
            case(__unique_0, ... [[1, "bar"], [else, "?"]])))(1)

    (\__unique_0: if((__unique_0 = 0),
            "foo",
            (\__unique_1: if((__unique_1 = 1),
                    "bar",
                    case(__unique_1, ... [[else, "?"]])))(__unique_0)))(1)

    (\__unique_0: if((__unique_0 = 0),
            "foo",
            (\__unique_1: if((__unique_1 = 1),
                    "bar",
                    (\__unique_2: if(true,
                            "?",
                            nil))(__unique_1)))(__unique_0)))(1)

    "After (lambda)(const) inlining..."

    if((1 = 0), "foo", if((1 = 1), "bar", if(true, "?", nil)))

    "After constant expression expansion..."

    if(false, "foo", if(true, "bar", if(true, "?", nil)))

    "After constant branch expansion..."

    "bar"

Ideas For Future
================

``a ~$ list``, ``format(a, list)``
    String formatting.

