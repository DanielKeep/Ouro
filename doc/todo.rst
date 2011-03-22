
Ouro Todo
=========

.. contents::

Optional Arguments
------------------

Functions should be able to specify default values for arguments, allowing
them to be omitted by the caller.  For example::

    let atan(y,x=nil) =
        if {
            x = nil,
            atan(y),
            atan2(y,x)
        }

The default value expression should be evaluated once and in the context of
the function's defining scope.

Named Arguments
---------------

It should be possible to pass function arguments by name.  For example::

    let colorFrom(r, g, b, a=1.0) =
        [a, r, g, b]

    colorFrom(r:1.0, g:0.5, b:0.0)

It might also be a good idea to expand the explode syntax to also work for
maps, although it might need new or modified syntax::

    let rgb = [: r:1.0, g:0.5, b:0.0 :]
    colorFrom(rgb:...)

Referring to symbols from inside macros
---------------------------------------

Currently, all functions track their defining module and get a unique symbol
name.  Of course, they aren't exported by default which can complicate things.

It seems that two additional things would be very useful:

1.  The ability to load a symbol from a module irrespective of export
    visibility.  Perhaps ``lookup`` could be modified to accept an optional
    "use scop instead of exportScop" flag.

2.  Some syntactic construct for converting a local reference to a globally
    usable form.  For example::

        let x = 42

        export let macro f() = #?{x}

        f() = #'{lookup(module(__PATH__), 'x, 'force)}

"Extern" identifiers
--------------------

I think the trick to this is to simply make the syntax a literal ``$``
followed by letters, digits or single-character symbols.

That way, we can write things like::

    let $dstp:blargh = 42

Member access syntax
--------------------

There are two syntaxes that I've considered: C and Smalltalk.

C style uses a ``.`` between the value and the member identifier.
Smalltalk style simply places the member identifier on the RHS of the value.
For example, to look up the ``bar`` member of the value ``foo``::

    foo.bar  |-- C style
    foo bar  |-- Smalltalk style

The Smalltalk style has the benefit of reading better and introduces less
noise into code.  There is, however, a downside to this style: the following
code becomes syntactically valid::

    do {
        woutL~("Line one"),
        woutL~("Line two")
        woutL~("Line three")
    }

Forgetting a comma will be a much harder to diagnose problem.  Personal
experience has shown that forgetting commas is all too easy.

I think the best way forward would be to implement Smalltalk style and see how
it pans out, keeping C style member access in reserve as a fallback option.

Exceptions
----------

Ouro needs a coherent error handling system.  Although continuation-based
systems look cool, it needs something in the near term that's reasonably easy
to implement.

For that, I think a standard try/catch system will be sufficient.  Some
thinking::

    let throw(id, msg, args...) = ...

    guard {
        [success,   "Expr returned normally."],
        [failure,   "Expr was unwound by an exception."],
        [exit,      "Expr returned or unwound."],
        ['DivideByZero, "Expr was unwound by a 'DivideByZero exception."],

        expr
    }

    |-- Catches any exception
    let ex = catch { expr }

    |-- Catches three specific kinds of exception
    let ex = catch { ['DivideByZero, 'SegFault, 'Win32Exception], expr }

There would need to be integration between host exceptions and Ouro
exceptions.  There would also need to be integration of stack traces.

Nascent Ideas
-------------

Additional Types
````````````````

Integer
    Should be like Python's ``int`` type; a big int.

Integer Ranges
``````````````

Being able to concisely express a consecutive sequence of integers would be
very nice.  However, the current ``range`` syntax doesn't really allow for
that.  There are two ways it could be handled:

1.  Introduce a specific integer type along with appropriate syntax.  It might
    also be a good idea to allow constraints.  For example::

        range [0i, 10i)

        range : Integer [0, 10)

    This could work with any type that has a ``nextValue`` metamethod.

2.  Introduce a ``discrete`` syntax for ranges which is distinct from
    continuous ranges.  For example::

        range [0 .. 10)

    Of course, one could just make ``..`` a proper operator which binds more
    loosely than everything except comma.  Then you could do::

        slice([0,1,2,3,4], 1..4)

    But what about steps other than ``1``?

List & Map Comprehensions
`````````````````````````

Comprehensions are a syntactically nice way of expressing transforms.  The
only real question is whether or not the syntactic additions are worth the
saving.  Some examples::

    map(\x.x**2, range[0,5))
    [= x**2 | x <- range[0, 5)]
        = [0, 1, 4, 9, 16]

    mapDict(\x,y.[x,x/y], [:1:2,3:4:])
    [=: x:x/y | x:y <- [:1:2,3:4:]:] = [:1:0.5, 3:0.75:]

Transactions
````````````

Functions could return [value, undoFn] and be used like so::

    transaction {
        returnsNil(),
        var = returnsNonNil(),
        mightThrow(),
        result()
    }

If an exception is thrown at any point, all functions which have already
completed have their "undo" function called in reverse order.

