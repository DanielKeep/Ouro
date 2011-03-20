
Ouro Todo
=========

.. contents::

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

