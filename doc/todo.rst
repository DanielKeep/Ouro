
Ouro Todo
=========

.. contents::

Fix compile-time evaluation
---------------------------

Currently, compile-time evaluation stops as soon as it encounters a runtime
function.  Actually, that's not quite true.  It *does* evaluate the function's
arguments, it just doesn't store them anywhere.

What needs to happen, I think, is a modified version of the Eval visitor that
tries to evaluate a tree.  However, if a call has to be delayed, it updates
the tree instead of giving up entirely.

Referring to symbols from inside macros
---------------------------------------

It's a complete pain in the arse trying to use other functions or macros from
inside a macro.  Since the macro gets expanded in another context, it's very
hard to tell whether you can safely use a particular name or not.

Ideas:

-   **Automatic, unique symbols.** This involves ensuring that all functions can
    be turned back into an AST by ensuring there exists an expression that
    uniquely refers to them anywhere.

    For any function which is not being directly bound to a symbol, create a
    new, top-level statement with a unique symbol name.  Bind the function to
    that and substitute a reference.

    For example, this (in module ``fib``)::

        let fib(n) = let {
            [fib', \a,b,n,m. if { n < m, fib'(b,a+b,n+1), a }],
            fib'(0,1,0,n)
        }

    Might become::

        let $"--λ 0" = \a,b,n,m. if { n < m, $"--λ 0"(b,a+b,n+1), a }

        let fib(n) = let {
            [fib', $"--λ 0"],
            fib'(0,1,0,n)
        }

    Then, the result of ``ast(fib')`` would be
    ``#~'{module("/fib") $"--λ 0"}``.

"Extern" identifiers
--------------------

I think the trick to this is to simply make the syntax a literal ``$``
followed by letters, digits or single-character symbols.

That way, we can write things like::

    let $dstp:blargh = 42

Modules
-------

Here's what I'm thinking.

When compiling a program, we create a 'statement pool' into which all
statements to be compiled and evaluated go.  Each statement maintains its
association with a specific module, scope, etc.

Loading a module involves pouring the results of the initial semantic pass
into this pool.  The statements can then be iterated until they have all been
processed.

The pool itself should probably *actually* be a list of modules since we might
want to add extra statements into the mix whilst doing semantics (for example,
binding lambdas to a symbol).

Symbols
-------

Symbols are immutable, interred strings.  They can be written like so::

    'thisIsASymbol

    '$"this is also a symbol"

    Symbol("I'm a symbol too, don'tcherknow?")

These will be useful as the runtime representation of identifiers and named
flags to functions.

For example::

    open("blah.txt", 'append)

Will be more space-efficient and faster than::

    open("blah.txt", "append")

This is because an interred string need only be stored exactly once
program-wide and equality is determined with a pointer test.

Member access syntax
--------------------

Still not 100% decided on this.  The following are the current possibilities
I'm considering.

The three examples are:

- Accessing member ``bar`` of value ``foo``.
- Accessing member ``baz`` of module ``/quxx``.
- Parameterised access of member ``bar`` of value ``foo``.

Note that the below use the as-yet unimplemented symbol syntax.

Internal representation
```````````````````````

::

    getElement(foo, 'bar)

    getElement(module("/quxx"), 'baz)

    getElement(foo, 'bar)

C-style
```````

::

    foo.bar

    module("/quxx").baz

    foo.('bar)

Smalltalk-style
```````````````

Note: also used by Io.

::

    foo bar

    module("/quxx") baz

    foo ['bar]

