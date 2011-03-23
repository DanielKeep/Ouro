
``/ouro/lang``
==============

This module is implicitly imported and merged into every module's
scope.  Thus, it contains the "built-in" functions and macros.  You do not
need to explicitly import this module, unless you want to ensure access to a
specific value.

``assert{expr}``
----------------

Ensures that *expr* evaluates to ``true``; if it doesn't, it raises an error.

``astOf(v)``
------------

Attempts to convert the given value into a semantically equivalent AST node.

::

    assert { astOf(42) = #'{ 42 } }
    assert { astOf(['a,"b"]) = #'{ ['a, "b"] } }

``branch(l, b_t, b_f)``
-----------------------

Calls |b_t| or |b_f| based on the value of *l*,
returning its result.
Both |b_t| and |b_f| are expected to be zero-argument
functions.

::

    let branch(l, b_t, b_f) = (
        if { l, b_t(), b_f() }
    )

    assert { branch(true, \."yup", \."nope") = "yup" }
    assert { branch(false, \."yup", \."nope") = "nope" }

``case{value, rules...}``
------------------------------

Evaluates *value* and then checks its value against each rule of
the form ``[v, expr]``.  When a matching *v* is found, the
corresponding *expr* is evaluated.

The last rule may replace *v* with ``else``; this will match any value.

If no rule matches, an error is raised.

``cond{rules...}``
------------------

Tests each successive rule of the form ``[test, expr]``.  Once a *test*
expression evaluates to ``true``, the corresponding *expr* is evaluated and no
further rules are processed.

The last rule may replace *test* with ``else``; this is effectively equivalent
to using ``true``.

If no rule matches, an error is raised.

``do{exprs...}``
----------------

Evaluates ``exprs`` in strict left-to-right order, returning the result of the
last.

::

    assert { do { 1, 2, 3 } = 3 }

``fail(msg)``
-------------

Halts execution with the specified ``msg``.

``filter(f, l)``
----------------

Returns all elements from *l* for which ``f(l)`` is ``true``.

Note that order of evaluation is *not* specified.

::

    let even?(x) = (x rem 2) = 0

    assert { filter(even?, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]) = [0, 2, 4, 6, 8] }

``forEach{binding, expr}``
--------------------------

Given a binding of the form ``[i, e]``, it evaluates *expr* once for each
element of *e*, binding the element's value to *i*.

*expr* may also use ``$".index"``; this contains the zero-based index of the
element during each evaluation.

Example::

    |-- Print out the arguments passed to the program.
    import "/ouro/stdio" : woutFL~
    export let main(args) =
        forEach {
            [arg, tail(args)],
            woutFL~(" args[$*] = ${:R}", $".index"+1, arg)
        }

``format(s_fmt, vs)``
---------------------

Substitutes *vs* into the format string |s_fmt|.  *vs* may be
either a list or a map.

The following character sequences are treated specially:

``$$``
    This results in a literal ``$`` being inserted into the output.

``$*``, ``${}``
    Substitutes the "next" value.  The first ``$*`` substitutes the
    0\ :sup:`th` value, the next ``$*`` substitutes the 1\ sup:`st`
    value, and so on.

    Only valid where *vs* is a list.

``$n``, ``$ident``
    Where *n* is a non-negative integer and *vs* is a list,
    substitutes the *n*\ :sup:`th` value.

    Where *ident* is an ouro identifier and *vs* is a map,
    substitutes the value with key '\ *ident*.

``${n}``, ``${ident}``
    Substitutes the *n*\ :sup:`th` value, or the value with key
    '\ *ident*.

``${n,a}``, ``${ident,a}``
    As above, but aligns the result based on *a*.

    The format of *a* is one of ``w<p``, ``w|p`` or ``w>p``.  Any component
    and all subsequent ones can be omitted but no single component may be
    skipped.

    *w*
        Specifies the width of the area into which the result should be
        aligned.  It must be a non-negative decimal integer.

        Future:
            It may also be a
            substitution of the form ``$*``, ``$n`` or ``$ident``.

    ``<``, ``|``, ``>``
        These are used to specify left-, centre- or right-alignment.

        If omitted, it defaults to right-alignment.

    *p*
        Specifies the string to use for padding.  If omitted, defaults to a
        single space.

        Future:
            It may also be a substitution of the form
            ``$*``, ``$n`` or ``$ident``.

``${n;p}``, ``${ident;p}``
    As above, but also specifies precision.

    Precision is an arbitrary string.  How it is interpreted depends on the
    value being substituted.  Common formats are listed below.

    ``tn``
        Truncates the formatted value to *n* code points.

    Future
        ``e<n``, ``e|n``, ``e>n``
            If the formatted value is longer than *n* code points, it
            truncates it and inserts ellipses (``...``) either on the right,
            in the centre or on the left (``<``, ``|``, ``>``).

        ``.m``, ``.mex``
            Limits numbers to *m* digits after the decimal place.

            If specified, *x* specifies the number of digits in the exponent.

        ``dm``, ``dmex``
            Limits numbers to *m* significant digits.

            If specified, *x* specifies the number of digits in the exponent.

``${n:f}``, ``${ident:f}``
    As above, but also uses the format option *f*.  Format options are
    specific to the exact value being substituted.  Some common format
    options are listed below.

    Simple substitutions of the form ``$*``, ``${}``, ``$n``, ``$ident``,
    ``${n}`` or ``${ident}`` may be used within format options, either as a
    complete option or as the argument to another option.  Some examples::

        ("${0:$*}" (.format.) [16, "x"]) = "10"

        ("${0:$1} ${0:(?:$2:$3)}" (.format.) [true, "(?:a:b)", "x", "y"])
            = "a x"

    You can also substitute the value of a string literal like so::

        ("${0:(p:$'s':$\":\")}" (.format.) [2]) = ":"

    String substitutions can be written as either ``$"..."`` or ``$'...'``;
    the latter is allowed to make writing them inside string literals easier.

    Note that an option or option argument can only be one of a literal,
    string substitution or general substitution.

    -   General

        ``R``
            Substitutes the value's representation.

    -   Strings

        ``l``
            Substitutes the length of the string in code points.

    Future
        -   Logical

            ``(?:t:f)``
                Substitutes one of *t* or *f* based on value.

                **TODO**: clarify how substitutions work with this.

            ``1``
                Substitutes ``1``/``0`` based on value.

            ``t``, ``T``
                Substitutes ``true``/``false`` or ``True``/``False`` based on
                value and case.

                **TODO**: decide on this.

                If alignment/precision is 1, only the first letter is
                substituted.

            ``y``, ``Y``
                Substitutes ``yes``/``no`` or ``Yes``/``No`` based on value
                and case.

                **TODO**: decide on this.

                If alignment/precision is 1, only the first letter is
                substituted.

        -   Numbers

            ``+``
                Force the inclusion of leading ``+`` for positive numbers and
                exponents.

            ``b``
                Represents the number in binary.

            ``c``
                Treats the number as a Unicode code point, substituting the
                code point itself.

            ``e``, ``E``
                Uses scientific notation.  The case determines the case of the
                exponent letter.

            ``(e:n)``, ``(E:n)``
                Uses scientific notation as above.  Forces the exponent to be
                *n* digits wide.

            ``o``
                Represents the number in octal.

            ``(p:s_0:s_1:...)``
                Substitutes *s*\ :sub:`0`, *s*\ :sub:`1`, ... based on the
                plurality of the number.

                **TODO**: clarify how substitutions work with this.

            ``r``
                Rounds the number to the nearest integer.

            ``(r:R)``
                Rounds the number based on the value of *R*.

            ``x``, ``X``
                Represents the number in hexadecimal.  The case determines the
                case of the non-decimal digits.

            ``,``, ``_``
                Inserts a separator (either a ``,``\ [*]_ or ``_``) between
                every 3 digits, counting out from the decimal place.

            ``(,:n)``, ``(_:n)``
                Inserts a separator as above; instead of every 3 digits, it
                inserts it every *n* digits, where *n* is a positive integer.

        -   Lists

            ``:f...``, ``(:f...)``
                Uses *f...* as the format options for elements.

            ``l``
                Substitutes the length of the list.

            ``r``
                Raw formatting: formats all elements without brackets,
                commas or spacing.

            ``(s:S...)``
                Uses *S...* as the separator between elements.

        -   Maps

            ``(k:...)``, ``(v:...)``
                Uses *fk...* and *fv...* as the format options for keys and
                values respectively.

            ``l``
                Substitutes the number of elements in the map.

            ``(p:S...)``
                Uses *S...* as the separator between key/value pairs.

            ``(s:S...)``
                Uses *S...* as the separator between elements.

``${x,a;p}``, ``${x,a:f}``, ``${x;p:f}``, ``${x,a;p:f}``
    Valid combinations of the above.  *x* is either a non-negative integer
    or an identifier.

.. [*]  The actual character used may be modified by culture settings,
    although how this is performed is as-yet undefined.

``if{condition, then, else}``
-----------------------------

Evaluates and returns the result of |then| or |else|
based on the value of *condition*.  The branch not chosen is not evaluated.

::

    assert { if { true, "yup", "nope" } = "yup" }
    assert { if { false, "yup", "nope" } = "nope" }

    if { true,
        then: 'doNothing,
        else: fail { "Oh noes!" }
    }

``head(l)``
-----------

Returns the first element of the list *l*.  Passing a list with zero
elements is an error.

::

    assert { head([1,2,3]) = 1 }

``lookup(v, sym)``
------------------

Looks up the value bound to ``sym`` in the value ``v``.

Compile-time only.

::

    assert { lookup(module("/ouro/lang"), 'if) = if }

``map(f, l)``
-------------

Transforms the elements of *l* by passing them through *f*.

Note that order of evaluation is *not* specified.

::

    let square(x) = x**2

    assert { map(square, [0,1,2,3,4]) = [0,2,4,6,8] }

``module(path)``
----------------

Returns the module specified by the string ``path``.

Compile-time only.

::

    let lang = "/ouro/lang"

    assert { module("/ouro/lang") = lang }

``reduce(f, l)``
----------------

Reduces the list *l* to a single value.  It has the same effect as if the
expression ``l_0 (.f.) l_1 (.f.) ... (.f.) l_n`` was evaluated, with
``l_0``, ``l_1``, ..., ``l_n`` being elements of *l*.

Note that order of evaluation is *not* specified.

::

    let add(x,y) = x+y

    assert { reduce(add, [0,1,2,3,4,5,6,7,8,9]) = 45 }

``tail(l)``
-----------

Returns *l* sans the first element.  Passing a list with zero elements is
an error.

::

    assert { tail([1,2,3]) = [2,3] }

..
    Some shortcuts, because I'm lazy.

.. |b_t| replace:: *b*\ :sub:`t`
.. |b_f| replace:: *b*\ :sub:`f`
.. |s_fmt| replace:: *s*\ :sub:`fmt`
.. |expr_t| replace:: *expr*\ :sub:`t`
.. |expr_f| replace:: *expr*\ :sub:`f`

