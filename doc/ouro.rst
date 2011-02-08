
====
Ouro
====

:Author: Daniel Keep <daniel.keep@gmail.com>

.. contents::

Introduction
++++++++++++

Execution of Ouro programs
++++++++++++++++++++++++++

Source code
===========

Ouro source code is stored as text using the UTF-8 encoding.  The UTF-8 byte
order mark is optional and is not considered part of the source text itself.

Lexical analysis
================

The first step in executing a Ouro program is to perform lexical analysis of
the source code.  This transforms the source from a sequence of characters
into a sequence of distinct tokens.

The following diagrams represent the tokenisation process.  In ambiguous
cases, the earliest branch always wins.  Note that this is not necessarily
equivalent to "maximal munch".

For example, a DOS-style newline (``"\x0d\x0a"``) is always lexed as a single
newline token as opposed to two.

Lexical Structure
-----------------

Whitespace
``````````

::

    whitespace
        >>─┬────U+20───────┐
           ├────U+09─────┘ ╧
           ├────U+0B─────┘
           ├────U+0C─────┘
           └─╢ comment ╟─┘

Note that whitespace does not form a distinct lexeme.  For the purposes of
parsing the grammatical structure of the program, whitespace is never included
in the token stream.

However, it may be useful to retain all whitespace tokens for round-tripping
source.  This can be done by associating a list of whitespace tokens with each
"proper" token included in the final token stream.

End of line
```````````

End of line tokens are slightly magical.  They are syntactically important at
the statement level, but not so within subexpressions or function calls.
Since the parsing stage needs them in *some* circumstances, they must produce
actual tokens.

On the other hand, if you are preserving whitespace, they need to be included
in the list of whitespace tokens attached to "proper" tokens.

The recommended strategy, in this case, is to emit ``eol`` tokens to the token
stream *and* to any whitespace token list, without the ``eol`` token consuming
currently accumulated whitespace.

::

    eol
        >>─┬─U+0D─U+0A───┐
           ├────U+0D───┘ ╧
           └────U+0A───┘

End of source
`````````````

::

    eos
        >>─( end of source text )─┐
                                  ╧

``eos`` should only match at the end of the input.

Comments
````````

Comments are treated, at the lexical stage, as whitespace.  The parsing stage
may choose to treat ``doc comment`` tokens specially, however.

Nevertheless, they do not change the structure of the program under any
circumstances.

::

    comment
        >>─┬───╢ line comment ╟──────┐
           ├─╢ block comment (0) ╟─┘ ╧
           └──╢ doc comment (0) ╟──┘ 

    line comment
        >>─'|--'───┬─╢ eol ╟─┐
                 | └─ * ─┐   ╧
                 └───────┘

    block comment (n)
        >>─┐       ┌─────────────────────────────┐
           └─'(--'───┬─╢ block comment (n+1) ╟───┘
                     ├──────────── * ────────────┘
                     └─'--)'─┐
                             ╧

    doc comment (n)
        >>─┐       ┌─────────────────────────┐
           └─'(--'───┬─╢ doc comment (n+1) ╟─┘
                     ├────────── * ──────────┘
                     └─'--)'─┐
                             ╧

Symbol
``````

These tokens are used for non-alphanumeric language keywords.

::

    symbol
        >>─┬──╢ multi character symbol ╟───┐
           ├──────╢ nesting symbol ╟─────┘ X
           └─╢ single character symbol ╟─┘

    multi character symbol
        >>─┬─'!='────┐  - Inequality
           ├─'//'──┘ ╧  - Binary integer division
           ├─'**'──┘    - Binary exponentiation
           ├─'<='──┘    - Less-than or equal
           ├─'>='──┘    - Greater-than or equal
           ├─'<>'──┘    - Inequality
           ├─'::'──┘    - Sequence cons
           ├─'++'──┘    - Sequence join
           ├─'...'─┘    - Sequence explode
           ├─'(.'──┘    - Infix/postfix opening parenthesis
           ├─'.)'──┘    - Infix closing parenthesis
           ├─'[:'──┘    - Map opening bracket
           ├─':]'──┘    - Map closing bracket
           ├─"#~'"─┘    - Ast quote
           ├─'#~"'─┘    - Ast quasi-quote
           └─'#~$'─┘    - Ast quasi-quote escape (a.k.a. substitution)

    nesting symbol
        >>─┬─'('───┐
           ├─')'─┘ ╧
           ├─'['─┘
           └─']'─┘

    single character symbol
        >>─┬─'='───┐  - Equality
           ├─','─┘ ╧  - Argument separator
           ├─'+'─┘    - Prefix positive, binary addition
           ├─'-'─┘    - Prefix negation, binary subtraction
           ├─'/'─┘    - Binary division
           ├─'*'─┘    - Binary multiplication
           ├─'<'─┘    - Less-than
           ├─'>'─┘    - Greater-than
           ├─'\'─┘    - Lambda introduction
           ├─':'─┘    - Lambda argument terminator
           └─'.'─┘    - Function composition, postfix and infix syntax

Keyword
```````

Keywords are names reserved by the language which cannot be used as
identifiers.

::

    keyword
        >>─┬─'and'──────┐   - Binary logical and
           ├─'let'────┘ ╧   - Declaration statement
           ├─'not'────┘     - Unary logical not
           ├─'or'─────┘     - Binary logical or
           ├─'mod'────┘     - Binary modulus
           ├─'rem'────┘     - Binary remainder
           ├─'true'───┘     - Logical true
           ├─'false'──┘     - Logical false
           ├─'nil'────┘     - Nil
           ├─'import'─┘     - Module import statement
           ├─'macro'──┘     - Macro keyword
           └─'range'──┘     - Range constructor

Identifier
``````````

Identifiers are used to name and refer to variables and functions.

Identifiers can take one of three forms:

Basic
    A basic identifier is one comprised of alphanumeric characters (plus
    underscore) and starting with an alpha character or underscore.  This
    broadly matches the definition of an identifier in, for example, the C
    programming language.

Literal
    A literal identifier is written as a dollar sign followed immediately by a
    string literal.  This is used to write arbitrary identifiers that may not
    be possible to otherwise include.

    Generally, this should only be used in extreme circumstances or macro
    programming.  This syntax makes it possible to create identifiers that the
    implementation might be using internally.

External
    An external identifier is introduced by a dollar sign.  The identifier
    can contain any combination of valid basic identifier characters,
    single character symbols and parenthesis (provided the parentheses are
    balanced).

::

    identifier
        >>─┬─╢ ident start ╟───╢ ident ╟─┬───┐
           │                 └───────────┘ │ ╧
           ├─'$'─╢ string ╟────────────────┘
           └─'$'─╢ external ident ╟────────┘

    ident start
        >>─┬─╢ letter ╟───┐
           └─────'_'────┘ ╧

    ident
        >>─┬─╢ ident start ╟───┐
           ├────╢ digit ╟────┘ ╧
           ├───────`'`───────┘
           ├───────'$'───────┘
           ├───────'|'───────┘
           ├───────'?'───────┘
           ├───────'!'───────┘
           └───────'~'───────┘

Externals might need rethinking...

::

    external ident
        >>─┐     ┌─────────────────────────────────┐
           └─'('───┬─'('─╢ external ident ╟─')'────┴─')'─┐
                   ├─────────╢ ident ╟───────────┘       ╧
                   └─╢ single character symbol ╟─┘

    letter
        >>─( Unicode character classes L* )─┐
                                            ╧

    digit
        >>─( Unicode character classes Nd )─┐
                                            ╧

Real number literal
```````````````````

::

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

String literal
``````````````

::

    string
        >>─'"'───+─'\'─╢ escape ╟─┬─'"'─┐
               │ └────── * ───────┐     ╧
               └──────────────────┘

    escape
        >>─┬─'U'─╢ hex digit * 8 ╟───┐
           ├─'u'─╢ hex digit * 4 ╟─┘ ╧
           ├─'x'─╢ hex digit * 2 ╟─┘
           ├──────────'a'──────────┘
           ├──────────'b'──────────┘
           ├──────────'f'──────────┘
           ├──────────'n'──────────┘
           ├──────────'r'──────────┘
           ├──────────'t'──────────┘
           ├──────────'v'──────────┘
           ├──────────'''──────────┘
           ├──────────'"'──────────┘
           ├──────────'?'──────────┘
           └──────────'\'──────────┘

    hex digit
        >>─┬─╢ digit ╟───┐
           ├──'a..f'───┘ ╧
           └──'A..F'───┘

Parsing
=======

Parsing is the process by which the sequence of tokens is transformed into an
abstract symbol tree (AST).

It must be noted that all syntax forms fall into one of two categories: basic
syntax and derived syntax.  Derived syntax forms are alternate representations
of some basic syntax form.  When encountered, they are rewritten into the
equivalent basic form before being added to the AST.

For example, the syntax ``a + b`` is a derived form equivalent to
``$"+"(a,b)``; that is, calling the function ``+`` with arguments ``a`` and
``b``.

Also note that the grammar is context-dependent: the interpretation of
end of line tokens changes depending on whether or not the given production is
*inside* any form of nesting.  This is denoted by the following syntax::

    <treat eol as whitespace( X )>

Where ``X`` are the productions for which the ``eol`` token should be treated
as a ``whitespace`` token.

The following EBNF productions describe the grammatical structure of the language.

There are probably inconsistencies between what is described here and what is
actually implemented.  It needs a once-over to bring the two together (right
now, the code is canonical).

::

    <script> = { <statement> };

    <statement> = <empty statement>
                | <import statement>
                | <let statement>
                | <expression statement>
                ;

    <term> = <eol> | <eos>;

    <empty statement> = <term>;

    <import statement> =
        "import", [ <identifier>, "=" ], <string>,
            [ ":", ( <import identifier>, { ",", <import identifier> }
                   | "*"
                   ) ],
            <term>;

    <import identifier> = <identifier>;

    <let statement> = (
          "let", <identifier>, "=", <expression>
        | "let", [ "macro" ], <identifier>,
            "(", [ <function argument names> ], ")", "=", <expression>
        ),
        <term>;

    <function argument names> = <argument name>, { ",", <argument name> };

Note: eventually, pattern matching should be added here::

    <argument name> = <identifier>, [ "..." ];

    <expression statement> = <expression>, <term>;

    <expression> = <expression atom>,
                   { <binary op>, <expression atom> },
                   [ <postfix op> ];

    <expression atom> = [ <prefix op> ],
                            ( <number expression>
                            | <string expression>
                            | <logical expression>
                            | <nil expression>
                            | <list expression>
                            | <map expression>
                            | <lambda expression>
                            | <range expression>
                            | <function expression>
                            | <variable expression>
                            | <sub expression>
                            ),
                        [ <explode> ];

    <binary op> = "=" | "!=" | "<>"
                | "<" | "<=" | ">" | ">="
                | "+" | "-" | "*" | "/" | "//"
                | "mod" | "rem"
                | "**"
                | "and" | "or"
                | "." | "::" | "++"
                | "(.", <infix function>, ".)"
                ;

    <prefix op> = "+" | "-" | "not";

    <postfix op> = "(.", <postfix function>, ")";

    <explode> = "...";

    <number expression> = <number>;

    <string expression> = <string>;

    <logical expression> = "true" | "false";

    <nil expression> = "nil";

    <list expression> = "[", [ <expression>, { ",", <expression> } ], "]";

    <map expression> = "[:",
        [ <key value pair>, { ",", <key value pair> } ], ":]";

    <key value pair> = <expression>, ":", <expression>;

    <lambda expression> = "\", [ "macro" ], [ <function argument names> ],
        ".", <expression>;

    <function expression> = [ "macro" ], <function prefix>,
                            "(", [ <expression>, { ",", <expression> }], ")";

    <infix function> = <identifier>
                     | <sub expression>;

    <postfix function> = <infix function>;

    <function prefix> = <identifier>
                      | <function like keyword>
                      | <sub expression>
                      | <function expression>
                      ;

    <function like keyword> = "#~'"
                            | `#~"`
                            | "#~$"
                            | "let"
                            | "import"
                            ;

    <variable expression> = <identifier>;

    <range expression> = "range",
        ( "[" | "(" ), <expression>, ",",
        <expression>, ( "]" | ")" );

    <sub expression> = "(", <treat eol as whitespace( expression )>, ")";

Binary Operators
----------------

Operator precedence is expressed as a decimal number.  Operators are evaluated
before other operators with lower precedence.  This is expressed in the AST by
the arrangement of nodes.  For example, addition and multiplication have
precedences of 6.2 and 6.5 respectively; multiplication is always evaluated
before addition.

Also of note is the associativity (or fixity) of the operators.  This
determines whether they are left-associative or right-associative.  For
example, assuming a generic operator ∗.

=========== =================== ===================
Expression  Left-Associative    Right-Associative
=========== =================== ===================
a ∗ b ∗ c   (a ∗ b) ∗ c         a ∗ (b ∗ c)
=========== =================== ===================

=========== =========================== ======= ======= ===============
Symbol      Meaning                     Prec.   Assoc.  Alternatives
=========== =========================== ======= ======= ===============
``.``       Function composition        9.0     left
``**``      Exponentiation              6.7     right
``*``       Multiplication              6.5     left
``/``       Division                    6.5     left
``//``      Integer division [*]_       6.5     left
``mod``     Modulus [*]_                6.5     left
``rem``     Remainder [*]_              6.5     left
``+``       Addition                    6.2     left
``-``       Subtraction                 6.2     left
``::``      Sequence construction       5.6     right
``++``      Sequence join               5.4     left
``=``       Equality                    4.0     left
``!=``      Inequality                  4.0     left    ``<>``
``<``       Less-than                   4.0     left
``<=``      Less-than or equal-to       4.0     left
``>``       Greater-than                4.0     left
``>=``      Greater-than or equal-to    4.0     left
``and``     Logical conjunction         3.9     left
``or``      Logical disjunction         3.8     left
``(.f.)``   Infix function              -∞      left
=========== =========================== ======= ======= ===============

.. [*] ``x // y = floor(x / y)``
.. [*] ``x mod y = x - y*floor(x / y)``
.. [*] ``x rem y = x - y*trunc(x / y)``

Comparison operators also support "ternary syntax".  That is, the expression
``a < x < b`` is rewritten to ``a < x and x < b``.  For this to work, both
comparison operators must be "pointing" in the same direction.  That is, you
can mix ``<`` and ``<=`` or ``>`` and ``>=``, but you cannot mix ``<`` and
``>``.

Abstract Symbol Tree
====================

The following describes the structure of the AST nodes themselves.

::

    Node (abstract)
        loc : Location

    Program : Node
        stmts : Statement*

    Statement : Node

    ImportStmt : Statement
        modulePath : String
        ident : String
        all : Logical           |-- import all symbols?
        symbols : [String]

    LetStmt : Statement (abstract)
        ident : String
        expr : Expr

    LetExprStmt : LetStmt

    LetFuncStmt : LetStmt
        args : [Argument]
        expr : Expr

    Argument
        loc : Location
        ident : String
        isVararg : Logical

    ExprStmt : Statement
        expr : Expr

    Expr : Node (abstract)

    RewrittenExpr : Expr
        original : Node
        rewrite : Expr

    BinaryExpr : Expr
        op : ("Eq" | "Ne" | "Lt" | "LtEq" | "Gt" | "GtEq"
              | "Add" | "Sub" | "Mul" | "Div" | "IntDiv" | "Mod" | "Rem"
              | "Exp" | "And" | "Or" | "Comp" | "Cons" | "Join" )
        lhs : Expr
        rhs : Expr

    TernaryExpr : Expr
        op : ("LtLt" | "LeLt" | "LtLe" | "LeLe"
              | "GtGt" | "GeGt" | "GtGe" | "GeGe" )
        lhs : Expr
        mid : Expr
        rhs : Expr

    InfixFuncExpr : Expr
        func : Expr
        lhs : Expr
        rhs : Expr

    PrefixExpr : Expr
        op : ("Pos" | "Neg" | "Not")
        subExpr : Expr

    PostfixFuncExpr : Expr
        func : Expr
        subExpr : Expr

    NumberExpr : Expr
        value : Real

    StringExpr : Expr
        value : String

    LogicalExpr : Expr
        value : Logical

    NilExpr : Expr

    ListExpr : Expr
        elemExprs : [Expr]

    MapExpr : Expr
        keyValuePairs : [KeyValuePair]

    KeyValuePair
        loc : Location
        key : Expr
        value : Expr

    LambdaExpr : Expr
        isMacro : Logical
        args : [Argument]
        expr : Expr

    ExplodeExpr : Expr
        seqExpr : Expr

    CallExpr : Expr
        isMacro : Logical
        funcExpr : Expr
        argExprs : [Expr]

    VariableExpr : Expr
        ident : String

    RangeExpr : Expr
        incLower : Logical
        incUpper : Logical
        lowerExpr : Expr
        upperExpr : Expr

    AstQuoteExpr : Expr
        expr : Expr

    AstQuasiQuoteExpr : Expr
        expr : Expr

    AstQQSubExpr : Expr
        expr : Expr

    LetExpr : Expr
        bindExprs : [Expr]
        subExpr : Expr

    ImportExpr
        scopeExpr : Expr
        symbolsExpr : Expr
        subExpr : Expr

AST Refinement
==============

Once the AST has been produced, it must be refined.  To do this, the AST nodes
are walked top-down, with the following transformations taking place.

Syntax rewriting
----------------

Some syntax forms only exist as an intermediary step in the AST and are
rewritten into expressions.

The rules are given below.

Statements
``````````

Note that ``tail`` stands for the remainder of the statements following the one
being rewritten.

::

    import "path"
        --> import(module("path"), nil, tail)

    import "path" : ident...
        --> import(module("path"), [ident...], tail)

    import ident = "path"
        --> let([ident, module("path")], tail)

    let ident = expr
        --> let([ident, expr], tail)

    let ident(arg...) = expr
        --> let([ident, \ args... . expr], tail)

    expr
        --> do(expr, tail)

Note that the final expression statement, assuming it is not empty, is not
rewritten.  This is why the "result" of a given program is the value of the
final expression statement.

Expressions
```````````

::

    range {[|(} lower upper {]|)}
        --> range({true|false}, {true|false}, lower, upper)

Macro expansion
---------------

Some functions in Ouro are actually macros.  A macro's arguments are passed as
an AST as opposed to a computed value.

To facilitate this, each function call is checked to determine whether the
function is a macro or not.  This obviously requires that the function be
determinable at compile-time.

When a macro is found, it is invoked with its arguments passed to it as ASTs.
The result of the macro is expected to be an AST, which is then inserted into
the containing AST.  The AST walk is resumed at the root of the inserted AST.

Lambda substitution
-------------------

Numerous syntax forms in Ouro are defined in terms of anonymous functions.  As
an example, consider the following macro which evaluates an expression once
and substitutes it into another expression::

    let macro fix(name, value, expr) = #~"(
        (\#~$(name): #~$(expr))(#~$(value))
    )

Given the following code::

    fix(x, 42, 2*x)

The expansion is::

    (\x: 2*x)(42)

Whilst this will execute with the expected semantics, it is inefficient.  It
could be further rewritten into the more efficient (both time and space)
form::

    2*42

A more complex example is the ``cond`` macro.  The expansion of::

    cond(x,
        [0, "foo"],
        [1, "bar"],
        [else, "?"])

Is::

    (\a: if(a = 0, "foo",
        (\b: if(b = 1, "bar",
            (\c: if(true, "?", nil))(b)))(a)))(x)

Here, the ``cond`` macro has created several anonymous functions in order to
safely preserve semantics.  However, it can be rewritten, in several steps,
as::

    if(x = 0, "foo",
        (\b: if(b = 1, "bar",
            (\c: if(true, "?", nil))(b)))(x))

    if(x = 0, "foo", if(x = 1, "bar",
        (\c: if(true, "?", nil))(x)))

    if(x = 0, "foo", if(x = 1, "bar", if(true, "?", nil)))

In order to apply lambda substitution, all arguments to the lambda must be
either a literal value or a variable lookup.

Constant folding
----------------

Constant folding is the process by which expressions may be replaced with
their literal value.  A simple example would be::

    let twoPi = 2pi

Here, there is no benefit to calculating the value of ``twoPi`` at runtime;
the compiler can safely replace the above code with the equivalent::

    let twoPi = 6.283185307179586476925286766559

An expression is folded if any of the following statements are true; note that
expressions are folded bottom-up, meaning that when these rules are applied,
all sub-expressions which can be folded have already been folded.

- The expression is a lookup to a known variable, the value of which is a
  literal value.

- The expression is a binary operation between literal values.

- The expression is a call to a known function with literal arguments.  The
  function *MUST NOT* have side-effects.

Note that this folding includes special forms such as the ``if`` construct.

Execution requirements
----------------------

Tail call elimination.

