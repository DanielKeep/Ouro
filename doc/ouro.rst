
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
           └─'(++'───┬─╢ doc comment (n+1) ╟─┘
                     ├────────── * ──────────┘
                     └─'++)'─┐
                             ╧

Symbol
``````

These tokens are used for non-alphanumeric language keywords.

::

    symbol
        >>─┬──╢ multi character symbol ╟───┐
           ├──────╢ nesting symbol ╟─────┘ ╧
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
           ├─'(.)'─┘    - Function composition
           ├─'(.'──┘    - Infix/postfix opening parenthesis
           ├─'.)'──┘    - Infix closing parenthesis
           ├─'[:'──┘    - Map opening bracket
           ├─':]'──┘    - Map closing bracket
           ├─"#'"──┘    - Ast quote
           ├─'#"'──┘    - Ast quasi-quote
           └─'#$'──┘    - Ast quasi-quote escape (a.k.a. substitution)

    nesting symbol
        >>─┬─'('───┐
           ├─')'─┘ ╧
           ├─'['─┘
           ├─']'─┘
           ├─'{'─┘
           └─'}'─┘

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
           ├─':'─┘    - Reserved for constraints
           └─'.'─┘    - Lambda argument terminator

Keyword
```````

Keywords are names reserved by the language which cannot be used as
identifiers.

::

    keyword
        >>─┬─'and'───────────┐  - Binary logical and
           ├─'let'─────────┘ ╧  - Declaration statement
           ├─'not'─────────┘    - Unary logical not
           ├─'or'──────────┘    - Binary logical or
           ├─'mod'─────────┘    - Binary modulus
           ├─'rem'─────────┘    - Binary remainder
           ├─'true'────────┘    - Logical true
           ├─'false'───────┘    - Logical false
           ├─'nil'─────────┘    - Nil
           ├─'import'──────┘    - Module import statement
           ├─'macro'───────┘    - Macro keyword
           ├─'range'───────┘    - Range constructor
           └─'__builtin__'─┘    - Builtin lookup

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

    <let statement> =
        "let", [ "macro" ], <identifier>,
        [ "(", [ <function argument names> ], ")" ],
        "=", <expression>, <term>;

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

    <function expression> = <function prefix>, (
        "(", [ <expression>, { ",", <expression> } ], ")"
        | "{", [ <expression>, { ",", <expression> } ], "}" );

    <infix function> = <identifier>
                     | <sub expression>;

    <postfix function> = <infix function>;

    <function prefix> = <identifier>
                      | <function like keyword>
                      | <sub expression>
                      | <function expression>
                      ;

    <function like keyword> = "#'"
                            | `#"`
                            | "#$"
                            | "let"
                            | "import"
                            | "__builtin__"
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
``(.)``     Function composition        9.0     left
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
``or``      Logical disjunction         3.9     left
``and``     Logical conjunction         3.8     left
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

    Module : Node
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

Semantic Nodes
==============

The following describes the structure of the Semantic Information Tree (SIT)
nodes.  Note that unlike the AST, this is not part of the language
specification; provided an implementation matches the semantics, the exact
arrangement and implementation is irrelevant.

::

    Scope
        entries : [:String:Value:]
        parent  : Scope
        enclosed : Logical

A ``Scope`` is a mapping between identifiers and ``Value``\ s.  Each ``Scope``
may be linked to a parent ``Scope``.  A new ``Scope`` is created for each
module, import and function.

::

    PartialScope : Scope
        complete : Logical = false

These are used in cases where a complete list of symbols being defined in a
scope cannot be determined ahead of time.  An example of this would be
importing all symbols from a module; until the module has been processed, we
don't know what symbols it defines.

::

    abstract Node
        astNode : Ast Node

    Module : Node
        stmts : [Stmt]
        exportSymbols : [String]
        scope : Scope

    Stmt
        loc         : Location
        expr        : Expr
        bind        : Logical
        bindIdent   : String
        mergeAll    : Logical
        mergeList   : [String]

    abstract Expr : Node

    CallExpr : Expr
        funcExpr : Expr
        args     : [CallArg]

    CallArg
        expr    : Expr
        explode : Logical

    abstract Value : Expr

    UnfixedValue : Value
        scope : Scope
        ident : String

An ``UnfixedValue`` is produced by a ``Scope`` in cases where the actual value
is not yet known.

::

    DynamicValue

This is used for all ``UnfixedValue`` nodes which can have multiple values
during execution.

::

    ArgumentValue : UnfixedValue, DynamicValue

    EnclosedValue : Value
        value : UnfixedValue

An ``EnclosedValue`` wraps a ``DynamicValue`` used outside its defining
``Scope``.  For example, an ``EnclosedValue`` would be generated when ``x``
is used in the following::

    \x. \. x

::

    Resolvable

Used for any ``UnfixedValue`` which can have its actual value determined.
Defines a ``resolve`` method for this purpose.

::

    DeferredValue : UnfixedValue, Resolvable

A value which we know is defined somewhere, but which we haven't computed
yet.

::

    QuantumValue : UnfixedValue, Resolvable

A value which may or may not be defined at all.  Produced by
``PartialScope``\ s.

::

    RuntimeValue : Value, Resolvable
        expr : Expr

A value which is not available until runtime.

::

    AstQuoteValue : Value
        ast : Ast Node

    CallableValue : Value

    ClosureValue : CallableValue
        fn      : FunctionValue
        values  : [Value]

    FunctionValue : CallableValue
        name    : String
        args    : [Argument]
        scope   : Scope
        enclosedValues : [EnclosedValue]
        evalCtx = Mask('None, 'Compile, 'Runtime)
        expr    : Expr

Note that implementations will have to include additional fields to represent
functions provided by the implementation itself.  Currently, the reference
implementation allows for a function pointer.

::

    Argument
        loc      : Location
        ident    : String
        isVararg : Logical

    ListExpr : Expr
        elemExprs : [Expr]

    ListValue : Value
        elemValues : [Value]

    LogicalValue : Value
        value : Logical

    MapExpr : Expr
        kvps : [ExprKVP]

    MapValue : Value
        kvps : [ValueKVP]

    ExprKVP
        loc : Location
        key : Expr
        value : Expr

    ValueKVP
        loc : Location
        key : Value
        value : Value

    ModuleValue : Value
        module : Module

    NilValue : Value

    StringValue : Value
        value : String

    NumberValue : Value
        |-- Note: probably should have been called 'RealValue'
        value : Real

    RangeValue : Value
        incLower : Logical
        incUpper : Logical
        lowerValue : Value
        upperValue : Value

Semantic Analysis
=================

Once the AST has been produced, it must undergo semantic analysis.  This is
done by walking the tree top-down, turning it into a SIT.  This section
informally describes what these transformations are.

For some nodes, this simply involves copying the necessary information from
the AST note to the SIT node.  This includes simple literal expressions, for
example.  Others require more complex transforms.

The semantic analysis also requires some context be kept.  Contexts are passed
by reference, and contain the following information:

::

    Context
        scope       : Scope
        stmt        : Stmt
        builtinFn   : λ String. Value
        enclosedValues : [EnclosedValue]

..

    Note that the process of merging two sets of ``EnclosedValue``\ s is
    mentioned below.  Given ``ctx`` and ``subCtx``, it involves adding to
    ``ctx`` all ``EnclosedValue``\ s in ``subCtx`` which are not directly
    accessible from any ``Scope`` between ``ctx scope`` and ``subCtx scope``.

    In essence, it involves promoting all ``EnclosedValue``\ s from a function
    into its enclosing function if they cannot be satisfied by the enclosing
    function itself.

    For example, take the following code::

        \a. \b. \c. a+b+c

    The third function depends on ``a`` and ``b`` since both of these are
    ``DynamicValue``\ s which are not directly passed to it.  This means the
    second function depends on ``a`` since ``b`` *is* being directly passed.

Below is a description of the transformations that have to be performed on the
AST nodes.  The current node is called ``node`` and the current context is
called ``ctx``.  The meaning of *Eval* and *Fold* is explained later.

``Ast Module``
    - Create a new ``Scope`` and assign to ``ctx``.
    - Loop whilst there are un-processed statements.
        - For each ``stmt`` in ``node stmts`` which has not been processed:
            - Create a new ``Stmt`` and reference in ``ctx``.
            - Attempt to transform ``stmt`` into ``expr`` using ``ctx``.
            - If the attempt failed with a non-fatal error,
              skip this statement.
            - Attempt to *Fold* ``expr`` into ``expr'``.
            - If folding failed for any reason, skip this statement.
            - If ``expr'`` is a ``Value``, cast and assign to ``value``.
            - If ``expr'`` is not a ``Value``, wrap ``expr'`` in a
              ``RuntimeValue`` and assign to ``value``.
            - Handle any binding or merging with ``value``.
            - Result of the statement is ``value``.
            - Add statement to ``module``.
        - If every un-processed statement was skipped, fail.
    - Result is a ``Module`` containing the processed statements, exported
      symbols and scope.

``Ast ImportStmt``
    - Adjust ``ctx stmt`` to contain binding and merge information in ``node``.
    - Module path ``node modulePath`` is transformed into the equivalent of
      ``module(node modulePath)``.  This is the result.

``Ast LetExprStmt``
    - Adjust ``ctx stmt`` to contain binding information in ``node``.
    - Result is the transform of ``node expr``.

``Ast LetFuncStmt``
    - Adjust ``ctx stmt`` to contain binding information in ``node``.
    - Create a new ``Scope`` and assign to ``ctx scope``.
    - Create ``ArgumentValue`` bindings for the arguments in the new
      ``Scope``.
    - Transform ``node expr`` into the function's body.
    - Result is a new ``FunctionValue`` given the arguments, scope and body.

``Ast ExprStmt``
    - Result is the transform of ``node expr``.

``Ast RewrittenExpr``
    - Result is the transform of ``node rewrite``.

``Ast BinaryExpr``
    - Obtain a ``FunctionValue`` for the operator.
    - For operators other than ``and`` and ``or``:
        - Transform ``node lhs`` and ``node rhs``.
    - For ``and`` and ``or``:
        - Transform ``node lhs``.
        - Wrap ``node rhs`` in a lambda and transform it.
    - Result is a ``CallExpr`` of the operator with the ``lhs`` and ``rhs``.

``Ast TernaryExpr``
    - Obtain a ``FunctionValue`` for the operator.
    - Transform ``node``'s ``lhs``, ``mid`` and ``rhs``.
    - Result is a ``CallExpr`` of the operator with the ``lhs``, ``mid``
      and ``rhs``.

``Ast InfixFuncExpr``
    - Transform ``node``'s ``funcExpr``, ``lhs`` and ``rhs``.
    - Result is a ``CallExpr`` of ``funcExpr`` with ``lhs`` and ``rhs``.

``Ast PrefixExpr``
    - Obtain a ``FunctionValue`` for the operator.
    - Transform ``node``'s ``subExpr``.
    - Result is a ``CallExpr`` of the operator with ``subExpr``.

``Ast PostfixFuncExpr``
    - Transform ``node``'s ``funcExpr`` and ``subExpr``.
    - Result is a ``CallExpr`` of ``funcExpr`` with ``subExpr``.

``Ast NumberExpr``
    - Result is a ``NumberValue`` node with ``node value``.

``Ast StringExpr``
    - Result is a ``StringValue`` node with ``node value``.

``Ast LogicalExpr``
    - Result is a ``LogicalValue`` node with ``node value``.

``Ast NilExpr``
    - Result is a ``NilValue``

``Ast ListExpr``
    - Result is a ``ListExpr`` with the transformed elements of
      ``node elemExprs``.

``Ast MapExpr``
    - Result is a ``MapExpr``.  Each pair in ``node keyValuePairs`` is
      transformed and stored in a ``ExprKVP``.

``Ast LambdaExpr``
    - Copy ``ctx`` into ``subCtx``.
    - Clear the list of enclosed values in ``subCtx``.
    - Create a new ``Scope`` and store in ``subCtx``.
    - Create and bind arguments.
    - Transform ``node expr`` with ``subCtx`` into the body.
    - Result is a ``FunctionValue`` with the appropriate scope, args and body.
    - Merge the enclosed values of ``subCtx`` into ``ctx``.

``Ast ExplodeExpr``
    - This cannot be transformed.  Any semantically valid ``Ast ExplodeExpr``
      will be handled by the transformation of the surrounding
      ``Ast CallExpr``.

      If this is encountered directly, an error should be raised.

``Ast CallExpr``
    - Transform ``node funcExpr``.
    - If this is a non-macro call:
        - Transform each ``node argExpr``.  If the expression is an
          ``Ast ExplodeExpr``, transform ``argExpr subExpr`` instead and flag
          the argument as an explode.
        - Result is a ``CallExpr`` of the function expression itself with the
          arguments.
    - If this is a macro call:
        - Transform each ``node argExpr`` into an ``AstQuoteValue`` containing
          the original argument expression node.
        - *Eval* the function expression.  It must result in a
          ``FunctionValue``.
        - *Eval* the function with the transformed arguments.  It must result
          in an ``AstQuoteValue``.
        - Result is the result of transforming the above ``AstQuoteValue``.

``Ast VariableExpr``
    - Result is looked up via ``ctx``, given ``node ident``.
    - If the result is an ``EnclosedValue``, it is added to ``ctx``'s list of
      enclosed values.

``Ast RangeExpr``
    - Current implementation:
        - Result is the transform of the equivalent code
          ``range(#${node.incLower}, #${node.incUpper}, #${node.lowerExpr},
          #${node.upperExpr})``, where ``range`` is a ``RangeValue``-producing
          function.
    - Alternately:
        - Transform ``node lowerExpr`` and ``node upperExpr`` and produce a
          ``RangeExpr``.

    The reason the alternative isn't used is because it was done that way
    originally and I was too lazy to change it.

``Ast AstQuoteExpr``
    - Result is an ``AstQuoteValue`` containing ``node expr``.

``Ast AstQuasiQuoteExpr``
    - A reference to a quasi-quote substitution function is obtained.
    - ``node expr`` is rewritten to replace all ast substitution expressions
      with indexed substitutions.  Currently, these are represented as ``#$n``
      where ``n`` is the index and cannot be directly written in source.

      This rewriting process also extracts all the substitution expressions
      into an ordered list of ``AstQuoteValue``\ s.
    - Result is a ``CallExpr`` of the quasi-quote substitution function with
      the ordered list of substitution expressions as the arguments.

    Alternately, you could produce a specialised expression node.

``Ast AstQQSubExpr``
    - ``node expr`` is transformed into ``sitExpr``.
    - *Eval* ``sitExpr`` into ``value``.
    - ``value`` must be an ``AstQuoteValue``.
    - Result is the transform of ``value ast``.

``Ast LetExpr``
    - A reference to the builtin ``let`` macro ``FunctionValue`` is obtained.
    - Each expression in ``node bindExprs`` is transformed into an
      ``AstQuoteValue``.
    - The bind expressions are wrapped in a ``ListExpr``.
    - ``node subExpr`` is transformed into an ``AstQuoteValue``.
    - *Eval* the ``let`` macro, called with the bind list and sub expression
      ast.  Result must be an ``AstQuoteValue``.
    - Result is the transform of the *Eval*\ ed ``AstQuoteValue``.

``Ast ImportExpr``
    - Result is the transform of rewriting the node into::

        importFn(#${node scopeExpr}, #${node symbolsExpr}, #${node subExpr})

      ... and *Eval*\ ing it.

``Ast BuiltinExpr``
    - Result is obtained by calling the builtin lookup function in ``ctx``
      with ``node ident``.

Evaluation and Folding
----------------------

Evaluation and folding are very similar processes.  They are processes where
expressions are transformed.  In the case of evaluation, they are transformed
into a concrete value.  If a value cannot be produced for any reason,
evaluation fails.

In the case of folding, they are transformed either into a concrete value or a
simplified expression.  As much of the simplified expression is folded as
possible.

Folding is, in essence, compile-time partial evaluation.

Both of these processes are defined below as transforms of semantic nodes.
Differences between evaluation and folding are noted where they exist.

``Module``
    *Evaluation*
        Each statement of the ``Module`` is evaluated in order.  Any
        statements which contain a ``RuntimeValue`` have the expression
        evaluated and fixed; fixing it causes all future evaluations of the
        ``RuntimeValue`` to simply be substituted with the result of the
        expression.

        The result of the module is the value of the last statement.

    *Folding*
        A ``Module`` shouldn't be folded; it doesn't make any sense.

``CallExpr``
    Both the function expression and arguments are processed.

    *Folding*
        If any argument resolved to a ``RuntimeValue``, treat it as if it were
        an ``Expr``.

        If any of the above fail to resolve to a Value, a new ``CallExpr`` is
        returned with the result of processing the original expressions.

        If the function resolved to a value, but the function cannot be called
        at compile time, return a new ``CallExpr`` as above.

    The function value is called with the argument values and the result
    returned.

``ArgumentValue``, ``EnclosedValue``, ``DeferredValue``, ``QuantumValue``
    Look up in ``ctx`` and return.

``RuntimeValue``
    *Evaluation*
        Look up in ``ctx`` and return.

    *Folding*
        Return the node un-modified.

``AstQuoteValue``, ``ListValue``, ``MapValue``, ``ModuleValue``, ``NilValue``, ``StringValue``, ``NumberValue``
    Return the node un-modified.

``ClosureValue``
    Process all the closure's values and return a new ``ClosureValue`` with
    them.

``FunctionValue``
    If there are no enclosed values, return the node un-modified.

    Otherwise, create a closure by looking up the enclosed values in ``ctx``.

``ListExpr``, ``MapExpr``
    Process all sub-expressions and return a ``Value``.

    *Folding*
        If any sub-expression results in an ``Expr`` or ``RuntimeValue``,
        produce a new ``Expr`` instead.

Module Paths
============

In Ouro, there is a 1:1 correspondence between modules and files, unlike C++
or C#.

A module value may be obtained using the ``module`` function, passing
the path to the module like so::

    let ast = module("/ouro/ast")

The path passed is actually a URI, both relative and absolute URIs are
supported.  Relative URIs are evaluated relative to the module's parent.  For
example, the path ``../baz`` used from within the module ``/foo/bar`` resolves
to ``/baz``.

All URIs which do not specify the scheme default to the ``module`` scheme.
Other schemes may be specified, although there is no requirement for schemes
other than ``module``.  Nevertheless, standard behaviour for some schemes is
provided below.

Resolving a ``module`` URL to an actual resource depends on the import roots.
This is an ordered list of absolute URIs used to resolve ``module`` URLs.
For example, a typical Ouro environment running in a UNIX environment might
have the following root URIs:

    - ``file://$CD``
    - ``file://$HOME/.ouro/lib``
    - ``file:///usr/lib/ouro``

Resolving a ``module`` URL involves combining the path of the ``module`` URL
with the root URI and determining if the result is valid. If valid, the
resulting URI is returned as the result; otherwise, the next root is tried.

URIs and File Extensions
------------------------

Simply appending a ``module`` URL to a root URI is generally not sufficient to
locate the actual module resource.  Modules will typically be stored as files
with file extensions determining format.

As such, combining a ``module`` URL with a root URI will often require
"mangling" the path and then checking for existence.

Below is a list of possible mangles.  Implementations are free to support
others.  The only required mangle is the one which adds the ``.ouro`` file
extension.

- ``module.ouro`` - Ouro source files.
- ``module.osem`` - Folded Ouro module semantic tree.
- ``module.obc`` - Compiled Ouro bytecode.

URI Schemes
-----------

``file`` URLs
`````````````

``file`` URLs behave as expected.

``zip`` URLs
````````````

``zip`` URLs are of the form::

    zip:url

The ``url`` portion specifies a URL to another resource which must be a ZIP
archive.

See the note on packages for additional details.

Packages
--------

Ouro modules may be stored in a package.  These include things such as
compressed archives.

Packages may contain a file called *TODO* which contains metadata on the
package.  This includes a path prefix.

**TODO**: work out details.

Execution requirements
======================

Tail call elimination.

Builtin Functions and Macros
++++++++++++++++++++++++++++

``ouro``
========

The ``ouro`` module is implicitly imported and merged into every module's
scope.  Thus, it contains the "built-in" functions and macros.

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

``filter(f, l)``
----------------

Returns all elements from *l* for which ``f(l)`` is ``true``.

Note that order of evaluation is *not* specified.

::

    let filter(f, l) = (
        if {
            l = [],
            [],
            let {
                [l', head(l)],
                [ls, tail(l)],

                if { f(l'),
                     l' :: filter(f, ls),
                     filter(f, ls) }
            }
        }
    )

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

    The format of *a* is one of ``p<w``, ``p|w`` or ``p>w``.

    *p*
        This is optional and specifies the padding to use.
        It may be either ``0`` or a substitution of the
        form ``$*``, ``${}``, ``$n``, ``$ident``, ``${n}`` or
        ``${ident}``.

        If it is ``0``, the result is padded with literal zeroes.

        If it is a substitution, then the result of that substitution is
        used as the padding.

        If it is omitted, space is used for padding.

    ``<``, ``|``, ``>``
        These are used to specify left-, centre- or right-alignment.

        It may be omitted if *p* is not specified; in which case,
        right-alignment is the default.

    *w*
        Specifies the width of the area into which the result should be
        aligned.  It may be either a non-negative integer or a
        substitution of the form ``$*``, ``${}``, ``$n``, ``$ident``,
        ``${n}`` or ``${ident}``.

``${n;p}``, ``${ident;p}``
    As above, but also specifies precision.

    **TODO**.

``${n:f}``, ``${ident:f}``
    As above, but also uses the format option *f*.  Format options are
    specific to the exact value being substituted.  Some common format
    options are listed below.

    **TODO**: decide how to handle substitutions inside the format
    options.

    -   General

        ``R``
            Substitutes the value's representation.

    -   Logical

        ``(?t,f)``
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

        ``(en)``, ``(En)``
            Uses scientific notation as above.  Forces the exponent to be
            *n* digits wide.

        ``o``
            Represents the number in octal.

        ``(p:s_0,s_1,...)``
            Substitutes *s*\ :sub:`0`, *s*\ :sub:`1`, ... based on the
            plurality of the number.

            **TODO**: clarify how substitutions work with this.

        ``r``
            Rounds the number to the nearest integer.

        ``(rR)``
            Rounds the number based on the value of *R*.

        ``x``, ``X``
            Represents the number in hexadecimal.  The case determines the
            case of the non-decimal digits.

        ``,``, ``_``
            Inserts a separator (either a ``,``\ [*]_ or ``_``) between
            every 3 digits, counting out from the decimal place.

        ``(,n)``, ``(_n)``
            Inserts a separator as above; instead of every 3 digits, it
            inserts it every *n* digits, where *n* is a positive integer.

    -   Strings

        ``e``
            Prints the string with all non-printable characters escaped.

        ``l``
            Substitutes the length of the string in code points.

        ``q``
            Prints the string quoted as a string literal.

    -   Lists

        ``:f...``
            Uses *f...* as the format options for elements.

        ``l``
            Substitutes the length of the list.

        ``r``
            Raw formatting: formats all elements without brackets,
            commas or spacing.

        ``(sS...)``
            Uses *S...* as the separator between elements.

    -   Maps

        ``:(fk...):(fv...)``
            Uses *fk...* and *fv...* as the format options for keys and
            values respectively.

        ``l``
            Substitutes the number of elements in the map.

        ``(spS...)``
            Uses *S...* as the separator between key/value pairs.

        ``(seS...)``
            Uses *S...* as the separator between elements.

``${x,a;p}``, ``${x,a:f}``, ``${x;p:f}``, ``${x,a;p:f}``
    Valid combinations of the above.  *x* is either a non-negative integer
    or an identifier.

.. [*]  The actual character used may be modified by culture settings,
    although how this is performed is as-yet undefined.

``head(l)``
-----------

Returns the first element of the list *l*.  Passing a list with zero
elements is an error.

::

    let head(l) = (
        let {
            [head', \l', l's... . l'],

            head'(l...)
        }
    )

``if{l, expr_t, expr_f}``
-------------------------

Evaluates and returns the result of |expr_t| or |expr_f|
based on the value of *l*.  The branch not chosen is not evaluated.

::

    let macro if(l, expr_t, expr_f) = (
        #"{ branch( #${l}, \.#${expr_t}, \.#${expr_f} ) }
    )

``map(f, l)``
-------------

Transforms the elements of *l* by passing them through *f*.

Note that order of evaluation is *not* specified.

::

    let map(f, l) = (
        if {
            l = [],
            [],
            let {
                [l', head(l)],
                [ls, tail(l)],

                f(l') :: map(f, ls)
            }
        }
    )

``reduce(f, l)``
----------------

Reduces the list *l* to a single value.  It has the same effect as if the
expression ``l_0 (.f.) l_1 (.f.) ... (.f.) l_n`` was evaluated, with
``l_0``, ``l_1``, ..., ``l_n`` being elements of *l*.

Note that order of evaluation is *not* specified.

::

    let reduce(f, l) = (
        let {
            [l', head(l)],
            [ls, tail(l)],

            if {
                ls = [],
                l',
                f(l', reduce(f, ls))
            }
        }
    )

``tail(l)``
-----------

Returns *l* sans the first element.  Passing a list with zero elements is
an error.

::

    let tail(l) = (
        let {
            [tail', \l', l's... . l's],

            tail'(l...)
        }
    )

..
    Some shortcuts, because I'm lazy.

.. |b_t| replace:: *b*\ :sub:`t`
.. |b_f| replace:: *b*\ :sub:`f`
.. |s_fmt| replace:: *s*\ :sub:`fmt`
.. |expr_t| replace:: *expr*\ :sub:`t`
.. |expr_f| replace:: *expr*\ :sub:`f`

