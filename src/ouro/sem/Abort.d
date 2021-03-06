/**
    Semantic analysis abortion exceptions.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.Abort;

import Sit = ouro.sit.Nodes;

/*
    Root class for all semantic failures.
*/
class SemanticAbort : Exception
{
    this()
    {
        super("semantic abort");
    }

    this(char[] msg)
    {
        super(msg);
    }
}

/*
    NonFatalAbort instances are thrown to indicate that processing a given
    subtree is not possible yet and that this is not a fatal, unrecoverable
    error.
 */
class NonFatalAbort : SemanticAbort
{
    protected this(char[] msg)
    {
        super(msg);
    }

    static void throwForUnfixed(Sit.UnfixedValue value)
    {
        throw new UnfixedValueAbort(value);
    }
}

/*
    Thrown in cases where an unfixed value was encountered.  This means the
    value hasn't been evaluated or defined yet.
*/
class UnfixedValueAbort : NonFatalAbort
{
    Sit.UnfixedValue value;

    this(Sit.UnfixedValue value)
    {
        auto ast = value.astNode;
        super("encountered unfixed value '"
                ~ value.ident ~ "'"
                ~ (ast !is null
                    ? " @ " ~ ast.loc.toString
                    : ""
                    ));
        this.value = value;
    }
}

/*
    Used to indicate that a function call cannot be executed at compile time
    and must be delayed until runtime.
*/
class EarlyCallAbort : NonFatalAbort
{
    this()
    {
        super("must delay call");
    }
}

/*
    Thrown in instances when semantic analysis cannot be completed at all.
*/
class FatalAbort : SemanticAbort
{
    this()
    {
        super("fatal semantic abort");
    }

    this(char[] msg)
    {
        super(msg);
    }
}

/*
    Used to indicate that someone tried to call a compile-time function in a
    runtime context.
*/
class LateCallAbort : FatalAbort
{
    this()
    {
        super("tried to call a compile-time function in a runtime context");
    }
}

/*
    Indicates that expansion of an ast mixin failed.
*/
class MixinEvalFailedAbort : FatalAbort
{
    this()
    {
        super("couldn't evaluate macro/ast mixin");
    }
}

