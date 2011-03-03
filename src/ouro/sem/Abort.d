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
    this()
    {
        super("non-fatal semantic abort");
    }

    this(char[] msg)
    {
        super(msg);
    }

    static void throwForUnfixed(Sit.UnfixedValue value)
    {
        throw new NonFatalAbort;
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
        super("encountered unfixed value '" ~ value.ident ~ "'");
        this.value = value;
    }
}

