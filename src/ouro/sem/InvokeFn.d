/**
    Used to invoke a function object.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.InvokeFn;

import ouro.sem.Abort;

import Eval = ouro.sem.Eval;
import Sit  = ouro.sit.Nodes;

Sit.Value invoke(Sit.CallableValue callable, Sit.Value[] args,
        Sit.EvalContext evalCtx = Sit.EvalContext.Runtime)
{
    Sit.Value result = null;

    do
    {
        assert( callable !is null );

        Sit.FunctionValue fn;
        Sit.Value[] clValues;

        if( auto fnv = cast(Sit.FunctionValue) callable )
            fn = fnv;
        else if( auto clv = cast(Sit.ClosureValue) callable )
        {
            fn = clv.fn;
            clValues = clv.values;
        }
        else
            assert(false, "tried to invoke a " ~ callable.classinfo.name);

        // Process default argument values.
        void setArg(size_t i, Sit.Value v)
        {
            if( i >= args.length )
                args.length = i+1;
            assert( args[i] is null );
            args[i] = v;
        }

        // Go backwards so we only grow the array at most once.
        foreach_reverse( i, arg ; fn.args )
        {
            if( arg.defaultValue !is null
                    && (i >= args.length || args[i] is null) )
                setArg(i, arg.defaultValue);
        }

        foreach( i, arg ; args )
            assert( arg !is null, "arg "~fn.args[i].ident~" has no value" );

        // Handle callee-side varargs.  This means searching for any argument
        // which is flagged as vararg.
        {
            bool gotVa = false;
            size_t vaIdx = args.length;
            size_t vaBeg = vaIdx, vaEnd = vaIdx, vaLen = 0;

            foreach( i,fnArg ; fn.args )
            {
                if( fnArg.isVararg )
                {
                    if( gotVa )
                        assert( false, "cannot have more than one vararg" );
                    gotVa = true;
                    vaIdx = i;
                }
            }

            if( gotVa )
            {
                // We might have gotten fewer arguments than we needed.  Keep
                // in mind that the vararg argument can be a zero-width list.
                if( args.length < fn.args.length - 1 )
                    assert(false, "not enough arguments for " ~ fn.name );

                // Make a copy of args
                args = args.dup;

                // Compute start and end of the vararg segment
                vaBeg = vaIdx;
                vaLen = (args.length - vaBeg) - (fn.args.length - vaBeg - 1);
                vaEnd = vaBeg + vaLen;

                // Create vararg list
                auto vaListElems = new Sit.Value[vaLen];
                foreach( i,val ; args[vaBeg..vaEnd] )
                    vaListElems[i] = val;
                auto vaList = new Sit.ListValue(null, vaListElems);

                // Adjust arg list
                if( vaLen == 0 )
                {
                    // There was nothing in the vararg portion of the argument
                    // list.  We need to insert it in the middle.
                    args = args[0..vaBeg] ~ cast(Sit.Value) vaList
                        ~ args[vaBeg..$];
                }
                else if( vaBeg == args.length )
                {
                    // This means that the caller only supplied enough arguments
                    // for the non-vararg portion.  We'll just add a new one.
                    args ~= vaList;
                }
                else
                {
                    args[vaBeg] = vaList;

                    foreach( i,val ; args[vaEnd..$] )
                        args[vaBeg+i+1] = val;

                    args = args[0..$-(vaLen-1)];
                }
            }
        }

        // Did we get the right number of arguments?
        if( args.length != fn.args.length )
        {
            assert( false, "argument number mismatch" );
        }

        // Do the call.
        if( fn.host.fn !is null || fn.host.dg !is null )
        {
            assert( clValues.length == 0 );

            if( ! fn.host.evalCtxCompatible(evalCtx) )
            {
                /*
                    This means that we're trying to:
                    
                    - Call a runtime function at compile time.  This isn't an
                      error, but we do need to delay actual execution.  This
                      might be a problem if, for example, we're trying to
                      evaluate a macro.

                    - Call a compile-time function at runtime.  This is
                      *probably* an error since it's now too late to ever
                      execute.
                 */
                switch( evalCtx )
                {
                    case Sit.EvalContext.Compile:
                        throw new EarlyCallAbort;

                    case Sit.EvalContext.Runtime:
                        throw new LateCallAbort;

                    default:
                        assert(false);
                }
            }

            // Ok, call it.
            try
            {
                if( fn.host.fn !is null )
                    result = fn.host.fn(evalCtx, args);
                else
                    result = fn.host.dg(evalCtx, args);
            }
            catch( Sit.TailCall tc )
            {
                callable = tc.callable;
                args = tc.args;
            }
        }
        else if( fn.expr !is null )
        {
            scope eval = new Eval.EvalVisitor;
            try
            {
                result = eval.invokeExprFn(fn, args, clValues, evalCtx);
            }
            catch( Sit.TailCall tc )
            {
                callable = tc.callable;
                args = tc.args;
            }
        }
        else
        {
            assert( false, "function has no implementation" );
        }
    }
    while( result is null );

    return result;
}

