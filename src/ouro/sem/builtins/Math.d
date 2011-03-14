/**
    Math builtins.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.sem.builtins.Math;

private:

import Erf = tango.math.ErrorFunction;
import Gamma = tango.math.GammaFunction;
import IEEE = tango.math.IEEE;
import Math = tango.math.Math;
import Prob = tango.math.Probability;

import ouro.sem.builtins.Util; // many things

template Tuple(T...)
{
    alias T Tuple;
}

template TupleLen(T...)
{
    const TupleLen = T.length;
}

template Repeat(T, size_t n)
{
    static if( n == 0 )
        alias Tuple!() Repeat;
    else
        alias Tuple!(T, Repeat!(T, n-1)) Repeat;
}

template RealValue(char[] name, char[] expr)
{
    static this()
    {
        Builtins.register("ouro.math."~name,
                new Sit.NumberValue(null, mixin(expr)));
    }
}

template RealFunc(char[] name, char[] fn, ArgNames...)
{
    const ArgNum = TupleLen!(ArgNames);

    Sit.Argument[ArgNum] args;

    static this()
    {
        foreach( i,ArgName ; ArgNames )
            args[i] = Sit.Argument(ArgName);

        Builtins.register("ouro.math."~name,
                new Sit.FunctionValue("ouro.math."~name, args, &func));
    }

    alias Repeat!(real, ArgNum) RealArgs;

    Value func(EC ec, Value[] vs)
    {
        chkArgNum(vs, ArgNum);
        RealArgs rs;
        foreach( i,_ ; ArgNames )
            rs[i] = chkArgNumber(vs, i);
        return new Sit.NumberValue(null, mixin(fn~"(rs)"));
    }
}

mixin RealValue!("e",               "Math.E");
mixin RealValue!("π",               "Math.PI");
mixin RealValue!("eulerΓ",          "Math.EULERGAMMA");

mixin RealFunc!("abs",      "Math.abs",     "x");
mixin RealFunc!("minNum",   "Math.minNum",  "x", "y");
mixin RealFunc!("maxNum",   "Math.maxNum",  "x", "y");
mixin RealFunc!("minNan",   "Math.minNaN",  "x", "y");
mixin RealFunc!("maxNan",   "Math.maxNaN",  "x", "y");

mixin RealFunc!("cos",      "Math.cos",     "x");
mixin RealFunc!("sin",      "Math.sin",     "x");
mixin RealFunc!("tan",      "Math.tan",     "x");
mixin RealFunc!("cosπ",     "Math.cosPi",   "xπ¯¹");
mixin RealFunc!("sinπ",     "Math.sinPi",   "xπ¯¹");
mixin RealFunc!("atanπ",    "Math.atanPi",  "xπ¯¹");
mixin RealFunc!("acos",     "Math.acos",    "x");
mixin RealFunc!("asin",     "Math.asin",    "x");
mixin RealFunc!("atan",     "Math.atan",    "x");
mixin RealFunc!("atan2",    "Math.atan2",   "y", "x");
mixin RealFunc!("cosh",     "Math.cosh",    "x");
mixin RealFunc!("sinh",     "Math.sinh",    "x");
mixin RealFunc!("tanh",     "Math.tanh",    "x");
mixin RealFunc!("acosh",    "Math.acosh",   "x");
mixin RealFunc!("asinh",    "Math.asinh",   "x");
mixin RealFunc!("atanh",    "Math.atanh",   "x");

mixin RealFunc!("sqrt",     "Math.sqrt",    "x");
mixin RealFunc!("cbrt",     "Math.cbrt",    "x");
mixin RealFunc!("exp",      "Math.exp",     "x");
mixin RealFunc!("exp-1",    "Math.expm1",   "x");
mixin RealFunc!("exp2",     "Math.exp2",    "x");
mixin RealFunc!("log",      "Math.log",     "x");
mixin RealFunc!("log1p",    "Math.log1p",   "x-1");
mixin RealFunc!("log₂",     "Math.log2",    "x");
mixin RealFunc!("log₁₀",    "Math.log10",   "x");
mixin RealFunc!("pow",      "Math.pow",     "x", "y");

mixin RealFunc!("hypot",   "Math.hypot", "x", "y");
// poly
// feq -- deprecated?
mixin RealFunc!("floor",    "Math.floor",   "x");
mixin RealFunc!("ceil",     "Math.ceil",    "x");
mixin RealFunc!("round",    "Math.round",   "x");
mixin RealFunc!("trunc",    "Math.trunc",   "x");

mixin RealFunc!("erfc",     "Erf.erfc", "a");
mixin RealFunc!("erf",      "Erf.erf",  "x");

mixin RealFunc!("Γsign",    "Gamma.sgnGamma",   "x");
mixin RealFunc!("Γ",        "Gamma.gamma",      "x");
mixin RealFunc!("logΓ",     "Gamma.logGamma",   "x");
mixin RealFunc!("β",        "Gamma.beta",       "x", "y");
mixin RealFunc!("βp",       "Gamma.betaIncomplete",     "a", "b", "x");
mixin RealFunc!("βp¯¹",     "Gamma.betaIncompleteInv",  "a", "b", "y");
mixin RealFunc!("Γp",       "Gamma.gammaIncomplete",    "a", "x");
mixin RealFunc!("Γp'",      "Gamma.gammaIncompleteCompl", "a", "x");
mixin RealFunc!("Γp'¯¹",    "Gamma.gammaIncompleteComplInv", "a", "p");
mixin RealFunc!("diΓ",      "Gamma.digamma",    "x");

mixin RealFunc!("normalD",      "Prob.normalDistribution",      "a");
mixin RealFunc!("normalD'",     "Prob.normalDistributionCompl", "a");
mixin RealFunc!("normalD¯¹",    "Prob.normalDistributionInv",   "p");
mixin RealFunc!("normalD'¯¹",   "Prob.normalDistributionComplInv", "p");
// studentsTDistribution
// studentsTDistributionInv
// fDistribution
// fDistributionCompl
// fDistributionComplInv
mixin RealFunc!("χ²D",      "Prob.chiSqrDistribution",      "v", "x");
mixin RealFunc!("χ²D'",     "Prob.chiSqrDistributionCompl", "v", "x");
mixin RealFunc!("χ²D'¯¹",   "Prob.chiSqrDistributionComplInv", "v", "p");
mixin RealFunc!("ΓD",       "Prob.gammaDistribution",       "a", "b", "x");
mixin RealFunc!("ΓD'",      "Prob.gammaDistributionCompl",  "a", "b", "x");
mixin RealFunc!("βD",       "Prob.betaDistribution",        "a", "b", "x");
mixin RealFunc!("βD'",      "Prob.betaDistributionCompl",   "a", "b", "x");
mixin RealFunc!("βD¯¹",     "Prob.betaDistributionInv",     "a", "b", "x");
mixin RealFunc!("βD'¯¹",    "Prob.betaDistributionComplInv","a", "b", "x");
// poissonDistribution
// poissonDistributionCompl
// poissonDistributionInv
// binomialDistribution
// binomialDistributionCompl
// binomialDistributionInv
// negativeBinomialDistribution
// negativeBinomialDistributionInv

mixin RealFunc!("sign",     "IEEE.signbit",     "x");

