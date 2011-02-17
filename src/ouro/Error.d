/**
    Compiler errors.
    
    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module ouro.Error;

import tango.text.convert.Format;

import ouro.Location : Location;

enum CompilerErrorCode : uint
{
    Unknown = 0,

    Lexical = 1000,
    LUnexpected,
    LUnusWs,
    LUntermBlockCmt,
    LBlockCmtOvfl,
    LExDecDigit,
    LExHexDigit,
    LInvStrEsc,
    LUntermString,

    Parse = 2000,
    PUnexpected,
    PUnexEos,
    PExGot,
    PExManyGot,
    PExExprGotTx,
    PExFuncAftMacro,
    PAstQArgNum,
    PAstQQArgNum,
    PAstQQSArgNum,
    PLetArgNum,
    PImpArgNum,
    PMacroKeyword,
    PExInfFunc,
    PBiArgNum,
    PBiArgType,
    PExBraceForKw,
    PExParenForKw,

    Semantic = 3000,
    SUnexpected,
    SUnexExplode,
    SLetSelImp,
}

private alias CompilerErrorCode CEC;

char[][uint] CompilerErrorMessages;

static this()
{
    CompilerErrorMessages =
    [
// Set implicit type; we'll set the message itself at the end.
CEC.Unknown:        "unknown error"[],

// Error messages
CEC.LUnusWs:        "unusual whitespace detected; only regular spaces, tabs and newlines allowed",
CEC.LUntermBlockCmt:"unterminated block comment",
CEC.LBlockCmtOvfl:  "comment too deeply nested",
CEC.LExDecDigit:    "expected decimal digit, not '{}'",
CEC.LExHexDigit:    "expected hex digit, not '{}'",
CEC.LInvStrEsc:     "invalid string escape sequence: '\\{}'",
CEC.LUntermString:  "unterminated string literal",
CEC.LUnexpected:    "unexpected character '{}'",

CEC.PUnexpected:    "unexpected '{}'",
CEC.PUnexEos:       "unexpected end-of-source",
CEC.PExGot:         "expected {}, got {}",
CEC.PExManyGot:     "expected one of {}; got {}",
CEC.PExExprGotTx:   "expected expression, got '{}'",
CEC.PExFuncAftMacro:"expected function call after 'macro'",
CEC.PAstQArgNum:    "ast quote requires one argument",
CEC.PAstQQArgNum:   "ast quasi-quote requires one argument",
CEC.PAstQQSArgNum:  "ast quasi-quote substitution requires one argument",
CEC.PLetArgNum:     "let requires at least one argument",
CEC.PImpArgNum:     "import requires three arguments",
CEC.PMacroKeyword:  "cannot macro call a keyword-like function",
CEC.PExInfFunc:     "expected function name or sub-expression",
CEC.PBiArgNum:      "__builtin__ requires exactly one argument",
CEC.PBiArgType:     "__builtin__ expected a string literal argument",
CEC.PExBraceForKw:  "expected macro call for {}",
CEC.PExParenForKw:  "expected regular call for {}",

CEC.SUnexpected:    "unexpected semantic error",
CEC.SUnexExplode:   "misplaced explode expression",
CEC.SLetSelImp:     "cannot have a selective binding import",

// Set message for real this time
cast(CEC) uint.max: null
    ];
}

class CompilerException : Exception
{
    alias CompilerErrorCode Code;
    alias CompilerErrorMessages Messages;

    this(Code code, Location loc, char[] arg0 = null, char[] arg1 = null)
    {
        this.arg0 = arg0;
        this.arg1 = arg1;
        this.code = code;
        this.loc = loc;

        super(buildMessage);
    }

    private char[] buildMessage()
    {
        char[] msg;
        if( code in Messages )
            msg = Messages[code];
        else
            msg = "undefined error";

        return Format("{} #{,04}: {}", loc.toString, code,
                Format(msg, arg0, arg1));
    }

    Code code;
    Location loc;
    char[] arg0;
    char[] arg1;
}

// vim:tw=0
