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

    LUnexpected = 1000,
    LUnusWs,
    LUntermBlockCmt,
    LBlockCmtOvfl,
    LExDecDigit,
    LExHexDigit,
    LInvStrEsc,
    LUntermString,

    PUnexpected = 2000,
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
