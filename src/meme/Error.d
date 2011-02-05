/**
    Compiler errors.
    
    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
*/
module meme.Error;

import tango.text.convert.Format;

import meme.Location : Location;

enum CompilerErrorCode : uint
{
    Unknown = 0,

    LUnusWs = 1000,
    LUntermBlockCmt,
    LBlockCmtOvfl,
    LExDecDigit,
    LExHexDigit,
    LInvStrEsc,
    LUntermString,
    LUnexpected,
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

// Set message for real this time
cast(CEC) uint.max: null
    ];
}

class CompilerException : Exception
{
    alias CompilerErrorCode Code;
    alias CompilerErrorMessages Messages;

    this(Code code, Location loc, char[] arg0)
    {
        this.arg0 = arg0;
        this(code, loc);
    }

    this(Code code, Location loc)
    {
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
                Format(msg, arg0));
    }

    Code code;
    Location loc;
    char[] arg0;
}

// vim:tw=0
