/**
    Code relating to formatting.

    Authors: Daniel Keep <daniel.keep@gmail.com>
    Copyright: See LICENSE.
 */
module ouro.util.Formatting;

import Integer = tango.text.convert.Integer;

void parseAlignment(char[] s, out size_t w, out char a, out char[] p)
{
    a = Alignment.Right;

    enum St { I, W }
    auto st = St.I;

    char[] ws;

    size_t mark = 0;

chLoop:
    foreach( i,dchar c ; s )
    {
        switch( st )
        {
            case St.I:
            {
                switch( c )
                {
                    case '0': case '1': case '2': case '3': case '4':
                    case '5': case '6': case '7': case '8': case '9':
                        st = St.W;
                        break;

                    default:
                        assert( false, "unexpected character" );
                }
            }
            break;

            case St.W:
            {
                switch( c )
                {
                    case '0': case '1': case '2': case '3': case '4':
                    case '5': case '6': case '7': case '8': case '9':
                        // All good.
                        break;

                    case '<':
                    case '|':
                    case '>':
                        ws = s[0..i];
                        mark = i+1;
                        a = cast(char) c;
                        break chLoop;

                    default:
                        assert( false, "unexpected character" );
                }
            }
            break;

            default:
                assert(false);
        }
    }

    if( ws is null )
    {
        ws = s[];
        mark = s.length;
    }

    w = Integer.toInt(ws);
    p = s[mark..$];
}

