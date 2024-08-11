#ifndef ESCAPE_SEQUENCES_H
#define ESCAPE_SEQUENCES_H

#include "util/from_hex.h"
#include "util/to_utf8ish.h"

#include<string>

/**
 converts jest escape sequences:
   \a          0x07 ascii BEL ("alert")
   \b          0x08 ascii BS (backspace/ctrl-h)
   \t          0x09 ascii HT (horizontal tab)
   \n          0x0A ascii NL (newline)
   \v          0x0B ascii VT (vertical tab)
   \f          0x0C ascii FF (formfeed/page break)
   \r          0x0D ascii CR (carriage return)
   \e          0x1B ascii ESC (escape) (differs from c)
   \xhh?       One or 2 hex digits -> 1 byte (differs from c)
   \[uU]h{1,8} "Unicode" -> utf8-ish (differs from c,
               and not strictly unicode - see below)
   \.          whatever byte follows the backslash
               (differs from c - see below)

 Differences from c:
  - no octal support
  - \e is apparently nonstandard in c, but we have it
  - \[anything else] silently resolves to [anything else],
    which implicitly supports '\\', '\'', '"' and '\?'
    NOTE that this applies to things like invalid hex or
    \[uU] sequences: if the input is "\xyubba", the 
    output is "xyubba", and this is not treated as an error.
  - \xh is a valid 1 digit hex sequence. As with most
    numerals, leading 0 is implied, so \xf = 0x0f etc.
  - \x takes a maximum of 2 hex digits.  "\x234" is the
    same as "#4".
  - For "Unicode", U and u are interchangable, and you can
    specify 1 to 8 hex digits.  Whatever you give it will
    be 0-padded on the left and encoded into a utf-8 style
    series of bytes (i.e. high order bits signifying how
    many additional bytes follow in the current code point).
    No checks for valid unicode are performed.
    As such, anything <= 0x7f is the same in \Uhh or \xhh
    notation, and anything > 0x10ffff is guaranteed not to
    be actual unicode (though it may still be utf-8 encodable).
    Also (because the encoding "runs out" of bits in the
    first byte), the maximum encodable value is \u7fffffff.
    If the sequence specified can't be utf-8-style encoded,
    it will be treated as an escaped U or u followed by
    arbitrary text.
 */
std::string convert_jest_escapes(const std::string &src) {
    std::string out; out.reserve(src.length());
    int ind = 0;
    while(ind < src.length()) {
        // utf-8 note:  backslash is 0x5f = 0b01011100, and
        // multi-byte utf-8 chars have the high bits set on
        // each byte, so (on valid utf-8) the backslash will
        // never abridge a utf-8 character, so this does dtrt.
        if(src[ind] == '\\') {
            ++ind; // skip the backslash
            switch(src[ind]) {
                // (ordered numerically by output, not by escape sequence)
                case 'a': out += '\x07'; ind++; break;
                case 'b': out += '\x08'; ind++; break;
                case 't': out += '\x09'; ind++; break;
                case 'n': out += '\x0A'; ind++; break;
                case 'v': out += '\x0B'; ind++; break;
                case 'f': out += '\x0C'; ind++; break;
                case 'r': out += '\x0D'; ind++; break;
                case 'e': out += '\x1B'; ind++; break;
                case 'x': { // 2 hex chars max
                    ind++; // skip 'x'
                    // read hex digits (modifies ind):
                    auto tpos = ind;
                    char tmp = from_hex<char>(src, ind, 2);
                    if(ind > tpos) {
                        out += tmp;
                    } else {
                        // no/invalid hex digits, so it's not a proper
                        // hex escape sequence: copy the x "normally":
                        out += "x";
                    }
                    break;
                }
                case 'u':
                case 'U': {
                    char u_or_U = src[ind++]; // read the 'u'/'U'
                    auto tpos = ind;
                    uint32_t codepoint = from_hex<uint32_t>(src, ind);
                    if(ind > tpos) {
                        out += to_utf8ish<std::string>(codepoint);
                    } else {
                        // unencodable pseudo-codepoint, so again
                        // we're just copying the 'u' as if it had
                        // been any other escaped character
                        out += u_or_U;
                    }
                    break;
                }
                default:
                    out += src[ind++]; // just copy the char
                    break;
            }
        } else {
            out += src[ind++];
        }
    }
    return out;
}


#endif // ESCAPE_SEQUENCES_H
