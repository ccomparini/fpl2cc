#ifndef C_STR_ESCAPE_H
#define C_STR_ESCAPE_H

#include<string>

/*
   Returns a version of the string passed which is suitable for
   embedding in a c (or c++) program
 */
inline std::string c_str_escape(const std::string src) {
    std::string escaped;
    for(const uint8_t &inch : src) {
        switch(inch) {
            case 0x00: escaped += "\\0";  break;
            case 0x07: escaped += "\\a";  break; // Alert (Beep, Bell)
            case 0x08: escaped += "\\b";  break; // Backspace
            case 0x0C: escaped += "\\f";  break; // Formfeed Page Break
            case 0x0A: escaped += "\\n";  break; // Newline (Line Feed)
            case 0x0D: escaped += "\\r";  break; // Carriage Return
            case 0x09: escaped += "\\t";  break; // Horizontal Tab
            case 0x0B: escaped += "\\v";  break; // Vertical Tab
            case 0x5C: escaped += "\\\\"; break;
            case 0x22: escaped += "\\\""; break;
            case 0x3F: escaped += "\\?";  break; // (to avoid trigraphs)
            default:
                if(inch < 0x20 || inch == 0x7f) {
                    // other non-printable ascii char not handled above.
                    // note that this will (effectively) cover utf-8
                    // or whatever - it's a case of "bytes is bytes".
                    char buf[23]; // 23 is a lucky number larger than 5 :P

                    // we use octal instrad of hex because hex escape
                    // sequences are arbitrarily long, which means that
                    // (eg) "foo\x23bar" would actully be interpreted
                    // as "foo" "\x23ba" "r" (since b and a are both
                    // hex digits).  I find this counter intuitive but
                    // that's how it works.  I believe it's because char
                    // size is not necessarily 8 bits, as far as c is
                    // concerned.  However, for whatever reason, octal
                    // escapes are always 3 octal digits, so octal
                    // doesn't have that problem.
                    snprintf(buf, sizeof(buf), "\\%03o", unsigned(inch) & 0xff);
                    escaped += buf;
                } else {
                    // either normal, or utf-8.  just copy it verbatim! yay
                    escaped += inch;
                }
                break;
        }
    }
    return escaped;
}

#endif // C_STR_ESCAPE_H
