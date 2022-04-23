#ifndef C_STR_ESCAPE_H
#define C_STR_ESCAPE_H

#include<string>

/*
   Returns a version of the string passed which is suitable for
   embedding in a c (or c++) program
 */
inline std::string c_str_escape(const std::string src) {
    std::string escaped;
    for(const char &inch : src) {
        switch(inch) {
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
                if(inch < 0x20 || inch >= 0x7f) {
                    // other non-printable ascii char not handled above.
                    // note that this will (effectively) cover utf-8
                    // or whatever - it's a case of "bytes is bytes".
                    char buf[23]; // because 23 is a lucky number :P
                    snprintf(buf, 23, "0x%02x", inch);
                    escaped += buf;
                } else {
                    escaped += inch;
                }
                break;
        }
    }
    return escaped;
}

#endif // C_STR_ESCAPE_H
