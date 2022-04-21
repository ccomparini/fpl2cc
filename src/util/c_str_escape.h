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
        // escape quotes and backslashes so that they make
        // it through the c compiler without prematurely
        // terminating the string:
        if(inch == '"' || inch == '\\') {
            escaped += '\\';
        } // else normal char

        escaped += inch;
    }
    return escaped;
}

#endif // C_STR_ESCAPE_H
