#ifndef JOIN_H
#define JOIN_H

#include "is_iterable.h"
#include "stringformat.h"

#include <string>

template<typename T>
std::string join(T elements, const std::string &jv) {
    if constexpr(!is_iterable(T)) {
        // it's not iterable?  try formatting it anyway:
        return stringformat("{}", elements);
    } else { 
        std::string out;
        for(auto el = elements.begin(); el != elements.end(); el++) {
            if(out.length())
                out += stringformat("{}{}", jv, *el);
            else
                out += stringformat("{}", *el);
        }
        return out;
    }
}

#endif // JOIN_H
