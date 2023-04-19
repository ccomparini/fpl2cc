#ifndef JOIN_H
#define JOIN_H

#include "is_iterable.h"
#include "jerror.h"
#include "src_location.h"
#include "stringformat.h"

#include <functional>
#include <string>

/*
   join(iterable, join_value, formatter) -
      returns a string containing each element formatted according to
      the formatter passed, join using the join value string passed.
 */
template<typename T>
std::string join(
    T elements, const std::string &jv,
    std::function<std::string(typename T::iterator)> fmtr,
    src_location caller = CALLER()
) {
    if constexpr(!is_iterable(T)) {
        jerror::error("type for first argument is not iterable", caller);
    } else { 
        std::string out;
        for(auto el = elements.begin(); el != elements.end(); el++) {
            if(out.length())
                out += jv;
            out += fmtr(el);
        }
        return out;
    }
    return "";
}

/*
   join(iterable, join_value) - returns a string containing each element
   of the iterable passed (formatted per stringformat()), with copies
   of the join_value string between them.
 */
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
