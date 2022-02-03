#ifndef SRC_LOCATION_H
#define SRC_LOCATION_H

#include<string>

// we don't have <source_location>.  hence this reinvention.

#define _CLUDGE_TO_STR(x) #x
#define _CLUDGE_STR(x) _CLUDGE_TO_STR(x)
#define THIS_LINE __FILE__ " line " _CLUDGE_STR(__LINE__)

#ifndef __has_builtin
  #define __has_builtin(x) 0
#endif

#if __has_builtin(__builtin_FILE) && __has_builtin(__builtin_LINE)
    #define CALLER() \
        (std::string(__builtin_FILE()) + " line " + std::to_string(__builtin_LINE()))
#else
    #define CALLER() "(caller unavailable)"
#endif


#endif // SRC_LOCATION_H