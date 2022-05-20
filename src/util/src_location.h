#ifndef SRC_LOCATION_H
#define SRC_LOCATION_H

#include<string>

// we don't have <source_location>.  hence this reinvention.
typedef const std::string &src_location;

#define _CLUDGE_TO_STR(x) #x
#define _CLUDGE_STR(x) _CLUDGE_TO_STR(x)
#define THIS_LINE __FILE__ " line " _CLUDGE_STR(__LINE__)

#ifndef __has_builtin
  #define __has_builtin(x) 0
#endif

#if __has_builtin(__builtin_FILE) && __has_builtin(__builtin_LINE)
    #define CALLER_FILE() (std::string(__builtin_FILE()))
    #define CALLER_LINE() (            __builtin_LINE() )

    // CALLER() returns a file/line string
    #define CALLER() \
        (std::string(__builtin_FILE()) + " line " + std::to_string(__builtin_LINE()))
#else
    #define CALLER_FILE() ("(file unavailable)")
    #define CALLER_LINE() (0)
    #define CALLER() "(caller unavailable)"
#endif


#endif // SRC_LOCATION_H
