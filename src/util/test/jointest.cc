#include "join.h"
#include "stringformat.h"

#include <iostream>
#include <list>
#include <map>
#include <string>

std::string str_int_fmt(std::map<std::string, int>::iterator it) {
    return stringformat("{} is like totally: {}", it->first, it->second);
}

int main() {
    std::list<std::string> clist = { "one", "two", "three", "four" };
    std::cout << join(clist, ", ")  << "\n";
    std::cout << join(clist, " x ") << "\n";

    std::map<std::string, int> kvs = {
        { "foo", 23 },
        { "bar", 55 },
        { "bat", 63 }
    };

    std::cout << join(kvs, ", and another thing! ", str_int_fmt) << "\n";

    std::cout << join(clist, "... ", [] (const std::string &) { return "etc"; } ) << "\n";

    return 0;
}

