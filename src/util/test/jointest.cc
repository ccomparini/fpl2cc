#include "join.h"
#include "stringformat.h"

#include <iostream>
#include <list>
#include <map>
#include <string>
#include <vector>

struct has_to_str {
    std::string contents;

    has_to_str(const char *str): contents(str) {
    }

    std::string to_str() const {
        return contents;
    }
};

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

    std::cout << join(clist, "... ",
        [] (const std::string &) {
            return "etc";
        }
    ) << "\n";

    std::vector<has_to_str> vlist = { "vector", "works" };
    std::cout << join(vlist, " totally ") << "\n";
    std::cout << stringformat("{}", join(vlist, "\n    ")) << "\n";

    std::cout << join(kvs, "; ",
        [] (std::map<std::string, int>::iterator item, int index) {
            return stringformat("{}th key is {}", index, item->first);
        }
    ) << "\n";

    return 0;
}

