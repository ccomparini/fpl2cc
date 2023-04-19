#include "join.h"

#include <iostream>
#include <list>
#include <map>
#include <string>


int main() {
    std::list<std::string> clist = { "one", "two", "three", "four" };
    std::cout << join(clist, ", ")  << "\n";
    std::cout << join(clist, " x ") << "\n";
/*
    std::map<std::string, int> = {
        { "foo", 23 }
    };
 */

    return 0;
}

