/*
    a rough stringformat test.
 */

#include <iostream>
#include "stringformat.h"

struct i_has_to_str {
    std::string to_str() { return "called to_str()!"; }
};


int main() {
    std::cout << stringformat(
        "here's arg 1: {1} and now 0: {0} and last {}\n", "zero", 1, 2
    );

    i_has_to_str foo;
    std::cout << stringformat("to string function: {}\n", foo);

    std::cout << stringformat("converting newlines: {::n}\n", "\nyay\n");

    return 0;
}


