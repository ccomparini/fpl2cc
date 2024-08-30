/*
   Check the jemp @error: ..@  directive
 */

#include <iostream>
#include <string>

#include "errortest.h"
#include "stringformat.h"


void print_error(const std::string &msg, src_location caller) {
     std::cerr << stringformat("Received message: {} at {}\n", msg, caller);
}

int main() {
    //std::cerr << "\n";   // (clarifies output)
    jerror::handler x(jerror::error_channel, print_error);
    std::cout << errortest(0, nullptr);       // no error; fill template
    std::cout << errortest(1, "as expected"); // throw an error
    std::cout << errortest(2, "");            // error message missing
    return 0;
}

