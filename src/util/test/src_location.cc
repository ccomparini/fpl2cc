
#include "util/src_location.h"
#include <iostream>
#include <string>


void print_caller(const std::string &msg, src_location caller = CALLER()) {
    std::cout << msg << " from " << caller << "\n";
}

int main(int argc, char **argv) {

    print_caller("\none");
    print_caller("next line");


    print_caller("skipped a couple");

    print_caller("woot");

    std::cout << THIS_LINE << " should be line 21\n";
}

