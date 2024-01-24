/*
    jemp/jemplpl test bed - calls the jemptest function with
   some arbitrary test params.

    For the real test source, see jemptest.jemp.
 */

#include <iostream>
#include <string>

#include "jemptest.h"

int main() {
    std::cout << jemptest(23, "hello world");

    return 0;
}

