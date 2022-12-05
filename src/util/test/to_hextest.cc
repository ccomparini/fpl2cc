
#include "to_hex.h"

#include <iostream>
#include <list>


int main() {
    std::cout << to_hex("#this is a c style string!\n") << "\n";

    std::list<char> clist = {
        '#', '1', '\n',
        '#', '2', '\n',
    };
    std::cout << bs_to_hex(clist) << "\n";

    unsigned short sh = 0x23; // mostly portable?
    std::cout << to_hex(sh) << "\n";

    // possibly mostly somewhat portable a little bit:
    unsigned long long sz = 0xf8070605040302f1;
    std::cout << to_hex(sz) << "\n";
}

