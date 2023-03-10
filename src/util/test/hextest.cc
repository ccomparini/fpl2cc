#include"from_hex.h"
#include"to_hex.h"

#include"stringformat.h"

#include<iostream>
#include<string>

int main() {
    int errors = 0;
    size_t pos = 0;

    const char *multi = "ff2344bb01020304";

    // since the type is 32 bits, this should read the first 8 chars:
    int32_t got = from_hex<int32_t>(multi, pos);
    std::cout << stringformat("{}\n", to_hex(got));

    // now read the rest 2 chars at a time:
    while(multi[pos]) {
        got = from_hex<int8_t>(multi, pos);
        std::cout << stringformat("{}\n", to_hex(got));
    }

    // now reread the whole thing, one input char at a time:
    pos = 0;
    while(multi[pos]) {
        uint8_t got = from_hex<uint8_t>(multi, pos, 1);
        std::cout << stringformat("{}\n", to_hex(got));
    }

    int pos2 = 0;
    uint64_t all = from_hex<uint64_t>(multi, pos2);
    std::cout << stringformat("{}\n", to_hex(all));

    std::cerr << stringformat("{} errors\n", errors);
    return errors;
}

