
#include "fpl2cc/code_block.h"

int main(int argc, const char **argv) {

    fpl::code_block a1("code a");
    fpl::code_block a2("code a");
    fpl::code_block  b("code b");

    std::cerr << stringformat("a1 == a1: {}\n", a1 == a1);
    std::cerr << stringformat("a1 != a1: {}\n", a1 != a1);
    std::cerr << stringformat("a1 == a2: {}\n", a1 == a2);
    std::cerr << stringformat(" b == a1: {}\n", b == a1);
}
