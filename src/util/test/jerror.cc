/*
    jerror unit tests
 */

#include <iostream>
#include "util/jerror.h"
#include "util/stringformat.h"

void generic_handler(jerror::channel chan, const std::string &msg, src_location where) {
    std::cerr << stringformat(
        "{} sez '{}' on channel {}\n",
        where, msg, chan
    );
}

void deeper_handler(const std::string &msg, src_location where) {
    std::cerr << stringformat( "{} is deeper! {}\n", where, msg);
}

void test_handlers() {
    jerror::handler a(jerror::error_channel, generic_handler);
    jerror::handler b(jerror::warning_channel, generic_handler);
    jerror::warning("gonna shout in a bucket");
    jerror::error("shouting in a bucket");

    {
        jerror::handler b(jerror::error_channel, deeper_handler);
        jerror::error("inner pease");
        jerror::warning("inner peace");
    }

    jerror::error("outer peace");

}

int main(int argc, const char **argv) {
    test_handlers();

    jerror::warning("I'm going to say goodbye now");
    jerror::error("goodbye!");

    std::cout << "can't get here\n";
}


