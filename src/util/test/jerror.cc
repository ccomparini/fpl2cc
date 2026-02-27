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

void deepest_handler(jerror::channel chan, const std::string &msg, src_location where) {
    std::cerr << "deepest handler will now defer to each outer handler...\n";
    // check each "outer" handler, starting with this one, and if
    // it's not this one, call it:
    for(int scope = 0; auto hdlr = jerror::outer_handler(chan, scope); scope++) {
        std::cerr << stringformat("Outerness level: {}   ", scope);
        if(hdlr == deepest_handler) {
            std::cerr << stringformat("It's us with {} on {} at {}!\n", msg, chan, where);
        } else {
            hdlr(chan, msg, where);
        }
    }
    std::cerr << stringformat("done with deepest handler of '{}'\n", msg);
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
        {
            jerror::handler deepest(jerror::warning_channel, deepest_handler);
            jerror::warning("🦑");
        }

        // there should be no 23rd outer scope, so the handler will
        // be missing, and missing handlers should default:
        std::cerr << "seeking missing handler....\n";
        auto missing = jerror::outer_handler(jerror::warning_channel, 23);
        if(missing) {
            std::cerr << "Missing handler turns out to not be missing??";
        } else {
            missing(jerror::warning_channel, "calling missing handler!", THIS_LINE);
        }
    }

    jerror::error("outer peace");
}

int main(int argc, const char **argv) {
    test_handlers();

    jerror::warning("I'm going to say goodbye now");
    jerror::error("goodbye!");

    std::cout << "can't get here\n";
}


