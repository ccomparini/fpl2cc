/*
 */

#include <iostream>
#include <string>

#include "jerror.h"
#include "variants.h"

int main() {
    std::list<std::string> vars = { "a", "b", "c" };
    std::cout << variants(vars);

    jerror::handler erhand(jerror::error_channel,
        []
        (const std::string &msg, src_location where) {
            std::cerr << stringformat(
                "caught error: {::T} at {}\n", chop_nl(msg), where
            );
            return;
        }
    );

    // this one, we expect to throw an error (which should be caught
    // in the handler above)
    std::cout << variants({ "no-such-variant" });

    return 0;
}


