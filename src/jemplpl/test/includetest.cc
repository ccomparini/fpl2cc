#include <cstdlib>
#include <iostream>
#include <string>

#include "includetest.h"
#include "util/searchpath.h"
#include "util/stringformat.h"

//static const auto data_dir_var = "DATA_DIR";
static const auto data_dir_var = "SRC_DIR";

int main() {
    const char *data_dir = std::getenv(data_dir_var);
    Searchpath src_path;
    if(!src_path.append_from_env(data_dir_var)) {
        jerror::error(stringformat(
            "{} not set in env - test may not work\n", data_dir_var
        ));
    }
    //std::cerr << stringformat("searching paths: {}\n", src_path);
    std::cout << includetest(src_path.find("includetest.cc"));

    return 0;
}

