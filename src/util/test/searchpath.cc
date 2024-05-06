#include <iostream>
#include "searchpath.h"
#include "stringformat.h"

bool change_dir(const std::string &dir) {
    std::error_code err;
    fs::current_path(dir, err);
    if(err) {
        std::cerr << stringformat(
            "can't change dir to {}: {}\n", dir, err.message()
        );
    }

    return !err;
}

int main(int argc, const char **argv) {
/*
    std::cerr << "\n";
    std::cerr << stringformat("invoked as {}\n", argv[0]);
    std::cerr << stringformat("  from dir {}\n", fs::current_path());
    std::cerr << stringformat("   SRC_DIR {}\n", getenv("SRC_DIR"));
    std::cerr << stringformat("  DATA_DIR {}\n", getenv("DATA_DIR"));
 */
    if(!getenv("SRC_DIR")) {
        std::cerr << "please set SRC_DIR to run this test\n";
        return 1;
    }

    if(!getenv("DATA_DIR")) {
        std::cerr << "please set DATA_DIR to run this test\n";
        return 1;
    } else if(!change_dir(getenv("DATA_DIR"))) {
        std::cerr << "can't continue.\n";
        return 1;
    }

    // the current working directory is now DATA_DIR so '.' should
    // match that. SRC_DIR is assumed to be above the data dir.
    Searchpath path(stringformat(
        "..:./b:./a:.", getenv("SRC_DIR")
    ));
    path.append("./nonexistent_dir");

    for(auto dir : path) {
        std::cout << stringformat("path contains: {}\n", dir);
    }

    std::cout << stringformat("found: {}\n", path.find("searchpath.cc"));
    std::cout << stringformat("found: {}\n", path.find("foobar.a.b"));

    std::string nsf = "no_such_file";
    std::cout << stringformat("find  ({}): '{}'\n", nsf, path.find  (nsf));
    std::cout << stringformat("search({}): '{}'\n", nsf, path.search(nsf));

    for(auto file : path.find_re("some.*")) {
        std::cout << stringformat("begins with 'some': {}\n", file);
    }
    
    for(auto file : path.find_re("\\.foo$")) {
        std::cout << stringformat("ends with 'foo': {}\n", file);
    }
    
    return 0;
}
