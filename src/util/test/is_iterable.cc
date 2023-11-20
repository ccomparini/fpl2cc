/*
  test for is_iterable<>
 */
#include "is_iterable.h"
#include "src_location.h"
#include <iostream>
#include <list>
#include <map>
#include <string>

template<typename T>
struct expect_iterable {
    expect_iterable(const std::string &ca = CALLER()) {
        if constexpr(is_iterable(T)) {
            std::cout << "OK " << ca << "\n";
        } else {
            std::cerr << "FAIL at " << ca << "\n";
        }
    }
};

template<typename T>
struct expect_not_iterable {
    expect_not_iterable(const std::string &ca = CALLER()) {
        if constexpr(is_iterable(T)) {
            std::cerr << "FAIL at " << ca << "\n";
        } else {
            std::cout << "OK " << ca << "\n";
        }
    }
};

template<typename T>
void expect_iterable_ref(T &item, const std::string &ca = CALLER()) {
    if constexpr(is_iterable(T)) {
        std::cout << "OK " << ca << "\n";
    } else {
        std::cout << "FAIL not iterable ref at " << ca << "\n";
    }
}

int main() {
    std::cout << "testing is_iterable template...\n";
    std::cerr << "\n";

    expect_not_iterable<int>();
    expect_not_iterable<const char *>();

    expect_iterable<std::list<int>>();
    expect_iterable<const std::list<int>>();

    expect_iterable<std::map<std::string, int>>();
    expect_iterable<const std::map<std::string, int>>();

    const std::map<std::string, int> foo;
    expect_iterable_ref(foo);

    std::map<std::string, int> ncfoo;
    expect_iterable_ref(ncfoo);

    std::cout << "\nis_iterable test is done\n";
}

