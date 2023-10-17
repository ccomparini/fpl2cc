/*
    a rough stringformat test.
 */

#include "stringformat.h"

#include <iostream>
#include <list>
#include <map>
#include <tuple>
#include <variant>

struct i_has_to_str {
    std::string to_str() { return "called to_str()!"; }
};

struct just_data {
    static const char max_size = 23;
    char stuff[max_size];

    just_data() {
        for(char ind = 0; ind < max_size; ind++) {
            stuff[ind] = ind;
        }
    }
};

int main() {
    std::cout << stringformat(
        "here's arg 1: {1} and now 0: {0} and last is {}\n", "zero", "oneth", 2
    );

    i_has_to_str foo;
    std::cout << stringformat("to string function: {}\n", foo);

    just_data bar;
    std::cout << stringformat("just data (to hex): {}\n", bar);

    std::variant<
        i_has_to_str,
        just_data
    > bat;
    bat = bar;

    // convert but don't write the variant version because the
    // formatted string  will depend on the implementation of
    // variant.  as long as the stringformat runs we'll consider
    // it ok.
    std::string converted_variant = stringformat("{}", bat);

    std::cout << stringformat("converting newlines: {::n}\n", "\nyay\n");

    std::list<std::string> buncha_strings = {
        "string 1", "things", "and stuff"
    };
    std::cout << stringformat("list: {}\n", buncha_strings);

    std::list<bool> bools = { true, true, false };

    std::tuple<bool, std::string> tuppy = { true, "foo" };
    std::cout << stringformat("tuple: {}\n", tuppy);

    std::map<std::string, int> some_map = {
        { "foo", 1 },
        { "bar", 2 },
        { "bat", 3 },
    };
    std::cout << stringformat("map: {}\n", some_map);

    // c string escape.  this might or might not be useful.
    // I did it because I thought I needed it but turns out
    // there was a better plan.  nonetheless:
    std::cout << stringformat("escaped: {::e}\n", "\n\t\"woot\"\n");

    // tab columnation:  this might not be the awesomest
    // interface, actually, because you probably want to
    // columnate the arguments or .. something.  anyway
    // it'll work for now:
    std::cout << stringformat("{::c}\n",
        "a\tb\tc\n"
        "deeeee\n"
        "\te\teff\tgee\thaitch\n"
        "\t\teye"
    );

    // uppercasing, and converting newlines
    std::cout << stringformat("louder: {::Un}\n", "I said\nhello");

    const char *maybenull = nullptr;
    std::cout << stringformat("{} null pointer doesn't crash\n", maybenull);
    maybenull = "for sure a const char *";
    std::cout << stringformat("and it's {}\n", maybenull);

    std::cout << stringformat("and I want to embed a {{ with stuff after it\n");
    return 0;
}


