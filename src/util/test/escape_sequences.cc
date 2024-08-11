
#include"escape_sequences.h"
#include"stringformat.h"
#include"to_hex.h"

#include<iostream>

bool jest_tests() {

    // all the old c-style things (some of these are dumb - \a?)
    // also \b might better used as binary input.
    // \n and \t (and maybe \r for systems with ms-dos ancestry)
    // might be useful to keep but the rest are perhasp dubious.
    std::string old_c_style = "\\a\\b\\e\\f\\n\\r\\t\\v";
    auto converted = convert_jest_escapes(old_c_style);
    if(converted != "\a\b\e\f\n\r\t\v") {
        std::cerr  << stringformat(
            "mismatch in old c-style conversion:\n\t{}\n\t\tvs\n\t{}\n",
            bs_to_hex(old_c_style), bs_to_hex(converted)
        );
    }

    // hex tests:
    std::string out;
    if((out = convert_jest_escapes("\\x0"))[0]) {
        std::cerr << stringformat("expected 0 length string");
    }
    if((out = convert_jest_escapes("\\x00"))[0]) {
        std::cerr << stringformat("expected 0 length string on double 0");
    }
    if((out = convert_jest_escapes("\\x2345")) != "#45") {
        std::cerr << stringformat("expected '#45' but got '{}'\n", out);
    }

    // utf8 encoded unicode
    if((out = convert_jest_escapes("\\U1F600")) != "😀") {
        // utf-8 f0 9f 98 80
        std::cerr << stringformat("expected '😀' but got '{}'\n", out);
    }
    if((out = convert_jest_escapes("\\u1F511")) != "🔑") {
        // utf-8 f0 9f 94 91
        std::cerr << stringformat("expected '🔑' but got '{}'\n", out);
    }
    if((out = convert_jest_escapes("\\u20ac")) != "€") {
        // utf-8  e2 82 ac
        std::cerr << stringformat("expected '€' but got '{}'\n", out);
    }
    if((out = convert_jest_escapes("\\uA1")) != "¡") {
        // utf-8 c2 a1
        std::cerr << stringformat("expected '¡' but got '{}'\n", out);
    }
    if((out = convert_jest_escapes("\\u23")) != "#") {
        std::cerr << stringformat("expected '#' but got '{}'\n", out);
    }

    // not actually unicode:
    if((out = convert_jest_escapes("\\uhi")) != "uhi") {
        std::cerr << stringformat("expected 'uhi' but got '{}'\n", out);
    }

    // 0x5c = ascii backslash
    out = convert_jest_escapes("n\x5cothing \x5c\" \x5c\x5c \x5c' \\3");
    auto expect =              "nothing \" \\ \' 3";
    if(out != expect) {
        std::cerr << stringformat(
            "expected '{}' but got '{}'\n", expect, out
        );
    }

    std::cout << "jest tests ok\n";
    return true;
}

int main() {
    std::cout << "\nescape sequences...\n";
    jest_tests();
    std::cout << "YAY\n";
    return 0;
}

