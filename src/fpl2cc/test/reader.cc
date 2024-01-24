
#include "fpl2cc/fpl_reader.h"
#include "util/jerror.h"
#include "util/stringformat.h"
#include "util/to_hex.h"

#include <iostream>
#include <list>
#include <set>
#include <vector>

/*
    fpl_reader test bed
 */

static fpl_reader_p test_reader() {
    const char *arbitrary = 
        "£23 is the cost. ¡that's 892ƒ for you!\n"
        "达科他盗龙成为了当时仅存的驰龙科之一。"
        "化石发现地位于一个含多个物种的骨层上，"
        "尽管后来发现正模标本和参考标本中部分骨骼属于甲鱼，"
        "但还没有对其潜"
        "ᚻᛖ ᚳᚹᚫᚦ ᚦᚫᛏ ᚻᛖ ᛒᚢᛞᛖ ᚩᚾ ᚦᚫᛗ ᛚᚪᚾᛞᛖ ᚾᚩᚱᚦᚹᛖᚪᚱᛞᚢᛗ ᚹᛁᚦ ᚦᚪ ᚹᛖᛥᚫ"
        "\r\n👍";
    utf8_buffer input((utf8_byte *)arbitrary, strlen(arbitrary));

    return std::make_shared<fpl_reader>(
        input, "reader.cc-arbitrary_utf8_text"
    );
}

int num_errors = 0;
void error_or_warning(const std::string &msg, src_location caller) {
    std::cerr << stringformat("{}: {}\n", caller, msg);
    num_errors++;
}

bool test_read_to_eof() {
    bool ok = true;
    const char *str = "edit";
    utf8_buffer buf((utf8_byte *)str, strlen(str));
    auto reader = fpl_reader(buf, "reader.short_text");

    auto read = reader.read_to_exact_match("item");
    if(read != "edit") {
        jerror::error(stringformat(
            "attempt to read to 'item' matched '{}' at '{}'\n",
            read, reader.debug_peek(0, 10)
        ));
        ok = false;
    }

    reader.go_to(0);
    read = reader.read_to_exact_match("it");
    if(read != "ed") {
        jerror::error(stringformat(
            "attempt to read to 'it' matched '{}' at '{}'\n",
            read, reader.debug_peek(0, 10)
        ));
        ok = false;
    }

    return ok;
}

int main(int argc, const char **argv) {

    jerror::handler _eh(jerror::error_channel, error_or_warning);
    jerror::handler _wh(jerror::warning_channel, error_or_warning);

    // testing char_length:
    fpl_reader_p reader = test_reader();
    std::vector<size_t> char_pos;
    std::set<int> sizes;
    do {
        size_t cur_pos = reader->current_position();
        char_pos.push_back(cur_pos);
        sizes.insert(reader->char_length());
        reader->eat_char();
    } while(!reader->eof());

    for(auto pos : char_pos) {
        std::cout << stringformat("there's a char at {}\n", pos);
    }

    for(auto size : sizes) {
        std::cout << stringformat("there's at least one char of size {}\n", size);
    }

    for(int pind = 1; pind < char_pos.size(); pind++) {
        reader->go_to(char_pos[pind - 1]);
        if(reader->char_length() > 1) {
            // (this checks to make sure we can't be derailed
            // by not starting at an actual character)
            reader->read_byte();
            // now we're in the middle of a single char...
        }
        reader->eat_char();
        if(reader->current_position() != char_pos[pind]) {
             jerror::error(stringformat(
                 "char {}: expected to be at {} but am at {}",
                 pind, char_pos[pind], reader->current_position()
             ));
        }
    }

    reader = test_reader(); // reset..
    // fist character in utf-8 is 2 bytes, 0xc2 0xa3 (GBP sign '£')
    utf8_byte start = reader->peek();
    if(start != 0xc2) {
        jerror::error(
            stringformat("expected 0xc2 but got 0x{}\n", to_hex(start))
        );
    }
    if(reader->read_byte_not_equalling(start)) {
        jerror::error("... read-not-equalling something impossible\n");
    }
    if(!reader->read_byte_equalling(0xc2)) {
        jerror::error("failed to read 0xc2 in GBP sign '£'\n");
    }
    if(!reader->read_byte_equalling(0xa3)) {
        jerror::error("failed to read second byte of GBP sign '£'\n");
    }

    test_read_to_eof();

    std::cout << stringformat("{} errors/warnings\n", num_errors);

    return 0;
}

