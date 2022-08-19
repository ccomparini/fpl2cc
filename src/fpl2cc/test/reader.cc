
#include "fpl2cc/fpl_reader.h"
#include "util/jerror.h"
#include "util/stringformat.h"
#include "util/to_hex.h"

#include <iostream>
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

int main(int argc, const char **argv) {

    // testing char_length:
    fpl_reader_p reader = test_reader();
    std::vector<size_t> char_pos;
    std::set<int> sizes;
    do {
        size_t cur_pos = reader->current_position();
        char_pos.push_back(cur_pos);
        sizes.insert(reader->char_length(cur_pos));
        reader->skip_char();
    } while(!reader->eof());

    for(auto pos : char_pos) {
        std::cout << stringformat("there's a char at {}\n", pos);
    }

    for(auto size : sizes) {
        std::cout << stringformat("there's at least one char of size {}\n", size);
    }

    for(int pind = 1; pind < char_pos.size(); pind++) {
        reader->go_to(char_pos[pind - 1]);
        if(reader->char_length(reader->current_position()) > 1) {
            // (this checks to make sure we can't be derailed
            // by not starting at an actual character)
            reader->read_byte();
            // now we're in the middle of a single char...
        }
        reader->skip_char();
        if(reader->current_position() != char_pos[pind]) {
             jerror::error(stringformat(
                 "char {}: expected to be at {} but am at {}\n",
                 pind, char_pos[pind], reader->current_position()
             ));
        }
    }

    return 0;
}

