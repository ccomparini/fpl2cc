
// ^^ turns out starting with a newline was what threw
// line numbers off.  doh.
#include "fpl2cc/fpl_reader.h"
#include "util/jerror.h"
//hey wat

// ÿ and 🐈 are multibyte chars,
// which don't seem to be the problem

#include <iostream>

int main(int argc, const char **argv) {
    std::shared_ptr<fpl_reader> reader = std::make_shared<fpl_reader>(
        __FILE__
    );

    std::cout << stringformat(
        "line {} is before 'this line' stuff\n", __LINE__
    );
    const int this_line = __LINE__;
    reader->go_to_line(this_line);
    std::cout << stringformat(
        "this line ({} = {}):\n'{}'\n",
        this_line, SourcePosition(reader),
        reader->debug_peek(-1, 35)
    );

    // scanning this whole file was useful in figuring out why
    // line numbers were sometimes off by one, so I'm keeping it,
    // even though it makes the output a bit big and brutal:
    reader->go_to(0);
    while(!reader->eof()) {
        SourcePosition pos(reader);
        std::cout << stringformat(
            "{}: '{}'\n",
            pos, reader->debug_peek(-1, 6)
        );
        reader->eat_char();
    }
}

