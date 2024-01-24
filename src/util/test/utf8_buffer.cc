
#include "stringformat.h"
#include "utf8_buffer.h"

#include <algorithm>
#include <iostream>
#include <string>

std::string read_line(const utf8_byte *src) {
    int len;
    for(len = 0; src[len] && src[len] != '\n'; len++)
        ;

    return std::string((const char *)src, len);
}

void dump_line_data(const utf8_buffer &buf) {
    auto lines = buf.line_starts();
    std::cout << stringformat("lines start at: {}\n", lines);

    for(int line_num = 1; line_num < lines.size(); ++line_num) {
        auto pos = lines[line_num];
        std::cout << stringformat(
            "{} (byte {}): '{}'\n",
            line_num, pos, read_line(buf.data() + pos)
        );
    }

    // note we include the position at buf.size()
    for(size_t pos = 0; pos <= buf.size(); ++pos) {
        auto line_num = utf8_buffer::line_number(lines, pos);
        std::cout << stringformat(
            "byte {} is on line {} (starting at {}). rest of line: '{}'\n",
            pos, line_num, lines[line_num],
            read_line(buf.data() + pos)
        );
    }

    auto outside = utf8_buffer::line_number(lines, buf.size() + 1);
    std::cout << stringformat(
        "line number just past the end gives {} (starts at {})\n",
        outside, lines[outside]
    );
    outside = utf8_buffer::line_number(lines, buf.size() + 23);
    std::cout << stringformat(
        "asking for the line number way past the end gives {}\n",
        outside
    );
}

int main() {
    const char *data_in = 
        "one\n"
        "two\n"
        "three\n\n\n"
        "six\n";
    const auto data_size = strlen(data_in);
    utf8_buffer buf((const utf8_byte *)data_in, data_size);

    dump_line_data(buf);

    // what happens if there are no newlines at all?
    dump_line_data(utf8_buffer((const utf8_byte *)"hi there"));

    // does starting with a newline break it?  it did at one point.
    dump_line_data(utf8_buffer((const utf8_byte *)"\nline 2\nline 3\n"));

}

