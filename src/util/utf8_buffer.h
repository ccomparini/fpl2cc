#ifndef UTF8_BUFFER_H
#define UTF8_BUFFER_H

#include <fstream>
#include <string>

typedef unsigned char utf8_byte;

struct utf8_buffer : public std::basic_string<utf8_byte> {
    utf8_buffer() { }

    explicit utf8_buffer(const utf8_byte *src, size_t num_bytes) {
        assign(src, num_bytes);
    }

    explicit utf8_buffer(const std::string &fn) {
        slurp_file(fn);
    }

    utf8_buffer(std::istream &in) {
        slurp_stream(in);
    }

    void slurp_file(const std::string &fn) {
        std::ifstream in(fn);
        if(!in.is_open()) {
            jerror::error(stringformat(
                "can't open '{}': {}\n", fn, std::string(strerror(errno))
            ));
        }

        in.seekg(0, std::ios::end);
        size_t filesize = in.tellg();
        in.seekg(0, std::ios::beg);

        utf8_byte buf[filesize + 1];
        in.read(reinterpret_cast<char *>(buf), filesize + 1);
        buf[filesize] = '\0';
        assign(buf, filesize + 1);
    }

    void slurp_stream(std::istream &in) {
        const size_t bufsize = 2<<16;
        utf8_byte buf[bufsize];
        while(!in.eof()) {
            in.read((char *)buf, bufsize - 1);
            size_t bytes_read = in.gcount();
            buf[bytes_read] = '\0';
            *this += buf;
        }
        *this += '\0';
    }
};


#endif // UTF8_BUFFER_H
