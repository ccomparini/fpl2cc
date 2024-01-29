#ifndef UTF8_BUFFER_H
#define UTF8_BUFFER_H

#ifndef GENERATED_FPL
  #include "jerror.h"
  #include "src_location.h"
  #include "utf8.h"
#endif // GENERATED_FPL

#include <cstring>
#include <fstream>
#include <string>
#include <vector>

struct utf8_buffer : public std::basic_string<utf8_byte> {
    std::string source;

    utf8_buffer(int dummy = 0, src_location caller = CALLER()) :
        source(caller) { }

    explicit utf8_buffer( 
        const utf8_byte *src,
        size_t num_bytes = -1,
        src_location caller = CALLER()
    ) :
        source(caller)
    {
        if(num_bytes == -1)
            num_bytes = std::strlen((const char *)src);

        assign(src, num_bytes);
    }

    explicit utf8_buffer(
        const std::string &fn, src_location caller = CALLER()
    ) {
        slurp_file(fn, caller);
    }

    utf8_buffer(std::istream &in, src_location caller = CALLER()) {
        slurp_stream(in, caller);
    }

    void slurp_file(const std::string &fn, src_location caller = CALLER()) {
        source = fn;

        std::ifstream in(fn);
        if(!in.is_open()) {
            jerror::error(stringformat(
                "{} can't open '{}': {}",
                caller, fn, std::string(strerror(errno))
            ), caller);
        }

        in.seekg(0, std::ios::end);
        size_t filesize = in.tellg();
        in.seekg(0, std::ios::beg);

        utf8_byte buf[filesize + 1];
        in.read(reinterpret_cast<char *>(buf), filesize + 1);
        buf[filesize] = '\0';
        assign(buf, filesize + 1);
    }

    void slurp_stream(std::istream &in, src_location caller = CALLER()) {
        // This one's annoying, because it may be coming from a
        // file, but afaik there's no way to get the filename (or
        // file descriptor or anything else useful about where the
        // stream is from).  should never have used c++ streams.. sigh.
        source = caller;

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

    /**
        Returns a vector containing the offsets of the
        start of each line in the buffer.

        To match normal line number conventions, the vector
        is 1-based - i.e., line_starts()[1] is the start of
        the first line in the file.  line_starts()[0] happens
        to be the same as line 1, but that may change in
        the future.

     */
    std::vector<size_t> line_starts() const {
        std::vector<size_t> out;

        // line numbers are traditionally 1-based.
        // so, push 0 as the 0th line position.
        // (it has to be 0 because the line starts must
        // be ordered, and size_t is supposed to be unsigned,
        // and line 1 will start at 0)
        out.push_back(0);

        // first line starts at start of buffer:
        out.push_back(0);
        size_t pos = 0;
        while(pos < size()) {
            if(size_t nl_len = utf8::newline_length(data() + pos)) {
                out.push_back(pos + nl_len);
                pos += nl_len;
            } else {
                ++pos;
            }
        }

        return out;
    }

    /**
       Helper function for getting a line number from the result
       of line_starts().

       I would have thought that std::lower_bound would do this (i.e.
       return the lower bound of the range containing the value
       passed - hence the whole line_starts() approach), but it does
       not.  I'm not sure why it's named the way it is.  Anyway,
       use this instead.
     */
    static int line_number(
        const std::vector<size_t> &lines, size_t position
    ) {
        // size_t isn't supposed to be signed, so this probably
        // can't happen, but you never know:
        if(position < 0)
            return 0;

        auto ub = std::upper_bound(lines.begin(), lines.end(), position);
        // upper_bound -does- do at least aproximately what you would
        // expect: that is, it returns the first element whose value is
        // greater than the value passed.  So, in our case, it's the
        // start of the next line, and hence our line is the entry before
        // it.  Decrementing here is safe because we store 0 in the first
        // (and 0th) line entries, so the earliest possible thing
        // upper_bound can return is past those entries.
        --ub;
        return ub - lines.begin();
    }

};


#endif // UTF8_BUFFER_H
