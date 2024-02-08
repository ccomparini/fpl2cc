#ifndef FPL_READER_H
#define FPL_READER_H

/*
   The c++ standard regex engine doesn't work on streams or anything
   like that, so for now anyway we just buffer the entire input.
   Hence this class.
 */

#include <fstream>
#include <functional>
#include <memory>
#include <regex>
#include <string>

#ifndef GENERATED_FPL
  #include "util/fs.h"
  #include "util/jerror.h"
  #include "util/src_location.h"
  #include "util/stringformat.h"
  #include "util/to_hex.h"
  #include "util/utf8.h"
  #include "util/utf8_buffer.h"
#endif // GENERATED_FPL

class fpl_reader;
using fpl_reader_p = std::shared_ptr<fpl_reader>;
using fpl_reader_p_c = std::shared_ptr<const fpl_reader>;

using ErrorCallback = std::function<void(const std::string &error)>;

using LengthCallback = std::function<size_t(const utf8_byte *inp)>;


class fpl_reader {

    std::string input_filename;
    utf8_buffer buffer;

    std::vector<size_t> line_start;

    ErrorCallback on_error;
    size_t read_pos;

    // ... surely there's a better way to get an empty match set..?
    std::cmatch empty_match;
    inline std::cmatch no_match() {
        if(!empty_match.ready()) {
           // generate an empty match set by looking for an arbitrary
           // character in an empty string.  there must be a better way..
           std::regex_search("", empty_match, std::regex("a"));
        }
        return empty_match;
    }

    // Returns true if the position passed would be eof.
    // Throws an error (and returns true) if the position
    // is entirely outside the buffer.
    // See also the simpler eof() in the public section
    inline bool eof(size_t off, src_location caller = CALLER()) const {
        bool is_eof = false;
        if(off >= buffer.length()) {
            // we're completely outside the buffer. count it as
            // eof; in any case callers should not expect reads
            // from that position to work
            is_eof = true;

            // call the on_error callback directly, since the
            // line number is going to be invalid anyway:
            on_error(stringformat(
                "{}: test for eof at invalid offset {} (0x{})",
                caller, off, to_hex(off)
            ));
        } else {
            // else it's eof if we're at the last byte of the file
            is_eof = off == buffer.length() - 1;
        }
        return is_eof;
    }

    // returns a pointer to the next byte of the input
    // buffer.
    inline const utf8_byte *inpp() const {
        return buffer.data() + read_pos;
    }

    // as above, but with a relative offset.
    // returns a pointer to the end of the buffer if the
    // offset is outside the buffer.
    inline const utf8_byte *inpp(size_t offset) const {
        size_t full_pos = read_pos + offset;
        if(full_pos >= buffer.length()) {
            full_pos = buffer.length() - 1;
        }
        return buffer.data() + full_pos;
    }

    // as inpp(), above, but returns as a const char *
    // for ease in passing to standard library stuff.
    inline const char *inpp_as_char(size_t offset = 0) const {
        return reinterpret_cast<const char *>(inpp(offset));
    }

    // returns the line number for the position passed,
    // or 0 if pos is before the start of the buffer
    // or the number of the last line in the file if pos
    // is past the end of the buffer.
    // if *off is non-null, records the (1-based) byte
    // offset of the position within the line as well.
    inline int calc_line_number(
        size_t at, int *off, const std::string &caller = CALLER()
    ) const {

        auto line_num = utf8_buffer::line_number(line_start, at);

        if(off) {
            // store the offset within the line as well,
            // since we've been asked to.  as with lines,
            // offsets are 1 based - hence the +1.
            *off = at - line_start[line_num] + 1;
        }

        return line_num;
    }

public:

    // default error callback/handler:
    static void default_fail(const std::string &msg) {
        fprintf(stderr, "%s\n", msg.c_str());
        exit(1);
    }

    explicit fpl_reader(
        std::istream &input,
        const std::string &infn,
        ErrorCallback ecb = &default_fail
    ) :
        input_filename(infn),
        buffer(input),
        on_error(ecb),
        read_pos(0) {

        line_start = buffer.line_starts();
    }

    explicit fpl_reader(
        utf8_buffer &input,
        const std::string &infn,
        ErrorCallback ecb = &default_fail
    ) :
        input_filename(infn),
        buffer(input),
        on_error(ecb),
        read_pos(0) {
        // for better or worse, we explicitly end the
        // input buffer with a \0:
        buffer.push_back('\0');
   
        line_start = buffer.line_starts();
    }

    explicit fpl_reader(
        const std::string &infn,
        ErrorCallback ecb = &default_fail
    ) :
        input_filename(infn),
        buffer(infn),
        on_error(ecb),
        read_pos(0) {

        line_start = buffer.line_starts();
    }

    inline size_t current_position() const {
        return read_pos;
    }

    // total bytes buffered as of now
    inline size_t total_bytes() const {
        return buffer.length();
    }

    inline size_t bytes_left() const {
        return total_bytes() - current_position();
    }

    // returns the 1-based line number for the position passed.
    // positions past the end of the file evaluate to the last
    // line in the file.
    // if the offset pointer passed is non-null, the utf-8 char
    // offset within the line is saved there.
    inline int line_number(
        size_t pos, int *offset = nullptr, src_location &caller = CALLER()
    ) const {
        return calc_line_number(pos, offset, caller);
    }

    // as above, but returns the current input line number
    inline int line_number(
        int *offset = nullptr, src_location &caller = CALLER()
    ) const {
        return line_number(read_pos, offset, caller);
    }

    inline const std::string &filename() const {
        return input_filename;
    }

    inline std::string location_str(size_t offset=0) const {
        if(offset == 0) offset = read_pos;
        int ch;
        int ln = line_number(offset, &ch);
        return stringformat("{}:{}:{}", filename(), ln, ch);
    }

    std::string base_name() const {

        std::string infn = fs::path(input_filename).filename();

        // "base name" is everything before the first "."
        // in the filename...
        size_t end_of_base = infn.find(".");
        if(end_of_base > 0) {
            return infn.substr(0, end_of_base);
        }

        // .. or, if there's no ".", it's the whole filename:
        return infn;
    }

    // returns the directory portion of the path to the input file
    std::string input_dir() const {
        fs::path in = fs::path(input_filename);
        in.remove_filename();
        return in;
    }

    inline bool eof(src_location caller = CALLER()) const {
        return eof(read_pos, caller);
    }

    inline utf8_byte peek(int offset = 0) const {
        return *inpp(offset);
    }

    // returns the next utf-8 char (at offset relative to the
    // read pointer) as a std::string.
    inline std::string next_char(int offset = 0) const {
        return std::string(inpp_as_char(offset), char_length(offset));
    }

    // formats an error message in the context of this reader.
    std::string format_error_message(
        size_t pos,
        const std::string &msg,
        src_location caller = CALLER()
    ) const {
        const char *nl = "";
        if(msg[msg.length() - 1] != '\n')
            nl = "\n";

        return stringformat("Error {} near «{}»: {}{}",
            location_str(pos), debug_peek(pos, 12), msg, nl
        );
    }


    // Returns the length in bytes of the newline "character" at the
    // position passed.
    // Any 2 bytes in a row with one each of 0x0a and 0x0d counts as
    // a single newline (which covers Microsoft newlines and some
    // old-fashioned newlines).  Also, either of 0x0d or 0x0a alone
    // counts as a newline (which covers unix newlines and some other
    // old fashioned newlines).
    // If at isn't pointing to a newline, returns 0.
    size_t newline_length(size_t at) const {
        if(at >= buffer.length())
            return 0;

        return utf8::newline_length(buffer.data() + at);
    }


private:
    // Returns the length in bytes of the the character at the absolute
    // position passed.  Chracters ar positions outside the read buffer
    // count as 0-byte chars.  Multi-byte newlines are counted as one
    // multi-byte character.
    size_t char_length_abs(size_t pos) const {
        if(pos >= buffer.length()) {
            return 0;
        }

        if(size_t nll = newline_length(pos)) {
            return nll;
        }

        return utf8::character_length(
            buffer.data() + pos, buffer.length() - pos
        );
    }

public:
    // Returns the length in bytes of the the character at the relative
    // position passed. (relative to the current read pointers)
    // For purposes of this function, a character is a single utf-8 encoded
    // character, or a multi-ascii-character newline such as is used by
    // ms dos and descendants (which we count as one character).
    // If the position passed is in the middle of a character, returns
    // the length of the remaining bytes (or tries to - GIGO, at this point).
    // Returns 0 if given a NULL pointer.
    size_t char_length(size_t offset = 0) const {
        return char_length_abs(offset += read_pos);
    }

    inline void go_to(size_t position) {
        read_pos = position;
        if(read_pos >= buffer.length()) {
            read_pos = buffer.length() - 1;
        } else if(read_pos < 0) {
            read_pos = 0;
        }
    }

    void go_to_line(unsigned int line) {
        if(line < line_start.size()) {
            go_to(line_start[line]);
        } // else toss an error?
    }

    inline size_t skip_bytes(size_t skip) {
        size_t start = read_pos;
        go_to(read_pos + skip);
        return read_pos - start;
    }

    // moves the read pointer past the next character
    // and returns the number of bytes it was advanced
    size_t eat_char() {
        return skip_bytes(char_length());
    }

    static size_t space_length(const utf8_byte *inp) {
        return utf8::space_length(inp);
    }

    size_t separator_length(LengthCallback separator_cb = &space_length) {
        size_t len = 0;
        while(size_t adv = separator_cb(inpp(len))) {
            len += adv;
        }

        return len;
    }

    size_t eat_separator(LengthCallback separator_cb = &utf8::space_length) {
        return skip_bytes(separator_length(separator_cb));
    }

    inline char read_byte() {
        if(const utf8_byte *in = inpp()) {
            read_pos++;
            return *in;
        }
        return '\0';
    }

    inline bool read_byte_equalling(utf8_byte this_byte) {
        if(const utf8_byte *in = inpp()) {
            if(*in == this_byte) {
                read_pos++;
                return true;
            }
        }
        return false;
    }

    inline bool read_byte_not_equalling(utf8_byte this_byte) {
        if(const utf8_byte *in = inpp()) {
            if(*in != this_byte) {
                read_pos++;
                return true;
            }
        }
        return false;
    }

    // given the first byte of a potential string, returns
    // the byte to expect to use for the end of that string.
    // this is a helper for parse_string(), below.
    utf8_byte string_end_delimiter(utf8_byte start) {
        switch(start) {
            case '\'': return '\'';
            case '"':  return '"';
            case '`':  return '`';
            case '/':  return '/'; // used for regex
        }
        return '\0';
    }

    //
    // Reads and returns the string of characters delimited by the
    // start-of-string delimiter at the current input position.
    //
    // For example, if the current input is on the single quote
    // at the start of:
    //        'fruitbat'; # hi this is a line of code
    // this reads the "'" and infers that that's the delimiter, then
    // reads through the next "'", returning a string containing
    // "fruitbat", and leaving the read pointer on the ';'.
    //
    // End delimiters (and any other byte!) can be escaped with a
    // backslash.  Note, however, that we always leave the backslash
    // in - otherwise it would hide things like newlines encoded as
    // '\n' from callers.
    //
    // If the initial character doesn't look like anything which
    // can be used as a string delimiter, we return an empty string.
    // 
    // If there's no closing delimiter, it'll return everything up
    // to the end of input and leave the read pointer there.
    //
    // Caveats:
    //   - there's a good chance I'm going to deprecate/remove this
    //   - generally, embedded '\0' are disallowed.  '\0' is treated
    //     as a delimiter, however, so if you escape it you can
    //     actually read a std::string with an embedded '\0' (though
    //     you probably don't want to)
    //   - at the moment, this is all byte oriented, so it probably
    //     won't handle any non-ascii delimiters
    //
    inline std::string parse_string(src_location caller = CALLER()) {
        const size_t start_position = current_position();
        const utf8_byte end_byte = string_end_delimiter(read_byte());
        if(!end_byte) {
            // if end_byte is '\0' it means we're not starting on
            // a valid delimiter
            go_to(start_position); // (rewind)
            return "";
        }
        const char *start = inpp_as_char();

        // read_byte() returns 0 at (or past) end of input,
        // and returning a std::string with an embedded '\0'
        // is just going to cause mayhem, so we count any 0
        // as end of input.
        while(utf8_byte in = read_byte()) {
            if(in == end_byte) {
                break;
            }

            if(in == '\\') {
                read_byte(); // next byte is escaped - just skip
            }
        }

        const char *end = inpp_as_char();
        if(end > start) {
            // normal case, we read the string + the terminating
            // delimiter.  (end - start) is the number we read,
            // and - 1 excludes the terminator.
            return std::string(start, end - start - 1);
        }

        // end <= start is a degenerate case such as you might
        // get trying to parse a string at the last byte of the
        // file or such. returning empty string is the best we
        // can do.
        return "";
    }

    inline std::string read_string(size_t length) {
        auto result = std::string(inpp_as_char(), length);
        skip_bytes(length);
        return result;
    }

    // reads until we pass the next newline.
    // returns all chars up to (but not including)
    // that newline.
    inline std::string read_line() {
        size_t nll;

        const utf8_byte *start = inpp();
        const utf8_byte *end   = inpp();
        while(!(nll = newline_length(read_pos))) {
            eat_char();
        }
        end = inpp();
        skip_bytes(nll);

        return std::string((char *)start, end - start);
    }

    // attempts to read characters exactly matching the string passed.
    // returns true if it did, false otherwise.
    inline bool read_exact_match(const std::string &match) {
        const utf8_byte *result = NULL;

        size_t ml = match.length();
        if(bytes_left() >= ml) {
            if(const char *in = inpp_as_char()) {
                if(!match.compare(0, ml, in, ml)) {
                    result = inpp();
                    read_pos += ml;
                }
            } // else we're at eof
        } // else fewer bytes left than length sought -> no match

        return result;
    }

    // Returns a (possibly empty) string with all characters -until-
    // (but not including) the string passed (or end of input).
    inline std::string read_to_exact_match(const std::string &str) {
        size_t result_len = 0;
        size_t match_len = 0;

        while(utf8_byte nb = peek(result_len + match_len)) {
            if(match_len >= str.length()) {
                break;
            } else if(nb == (utf8_byte)str[match_len]) {
                match_len++;
            } else {
                result_len += match_len + 1;
                match_len = 0;
            }
        }

        // If there was a partial match, we need to add it to the result
        // length.  For example, if the input is "edit"<eof> and pattern
        // is (eg) "item", "ed" will be the first part of the result, then
        // the "it" will match, after which it'll hit eof and no more input.
        // So if the match length is < pattern length, we need to add it
        // in.
        if(match_len < str.length())
            result_len += match_len;

        return read_string(result_len);
    }

    inline std::cmatch read_re(
        const std::string &re, src_location caller = CALLER()
    ) {
        // this doesn't support utf-8 in any reasonable way.
        // btw:
        //  https://stackoverflow.com/questions/37989081/how-to-use-unicode-range-in-c-regex

        std::cmatch matched;
        // match_continuous is so that it will start the
        // match at exactly at the inpp (and ideally won't
        // try to keep matching the rest of the input)
        // https://en.cppreference.com/w/cpp/regex/match_flag_type
        auto opts = std::regex_constants::match_continuous;
        if(const char *in = inpp_as_char()) {

            // OK SO
            // https://en.cppreference.com/w/cpp/regex/syntax_option_type
            //   icase, multiline, nosubs
            // in theory, we can and should make these be options.
            // a reasonable way to do that might be to have callers
            // pass in a constructed re already (might want that so
            // that we're not constantly recompiling regexes anyway).
            // but, in the context of fully buffered files, I'm not
            // sure when you would _not_ want multiline - regexes here
            // already match past newlines, and $ is just going to match
            // the end of the buffer (which in theory could be anywhere,
            // and in practice is end of file - not useful).
            // SO I'm just going to turn on multiline always.
            // cmc - turning it back off again because gnu doesn't
            // support it (thus, I guess it's non-portable).
            // keeping it out of both versions because we wouldn't
            // want regexes to work differently on different platforms.
            // const std::regex cre(re, std::regex::multiline);
            try {
                const std::regex cre(re);
                if(std::regex_search(in, matched, cre, opts))
                    read_pos += matched.length();
            }
            catch(const std::regex_error& err) {
                // doing the error detection here essentially turns
                // a non-compiling regex error into an internal error.
                // so I think we really want callers to compile the
                // regex.  but, that complicates life for the callers,
                // so doing it this way for the moment.
                on_error(stringformat(
                    "/{}/ <- {} (caller: {})", re, err.what(), caller
                ));
                matched = no_match();
            }
        } else {
            // (we are at eof - no match)
            matched = no_match();
        }
        return matched;
    }

    // Reads everything -up to- the regular expression passed
    // (or to end of input, if there's no match)
    inline std::string read_to_re(
        const std::string &re, src_location caller = CALLER()
    ) {
        size_t length = bytes_left();
        if(const char *in = inpp_as_char()) {
            std::cmatch matched;
            // as above, should really compile the regex first.
            try {
                const std::regex cre(re);
                if(std::regex_search(in, matched, cre)) {
                    length = matched.position();
                }
            }
            catch(const std::regex_error& err) {
                on_error(stringformat(
                    "/{}/ <- {} (caller: {})", re, err.what(), caller
                ));
            }
        }
        return read_string(length);
    }

    // "eat" equivalents of the "read" method above simply
    // consume the matching input (without returning it).
    // Instead, they return the number of bytes consumed (i.e.
    // how far the read pointer went forward)
    size_t eat_exact_match(const std::string &match) {
        if(read_exact_match(match))
            return match.length();
        return 0;
    }

    // eats input not matching the string passed (i.e.,
    // consumes input up to the match or to end of input)
    size_t eat_not_exact_match(const std::string &match) {
        // might be more efficient to do read_to_exact_match in
        // term of this (instead of the other way around).  ohwell.
        return read_to_exact_match(match).length();
    }

    size_t eat_re(const std::string &match) {
        return read_re(match).length();
    }

    // as with eat_not_exact_match, consumes input not
    // matching the regular expression string passed
    size_t eat_not_re(const std::string &match) {
        return read_to_re(match).length();
    }

    // pos is the position in the input at which to look; size_t(-1)
    // means use the current read position (the default).
    // num_chars is the maximum number of utf8 chars to
    // look at.
    // since we translate things like newlines (to "\n")
    // and eof, the output length might be more characters
    // than the num_chars passed.
    // if pf_esc is set, we attempt to make the string safe
    // to pass to printf() family functions.
    inline std::string debug_peek(size_t pos, int num_chars = 12) const {
        if(!buffer.data()) return "<NO INPUT>";

        if(pos == size_t(-1)) pos = read_pos;

        if(num_chars < 0) {
            return "¡Negative peek length¡";
        }

        std::string out;
        for(int chp = 0; chp < num_chars; ++chp) {
            if(pos >= buffer.length()) {
                out += "<EOF>";
                break;
            } else if(buffer[pos] == '\0') {
                pos++;
                out += "\\0";
            } else if(int nl = newline_length(pos)) {
                pos += nl;
                out += "\\n";
            } else {
                out += buffer[pos++];
            }
        }
        return out;
    }

    // errrf params here are different from peek()
    inline std::string debug_peek(int num_chars = 12) const {
        return debug_peek(size_t(-1), num_chars);
    }
};

class SourcePosition {
    fpl_reader_p_c source;
    size_t         offset; // byte offset into the file

public:

    SourcePosition() : source(nullptr), offset(0) { }

    SourcePosition(fpl_reader_p src, size_t pos)
        : source(src), offset(pos) {
    }

    SourcePosition(fpl_reader_p src)
        : source(src), offset(src->current_position()) {
    }

    operator bool() const {
        return source != nullptr;
    }

    // errf.. wat?  this keeps it from magically converting to some
    // total nonsense size_t, (probably via bool?) if, for example,
    // we pass one of these to reader->go_to()
    operator size_t() const {
        return offset;
    }

    bool operator==(const SourcePosition &other) const {
        return (offset == other.offset)
            && (source == other.source);
    }

    bool operator!=(const SourcePosition &other) const {
        return !(*this == other);
    }

    const fpl_reader &reader()   const { return *source; }
    size_t            position() const { return offset; }

    std::string debug_peek(
        SourcePosition to_pos = SourcePosition(),
        int max_length = -1
    ) const {
        int length = 12;

        if(!source) {
            return "¡debug_peek: no source reader¡";
        }

        if(to_pos) {
            if(source == to_pos.source) {
                length = to_pos.offset - offset;
            } else {
                return "¡debug_peek source file mismatch: "
                     + source->filename() + " vs "
                     + to_pos.source->filename() + "¡";
            }
        }

        if(max_length >= 0 && length > max_length) {
            length = max_length;
        }

        return source->debug_peek(offset, length);
    }

    int line_number(int *pos = nullptr) const {
        if(source)
            return source->line_number(offset, pos);
        return 0;
    }

    inline std::string filename() const {
        if(source)
            return source->filename();
        return "<no source file>";
    }

    inline std::string to_str() const {
        if(source)
            return source->location_str(offset);

        return "<no source>";
    }
};


#endif // FPL_READER_H


