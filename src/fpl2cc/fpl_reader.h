#ifndef FPL_READER_H
#define FPL_READER_H

#include <fstream>
#include <functional>
#include <memory>
#include <regex>
#include <string>

#include "util/fs.h"
#include "util/src_location.h"
#include "util/stringformat.h"
#include "util/to_hex.h"

typedef unsigned char utf8_byte;
typedef std::basic_string<utf8_byte> utf8_buffer;

class fpl_reader;
using fpl_reader_p = std::shared_ptr<fpl_reader>;
using fpl_reader_p_c = std::shared_ptr<const fpl_reader>;

using ErrorCallback = std::function<void(const std::string &error)>;

// returns a utf8 buffer containing the contents
// of the file indicated by the filename passed.
utf8_buffer slurp_file(const std::string &fn, ErrorCallback err) {
    // I hope that this is inlined and doesn't end up making
    // extra copies of dest...
    utf8_buffer dest;
    std::ifstream in(fn);
    if(!in.is_open()) {
        err(stringformat(
            "can't open '{}': {}\n", fn, std::string(strerror(errno))
        ));
    }

    in.seekg(0, std::ios::end);
    size_t filesize = in.tellg();
    in.seekg(0, std::ios::beg);

    utf8_byte buf[filesize + 1];
    in.read(reinterpret_cast<char *>(buf), filesize + 1);
    buf[filesize] = '\0';
    dest.assign(buf, filesize + 1);
    return dest;
}

using LengthCallback = std::function<size_t(const utf8_byte *inp)>;

inline std::string to_std_string(const utf8_byte *str, int len) {
    return std::string(reinterpret_cast<const char *>(str), len);
}

// Returns the length in bytes of the utf-8 character at *at,
// or 0 if that character isn't a space.
size_t space_length(const utf8_byte *at) {
    if(at == NULL) return 0;

    switch(*at) {
        // ascii ones are simple and common:
        case 0x09:    // character tabulation (aka "tab")
        case 0x0A:    // line feed
        case 0x0B:    // line tabulation
        case 0x0C:    // form feed
        case 0x0D:    // carriage return
        case 0x20:    // space
            return 1;
        case 0xc2:
            if(at[1] == 0x85) return 2; // U+0085 = next line
            if(at[1] == 0xa0) return 2; // U+00A0 = no-break space
            return 0;
        case 0xe1:
            if(at[1] == 0x9a && at[2] == 0x80)
                return 3; // 0xe1,0x9a,0x80 = U+1680 = ogham space mark
            return 0;
        case 0xe2:
            if(at[1] == 0x80) {
                if(at[2] >= 0x80 && at[2] <= 0x8a) {
                    // 0xe2,0x80,0x80 -> U+2000 = en quad
                    // 0xe2,0x80,0x81 -> U+2001 = em quad
                    // 0xe2,0x80,0x82 -> U+2002 = en space
                    // 0xe2,0x80,0x83 -> U+2003 = em space
                    // 0xe2,0x80,0x84 -> U+2004 = three-per-em space
                    // 0xe2,0x80,0x85 -> U+2005 = four-per-em space
                    // 0xe2,0x80,0x86 -> U+2006 = six-per-em space
                    // 0xe2,0x80,0x87 -> U+2007 = figure space
                    // 0xe2,0x80,0x88 -> U+2008 = punctuation space
                    // 0xe2,0x80,0x89 -> U+2009 = thin space
                    // 0xe2,0x80,0x8a -> U+200A = hair space
                    return 3;
                }

                if(at[2] == 0xa8)
                    return 3; // 0xe2,0x80,0xa8 -> U+2028 = line separator

                if(at[2] == 0xa9)
                    return 3; // 0xe2,0x80,0xa9 -> U+2029 = paragraph separator

                if(at[2] == 0xaf)
                    return 3; // 0xe2,0x80,0xaf -> U+202F = narrow no-break sp.

            } else if(at[1] == 0x81 && at[2] == 0x9f) {
                return 3; // 0xe2,0x81,0x9f -> 205F = medium mathematical space
            }
            return 0;

        case 0xe3:
            if(at[1] == 0x80 && at[2] == 0x80)
                return 3; // 0xe3,0x80,0x80 = U+3000 ideographic space
            return 0;

        default:
            // not space
            return 0;
    }

    // can't get here.
}

class fpl_reader {

    std::string input_filename;
    utf8_buffer buffer;

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

    inline size_t bytes_left() const {
        return buffer.length() - read_pos;
    }

    // returns true if the position passed would be eof.
    // throws an error (and returns true) if the position
    // is entirely outside the buffer
    // see also the simpler eof() in the public section
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
    inline const char *inpp_as_char() {
        return reinterpret_cast<const char *>(inpp());
    }

    // returns the line number for the position passed,
    // or 0 if pos is before the start of the buffer
    // or the number of the last line in the file if pos
    // is past the end of the buffer.
    inline int calc_line_number(size_t at, const std::string &caller = CALLER()) const {

        if(at >= buffer.length()) {
            fprintf(stderr, "%s", stringformat(
                "warning {}: position {} is outside file '{}' (len {})"
                " by {} bytes\n",
                caller, at, filename(), buffer.length(),
                at - buffer.length()
            ).c_str());
            at = buffer.length() - 1;
        }

        // we rescan for line numbers instead of keeping a counter
        // because (1) it's easier than checking every read, which
        // may or may not be multi-byte or whatever and (2) since
        // the source file is (probably) small, and we only sometimes
        // care about the line number, it's going to be either fast
        // enough, or (with luck) net faster than keeping a line
        // counter and updating it on every read.
        // (if it turns out not to be fast enough, make an index)
        int line_no = 1;
        size_t pos;
        for(pos = 0; pos < at; pos += char_length(pos)) {
            if(newline_length(pos)) {
                line_no++;
            }
        }
        return line_no;
    }

public:

    // default error callback/handler:
    static void default_fail(const std::string &msg) {
        fprintf(stderr, "%s\n", msg.c_str());
        exit(1);
    }

    fpl_reader(
        utf8_buffer &input,
        const std::string &infn,
        ErrorCallback ecb = &default_fail
    ) :
        input_filename(infn),
        buffer(input),
        on_error(ecb),
        read_pos(0)
    { }

    fpl_reader(const std::string &infn, ErrorCallback ecb = &default_fail) :
        input_filename(infn),
        buffer(slurp_file(infn, ecb)),
        on_error(ecb),
        read_pos(0)
    {
    }

    inline int current_position() const {
        return read_pos;
    }

    // returns the 1-based line number for the position passed.
    // positions past the end of the file evaluate to the last
    // line in the file.
    inline int line_number(size_t pos, src_location &caller = CALLER()) const {
        return calc_line_number(pos, caller);
    }

    // as above, but returns the current input line number
    inline int line_number(src_location &caller = CALLER()) const {
        return line_number(read_pos, caller);
    }

    inline const std::string &filename() const {
        return input_filename;
    }

    inline std::string location_str(size_t offset) const {
        return filename() + ":" + std::to_string(line_number(offset));
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

    inline bool eof() const {
        return eof(read_pos);
    }

public:

    inline utf8_byte peek(int offset = 0) const {
        return *inpp(offset);
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
            location_str(pos), debug_peek(pos), msg, nl
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
private:
    inline size_t newline_length(size_t at) const {
        if(at >= buffer.length())
            return 0;

        if(buffer[at] == 0x0d) {
            if(buffer[at + 1] == 0x0a) return 2; // Microsoft newline
            return 1; // OS-9 style newline (heh)
        }

        if(buffer[at] == 0x0a) {
            if(buffer[at + 1] == 0x0d) return 2; // weirdo old British newline
            return 1; // normal unix newline
        }

        return 0;
    }

    inline size_t newline_length(
        const utf8_byte *at, src_location caller = CALLER()
    ) const {
        fprintf(stderr,
            "call to deprecated newline_length(ptr) at %s\n", caller.c_str()
        );
        return newline_length(at - buffer.data());
    }

    // Returns the length of the encoding of the character at *in, in bytes.
    // For purposes of this function, a character is a single utf-8 encoded
    // character, or a multi-ascii-character newline, such as is used by
    // ms dos and descendants.
    // If the pointer passed points to the middle of a character, returns
    // the length of the remaining bytes (or tries to - GIGO, at this point).
    // Returns 0 if given a NULL pointer.
    size_t char_length(size_t pos) const {
        if(size_t nll = newline_length(pos))
            return nll;

        // https://en.wikipedia.org/wiki/UTF-8#Encoding
        // if the high bit isn't set, it's a single byte:
        if((buffer[pos] & 0x80) == 0) return 1;

        // otherwise, if we're at the start of the character,
        // the top 2 bits will be set, and bits following
        // specify the size.  so if the top 2 bits are set,
        // we'll assume we're at the start of a char and take
        // that byte's word for it on the size:
        if((buffer[pos] & 0xe0) == 0xc0) return 2;  // 0b110x xxxx
        if((buffer[pos] & 0xf0) == 0xe0) return 3;  // 0b1110 xxxx
        if((buffer[pos] & 0xf8) == 0xf0) return 4;  // 0b1111 0xxx

        // looks like we're in the middle of a character.
        // count bytes until the start of a new character,
        // which we can identify by the top 2 bits being
        // anything other than 0b10:
// XXX test this case
        size_t len = 0;
        for( ; pos + len < buffer.length(); ++len) {
            // 0xc0 = 0b11000000
            if((buffer[pos + len] & 0xc0) != 0xc0)
                break;
        }

        return len;
    }

public:

    inline void go_to(size_t position) {
        read_pos = position;
    }

    inline size_t skip_bytes(size_t skip) {
        read_pos += skip;
        return skip;
    }

    // skips the current utf-8 character
    inline void skip_char() {
        read_pos += char_length(read_pos);
    }

    size_t separator_length(LengthCallback separator_cb) {
        size_t len = 0;
        while(size_t adv = separator_cb(inpp(len))) {
            len += adv;
        }

        return len;
    }

    size_t eat_separator(LengthCallback separator_cb = &space_length) {
        return skip_bytes(separator_length(separator_cb));
    }

    inline char read_byte() {
        if(const utf8_byte *in = inpp()) {
            read_pos++;
            return *in;
        }
        return '\0';
    }

    inline bool read_byte_equalling(char chr) {
        if(const utf8_byte *in = inpp()) {
            if(*in == chr) {
                read_pos++;
                return true;
            }
        }
        return false;
    }

    // reads and returns the string of characters delimited by the
    // current input byte, consuming the delimiters.  for example,
    // if the current input is on the single quote at the start of:
    //        'fruitbat'; # hi this is a line of code
    // this read the "'" and infer that that's the delimiter, then
    // read through the next "'", returning a string containing
    // "fruitbat", leaving the read pointer on the ';'.
    //
    // If there's no closing delimiter, it'll return everything up
    // to the end of input, which is kinda lame.
    //
    // Backslash can be used to escape delimiters, but the backslash
    // will be included in the returned string.  Arguably, that's
    // a bug, but it matches the behavior of what this is replacing
    // so I'm just keeping it for now.  ship it.
    //
    // Caveats:
    //   - there's a good chance I'm going to deprecate/remove this
    //   - generally, embedded '\0' are disallowed.  '\0' is treated
    //     as a delimiter, however, so if you escape it you can
    //     actually read a std::string with an embedded '\0' (though
    //     you probably don't want to)
    //   - at the moment, this is all byte oriented, so it probably
    //     won't handle any non-ascii delimiters in a way you'd expect.
    //     (may change, but more likely will deprecate the whole thing)
    //
    inline std::string parse_string(src_location caller = CALLER()) {
if(char_length(read_pos) > 1) {
fprintf(stderr, "whoa dude this is going to break because the char length is %lu", char_length(read_pos));
}
        const utf8_byte end_byte = read_byte();
        const char *start = inpp_as_char();

        // read_byte() returns 0 at (or past) end of input,
        // and returning a std::string with an embedded '\0'
        // is just going to cause mayhem, so we count any 0
        // as end of input.
        while(utf8_byte in = read_byte()) {
             if(in == end_byte)
                 break;

             if(in == '\\')
                 read_byte(); // next byte is escaped - just skip
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

    inline std::string read_range(size_t start, size_t end) const {
        return to_std_string(buffer.data() + start, end - start);
    }

    // reads until we pass the next newline.
    // returns all chars up to (but not including)
    // that newline.
    inline std::string read_line() {
        size_t nll;

        const utf8_byte *start = inpp();
        const utf8_byte *end   = inpp();
        while(!(nll = newline_length(read_pos))) {
            skip_char();
        }
        end = inpp();
        skip_bytes(nll);

        return std::string((char *)start, end - start);
    }

    // XXX this is only ever used in boolean context, so don't return a ptr
    // the caller knows what it matched anyway, since the caller passed it.
    inline const utf8_byte *read_exact_match(const std::string &match) {
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
            }
// XXX wait what?  we _can _ get here on matches of "0 bytes".  we don't want to return true in such cases. investigate.
// fprintf(stderr, "/%s/ matched %li bytes!  specifically: (%s)\n", re.c_str(), matched.length(), matched.str().c_str());
            // (matched is now set to whatever was matched, if anything)
        } else {
            // (we are at eof - no match)
            matched = no_match();
        }
        return matched;
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
    inline std::string debug_peek(size_t pos, int num_chars) const {
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

    inline operator bool() const {
        return source != nullptr;
    }

    const fpl_reader &reader()   const { return *source; }
    size_t            position() const { return offset; }

    std::string debug_peek(SourcePosition to_pos = SourcePosition()) const {
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

        return source->debug_peek(offset, length);
    }

    inline int line_number() const {
        if(source)
            return source->line_number(offset);
        return 0;
    }

    inline std::string filename() const {
        if(source)
            return source->filename();
        return "<no source file>";
    }

    inline std::string to_str() const {
        return filename() + ":" + std::to_string(line_number());
    }
};


#endif // FPL_READER_H


