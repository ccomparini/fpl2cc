#ifndef FPL_READER_H
#define FPL_READER_H

#include <fstream>
#include <regex>
#include <string>

#include "util/fs.h"
#include "util/stringformat.h"
#include "util/to_hex.h"

typedef uint32_t unich; // 4 byte unicode char; for realz, unlike wchar_t
typedef unsigned char utf8_byte;
typedef std::basic_string<utf8_byte> utf8_buffer;

typedef void (ErrorCallback)(const std::string &error);
typedef size_t (LengthCallback)(const utf8_byte *inp);

inline std::string to_std_string(const utf8_byte *str, int len) {
    return std::string(reinterpret_cast<const char *>(str), len);
}

// Returns the length in bytes of the utf-8 character at *at,
// or 0 if that character isn't a space.
//inline size_t space_length(const utf8_byte *at) {
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

    ErrorCallback *on_error;
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
    inline bool eof(const utf8_byte *pos) const {
        long int off = pos - buffer.data();
        bool is_eof = false;
        if((off < 0) || (off >= buffer.length())) {
            // we're completely outside the buffer. count it as
            // eof; in any case callers should not expect reads
            // from that position to work
            is_eof = true;

            // call the on_error callback directly, since the
            // line number is going to be invalid anyway:
            on_error(stringformat(
                "test for eof at invalid position: {} in buffer {}",
                to_hex(pos), to_hex(buffer.data())
            ));
        } else {
            // else it's eof if we're at the last byte of the file
            is_eof = off == buffer.length() - 1;
        }
        return is_eof;
    }

    // as inpp(), below, but returns as a const char *
    // for ease in passing to standard library stuff.
    inline const char *inpp_as_char() {
        return reinterpret_cast<const char *>(inpp());
    }

    // returns the string inside the matching chars.
    // returns empty string if no match.  yeah, it's ambiguous.. hmm
    std::string read_to_match(unich start_match, unich end_match) {
        skip_bytes(1); // XXX convert this whole thing to utf-8
        const utf8_byte *start = inpp();

        if(!*start) return std::string("");

        size_t total_size = 0;
        int depth = 1; // assume we're starting on a match
        do {
            size_t size;
            unich in = unicode_char(size, inpp());
            if(in == start_match) {
                depth++;
            } else if(in == end_match) {
                depth--;
            } else if(in == '\\') {
                // next char is escaped - skip it:
                read_pos   += size;
                total_size += size;
            }
            read_pos   += size;
            total_size += size;
        } while(depth > 0);

        // string length total_size - 1 so as to not include the
        // terminating char
        return to_std_string(start, total_size - 1);
    }

    // returns the line number for the position passed,
    // or -1 if the position is past the end of the buffer,
    // or -2 of the position is before the start of the buffer
    inline int calc_line_number(const utf8_byte *pos) const {
        const utf8_byte *buf = buffer.data();

        // default is to get the line number for the current read position:
        if(!pos) pos = buf + read_pos;

        if(pos > buf + buffer.length()) return -1; // XXX return last line really
        if(pos < buf)                   return -2;

        if(eof(pos)) pos = buf + buffer.length();

        // we rescan for line numbers instead of keeping a counter
        // because (1) it's easier than checking every read, which
        // may or may not be multi-byte or whatever and (2) since
        // the source file is (probably) small, and we only sometimes
        // care about the line number, it's going to be either fast
        // enough, or (with luck) net faster than keeping a line
        // counter and updating it on every read.
        int line_no = 1;
        const utf8_byte *rd;
        for(rd = buf; rd < pos; rd += char_length(rd)) {
            if(newline_length(rd)) {
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

    fpl_reader(const std::string &infn, ErrorCallback *ecb = default_fail) :
        input_filename(infn),
        on_error(ecb),
        read_pos(0)
    {
        std::ifstream in(infn);
        if(!in.is_open()) {
            on_error(stringformat(
                "can't open '{}': {}\n", infn, std::string(strerror(errno))
            ));
        }

        in.seekg(0, std::ios::end);   
        size_t filesize = in.tellg();
        in.seekg(0, std::ios::beg);

        utf8_byte buf[filesize + 1];
        in.read(reinterpret_cast<char *>(buf), filesize + 1);
        buf[filesize] = '\0';
        buffer.assign(buf, filesize + 1);
    }

    inline int current_position() const {
        return read_pos;
    }

    // returns the 1-based line number for the position passed.
    // returns 0 if the position passed is outside the buffer entirely.
    inline int line_number(size_t offset) const {
        return calc_line_number(buffer.data() + offset);
    }

    // as above, but returns the current input line number
    inline int line_number() const {
        return calc_line_number((const utf8_byte *)(NULL));
    }

    inline const std::string &filename() const {
        return input_filename;
    }

    inline std::string location() const {
        return filename() + ":" + std::to_string(line_number());
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

    inline bool eof() const {
        // -1 is because we stuff a '\0' at the end of the buffer
        return read_pos >= buffer.length() - 1;
    }

private:
    // returns a pointer to the next byte of the input
    // buffer.
    inline const utf8_byte *inpp() const {
        return buffer.data() + read_pos;
    }
public:

    inline utf8_byte peek() const {
        const utf8_byte *byte = inpp();
        return byte?*byte:'\0';
    }

    void error(const std::string &msg) const {
        const char *nl = "";
        if(msg[msg.length() - 1] != '\n')
            nl = "\n";

        std::string full_msg(
            stringformat("Error {} near «{}»: {}{}",
                location(), pf_debug_peek(), msg, nl
            )
        );

        if(on_error) {
            on_error(full_msg);
        } else {
            fprintf(stderr, "%s\n", full_msg.c_str());
        }
    }

    // Returns the length in bytes of the newline "character" at *at.
    // Any 2 bytes in a row with one each of 0x0a and 0x0d counts as
    // a single newline (which covers Microsoft newlines and some
    // old-fashioned newlines).  Also, either of 0x0d or 0x0a alone
    // counts as a newline (which covers unix newlines and some other
    // old fashioned newlines).
    // If at isn't pointing to a newline, returns 0.
    static inline size_t newline_length(const utf8_byte *at) {
        if(at == NULL) return 0;

        if(*at == 0x0d) {
            if((*at + 1) == 0x0a) return 2; // Microsoft newline
            return 1; // OS-9 style newline (heh)
        }

        if(*at == 0x0a) {
            if((*at + 1) == 0x0d) return 2; // weirdo old British newline
            return 1; // normal unix newline
        }

        return 0;
    }

    // Returns the length of the encoding of the character at *in, in bytes.
    // For purposes of this function, a character is a single utf-8 encoded
    // character, or a multi-ascii-character newline, such as is used by
    // ms dos and descendants.
    // If the pointer passed points to the middle of a character, returns
    // the length of the remaining bytes (or tries to - GIGO, at this point).
    // Returns 0 if given a NULL pointer.
    static size_t char_length(const utf8_byte *in) {
        if(!in) return 0;

        if(size_t nll = newline_length(in)) {
            return nll;
        }

        // https://en.wikipedia.org/wiki/UTF-8#Encoding
        // if the high bit isn't set, it's a single byte:
        if((*in & 0x80) == 0) return 1;

        // otherwise, if we're at the start of the character,
        // the top 2 bits will be set, and bits following
        // specify the size.  so if the top 2 bits are set,
        // we'll assume we're at the start of a char and take
        // that byte's word for it on the size:
        if((*in & 0xe0) == 0xc0) return 2;  // 0b110x xxxx
        if((*in & 0xf0) == 0xe0) return 3;  // 0b1110 xxxx
        if((*in & 0xf8) == 0xf0) return 4;  // 0b1111 0xxx

        // looks like we're in the middle of a character.
        // count bytes until the start of a new character,
        // which we can identify by either the top bit being 0
        // or the top 2 bits being 1:
        const utf8_byte *rd = in;
        for(rd = in; *rd & 0x80; rd++) {
            if((*in & 0xc0) == 0xc0) break; // start of char w/ cp > 127
        }
        return rd - in + 1;
    }

    // XXX kill this and/or rename to unicode_codepoint or such...
    // or something.  and move it.
    // The value of size_out will be set to the size in bytes of
    // the utf-8 representation of the character (scanned from *in)
    static unich unicode_char(size_t &size_out, const utf8_byte *in) {
        if(!in) {
            size_out = 0;
            return '\0';
        }

        unich out;
        uint8_t acc = in[0];
        out = acc & 0x7f;
        size_out = 1;
        while((acc & 0xc0) == 0xc0) {
            acc <<= 1;
            unich inb = in[size_out];
            if((inb & 0xc0) != 0x80) {
                // invalid input...
                fprintf(stderr, "invalid utf-8 byte 0x%0x\n", inb);
                // (but I guess blaze on..)
            }
            out |= inb << size_out*6;
            size_out++;
        }

        return out;
    }

    inline void go_to(size_t position) {
        read_pos = position;
    }

    inline void skip_bytes(int skip) {
        read_pos += skip;
    }

    inline void skip_char() {
        read_pos += char_length(inpp());
    }

    void eat_separator(LengthCallback *separator_cb = space_length) {
        while(size_t adv = separator_cb(inpp())) {
            skip_bytes(adv);
        }
    }

    std::string read_to_separator(LengthCallback *sep_cb = space_length) {
        const utf8_byte *start = inpp();

        if(!start)
            return std::string("");

        size_t length = 0;
        while(const utf8_byte *in = inpp()) {
            if(sep_cb(in))
                break;

            size_t len = char_length(in);
            length += len;
            skip_bytes(len);
        }

        return to_std_string(start, length);
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

    inline std::string read_to_byte(utf8_byte end_char) {
        return read_to_match('\0', end_char);
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
        while(!(nll = newline_length(inpp()))) {
            skip_char();
            end = inpp();
        }
        skip_bytes(nll);

        return std::string((char *)start, end - start);
    }

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

    inline std::cmatch read_re(const std::string &re) {
        // XXX this doesn't support utf-8 or unicode in any
        // reasonable way.  btw:
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
            const std::regex cre(re, std::regex::multiline);
            if(std::regex_search(in, matched, cre, opts))
                read_pos += matched.length();
// XXX wait what?  we _can _ get here on matches of "0 bytes".  we don't want to return true in such cases. investigate.
// fprintf(stderr, "/%s/ matched %li bytes!  specifically: (%s)\n", re.c_str(), matched.length(), matched.str().c_str());
            // (matched is now set to whatever was matched, if anything)
        } else {
            // (we are at eof - no match)
            matched = no_match();
        }
        return matched;
    }

    // pos is the position in the input at which to look; < 0
    // means use the current read position (the default).
    // num_chars is the maximum number of utf8 chars to 
    // look at.
    // since we translate things like newlines (to "\n")
    // and eof, the output length might be more characters
    // than the num_chars passed.
    // if pf_esc is set, we attempt to make the string safe
    // to pass to printf() family functions.
    inline std::string debug_peek(
        int pos = -1, int num_chars = 12, bool pf_esc = true
    ) const {
        if(!buffer.data()) return "<NO INPUT>";

        if(pos < 0) pos = read_pos;

        std::string out;
        const utf8_byte *inp = buffer.data() + pos;
        for(int chp = 0; chp < num_chars; ++chp) {
            if(eof(inp)) {
                out += "<EOF>";
                break;
            } else if(newline_length(inp) > 0) {
                out += "\\n";
            } else if(pf_esc && *inp == '%') {
                out += "%%";
            } else {
                out += *inp;
            }
            inp += char_length(inp);
        }
        return out;
    }
    inline std::string pf_debug_peek(int pos = -1, int num_chars = 12) const {
        return debug_peek(pos, num_chars, true);
    }

};


#endif // FPL_READER_H

