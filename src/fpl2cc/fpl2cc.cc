#include <climits>
#include <fstream>
#include <map>
#include <queue>
#include <regex>
#include <set>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdexcept>
#include <string>
#include <vector>


void fail(const char *fmt...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    exit(1);
}

/*

  fpl grammar:

   <exprs to match> -> <production name> { <code> }
                   or
   <exprs to match> -> <production name> ;

  In the first case, the <code> block returns a member (see below).
  In the second case, the ; tells it to return the string matching
  whatever's on the left of the ->.  ";" is only valid if
  everything on the left evaluates to a string.

  Expressions may be any of:
   - double-quoted string ("xxx") - match text + optional trailing whitespace
   - regular expression within slashes (eg /0x[0-9a-fA-F]+/)
   - names of other productions as plain text (no spaces)

  Expressions may be followed (no space) by one of *, +, or ?
  to mean 0-or-more, 1-or-more, or 0-or-1 respectively.

  Comments start with "#" and go to the end of the line.

  All input tokens are separated by space.

  Code blocks:

  Code blocks are enclosed in { }.

  Code blocks access their corresponding expressions via pseudo-positional
  argument variables named arg[0..x] (so the first is called arg0, second
  arg1, etc).  The type of each argument variable depends on the types 
  and (possible) repetitions of the expressions:
    - quoted string      -> std::string
    - regular expression -> std::smatch
    - production name    -> Jest::Member

  If there's any repetition,... XXX

  Code blocks should use a normal "return" statement to return a pointer
  to a Jest::Member.

 */

typedef uint32_t unich; // 4 byte unicode char; for realz, unlike wchar_t
typedef void (ErrorCallback)(const char *fmt...);

typedef unsigned char utf8_byte;
typedef std::basic_string<utf8_byte> utf8_buffer;

inline std::string to_std_string(const utf8_byte *str, int len) {
    return std::string(reinterpret_cast<const char *>(str), len);
}

// Returns the length in bytes of the newline "character" at *at.
// Any 2 bytes in a row with one each of 0x0a and 0x0d counts as
// a single newline (which covers Microsoft newlines and some
// old-fashioned newlines).  Also, either of 0x0d or 0x0a alone
// counts as a newline (which covers unix newlines and some other
// old fashioned newlines).
// If at isn't pointing to a newline, returns 0.
// The input is expected to be '\0' terminated.
inline size_t newline_length(const utf8_byte *at) {
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

// Returns the length in bytes of the utf-8 character at *at,
// or 0 if that character isn't a space.
inline size_t space_length(const utf8_byte *at) {
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

// Returns the length of the encoding of the character at *in, in bytes.
// For purposes of this function, a character is a single utf-8 encoded
// character, or a multi-ascii-character newline, such as is used by
// ms dos and descendants.
// If the pointer passed points to the middle of a character, returns
// the length of the remaining bytes (or tries to - GIGO, at this point).
// Returns 0 if given a NULL pointer.
static size_t char_length(const utf8_byte *in) {
    if(size_t nll = newline_length(in)) {
        return nll;
    }

    if(!in) return 0;

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
        if((*in & 0xc0) == 0xc0) break; // start of char w/ code point > 127
    }
    return rd - in + 1;
}

int line_number(const utf8_byte *start, const utf8_byte *end) {

    // we rescan for line numbers instead of keeping a counter
    // because (1) it's easier than checking every read, which
    // may or may not be multi-byte or whatever and (2) since
    // the source file is (probably) small, and we only sometimes
    // care about the line number, it's going to be either fast
    // enough, or (with luck) net faster than keeping a line
    // counter and updating it on every read.
    int line_no = 1;
    const utf8_byte *rd;
    for(rd = start; rd < end; rd += char_length(rd)) {
        if(newline_length(rd)) {
            line_no++;
        }
    }
    return line_no;
}

class SourceReader {
    std::string input_filename;
    utf8_buffer buffer;

    ErrorCallback *on_error;
    size_t read_pos;
public:
    //SourceReader(std::ifstream &in, ErrorCallback *ecb) :
    SourceReader(const char *infn, ErrorCallback *ecb) :
        input_filename(infn),
        on_error(ecb),
        read_pos(0)
    {
        std::ifstream in(infn);
        if(!in.is_open()) {
            fail("can't open '%s': %s\n", infn, strerror(errno));
        }

        in.seekg(0, std::ios::end);   
        size_t filesize = in.tellg();
        in.seekg(0, std::ios::beg);

        // stdlib templates are giving me a pita with the iterator approach
        // because of the char/utf8_byte thing so Im just going to buffer
        // and copy
        utf8_byte buf[filesize + 1];
        in.read(reinterpret_cast<char *>(buf), filesize + 1);
        buf[filesize] = '\0';
        buffer.assign(buf, filesize + 1);
    }

    std::string base_name() const {
        // "base name" is everything before the first "."
        // in the filename...
        size_t end_of_base = input_filename.find(".");
        if(end_of_base > 0) {
            return input_filename.substr(0, end_of_base);
        }

        // .. or, if there's no ".", it's the whole filename:
        return input_filename;
    }

    void error(const char *fmt...) {
        const int buf_size = 1024;
        char msg_fmt[buf_size];
        snprintf(msg_fmt, buf_size,
            "Error line %i near \"%.12s\": %s\n", line_number(), inpp(), fmt
        );

        char full_msg[buf_size];
        va_list args;
        va_start(args, fmt);
        vsnprintf(full_msg, buf_size, msg_fmt, args);
        va_end(args);

        on_error(full_msg);
    }

    inline bool eof() {
        // -1 is because we stuff a '\0' at the end of the buffer
        return read_pos >= buffer.length() - 1;
    }

    inline const utf8_byte *inpp() {
        if(!eof()) {
            return buffer.data() + read_pos;
        } else {
            return NULL;
        }
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

    int line_number(const utf8_byte *up_to = NULL) {
        return ::line_number(buffer.data(), up_to);
    }

    inline void skip_bytes(int skip) {
        read_pos += skip;
    }

    inline void skip_char() {
        read_pos += char_length(inpp());
    }

    void eat_space() {
        while(size_t adv = space_length(inpp())) {
            skip_bytes(adv);
        }
    }

    void eat_comment() {
        size_t nll;
        while(!(nll = newline_length(inpp()))) {
            skip_char();
        }
        skip_bytes(nll); // line comment includes the terminating newline
    }

    std::string read_to_space() {
        const utf8_byte *start = inpp();
        size_t length = 0;
        while(const utf8_byte *in = inpp()) {
            if(space_length(in))
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

    inline std::cmatch read_re(const char *re) {
        std::cmatch matched;
        // match_continuous is so that it will start the
        // match at exactly at the inpp (and ideally won't
        // try to keep matching the rest of the input)
        auto opts = std::regex_constants::match_continuous;
        const char *inp = reinterpret_cast<const char *>(inpp());
        if(std::regex_search(inp, matched, std::regex(re), opts)) {
            read_pos += matched.length();
        }
        return matched;
    }

    // returns the string inside the matching chars.
    // returns empty string if no match.  yeah, it's ambiguous.. hmm
    std::string read_to_match(unich start_match, unich end_match) {
        skip_bytes(1); // XXX convert this whole thing to utf-8
        const utf8_byte *start = inpp();

        if(!*start) return std::string("");

        size_t total_size = 0;
        // XXX how did passing '\0' as start of match work before, when this
        // started at depth 0?  test this whole thing.  the escaping is also suspect.
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

};

/*
   Returns a version of the string passed which is suitable for
   embedding in a c (or c++) program
 */
std::string c_str_escape(const std::string src) {
    std::string escaped;
    for(const char &inch : src) {
        // escape quotes and backslashes so that they make
        // it through the c compiler without prematurely
        // terminating the string:
        if(inch == '"' || inch == '\\') {
            escaped += '\\';
        } // else normal char

        escaped += inch;
    }
    return escaped;
}

/*

Production rules are ordered;  first one matches.

Production rules can be looked up by name.

Each production rule is an array of things to match (items),
and a code block (string).  Each item has a minimum and maximum
number of times to match (default 1; may be 0, 1, or, in the
max case, infinite).

Generating code: do we need an entry rule? (maybe a built-in
beginning of file match?) (if so, this would give a smaller
set of initially applicable rules to search.  yes, do this,
or make it so the first rule is the first set).
Anyway, given a certain position in the text,

   - Attempt to match the text at the current position vs the next
     position in each applicable rule.  If it continues to match,
     that rule stays in the set of applicable rules; otherwise, it's
     out.  .. how to recurse on named rules?  can we do it without
     recursing per se?

   - if a complete rule matches, we call the sub whose body is
     in the "code" part of the rule, with arguments derived from
     the matches.
 
 */

struct GrammarElement {
    std::string expr; // either a string, regex, or name of product
    typedef enum {
        NONE,
        TERM_EXACT,
        TERM_REGEX,
        NONTERM_PRODUCTION,
    } Type;
    Type type;

    GrammarElement(const std::string &str, Type tp)
        : expr(str), type(tp) { }

    // returns a negative, 0, or positive value depending on
    // if this element can be considered <, ==, or > than the
    // other element:
    inline int compare(const GrammarElement &other) const {
        int cmp = type - other.type;
        if(cmp == 0) {
            cmp = expr.compare(other.expr);
        }
        return cmp;
    }
    friend bool operator<(const GrammarElement& left, const GrammarElement& right) {
        return left.compare(right) < 0;
    }

    inline bool is_terminal() const {
        return(type == TERM_EXACT || type == TERM_REGEX);
    }

    inline std::string to_str() const {
        const char *lb = "";
        const char *rb = "";
        switch(type) {
            case TERM_EXACT: 
                lb = "'";
                rb = "'";
                break;
            case TERM_REGEX:
                lb = "/";
                rb = "/";
                break;
            case NONTERM_PRODUCTION:
                lb = "";
                rb = "";
                break;
            default:
                lb = "??????";
                rb = "??????";
                break;
        };

        std::string out;
        out += lb;
        out += expr;
        out += rb;

        return out;
    }
};

struct ProdExpr { // or step?
    GrammarElement gexpr;

    int min_times;
    int max_times;

    ProdExpr(const std::string &str, GrammarElement::Type tp)
        : gexpr(str,tp), min_times(1), max_times(1) { }

    inline bool matches(const GrammarElement &other) const {
        return gexpr.compare(other) == 0;
    }

    inline GrammarElement::Type type() const {
        return gexpr.type;
    }

    inline bool is_terminal() const {
        return gexpr.is_terminal();
    }

    inline std::string production_name() const {
        if(gexpr.type == GrammarElement::NONTERM_PRODUCTION) {
            return gexpr.expr;
        } else {
            return "[NOT A PRODUCTION]";
        }
    }

    inline std::string terminal_string() const {
        if(is_terminal()) {
            return gexpr.expr; 
        } else {
            return "[NOT TERMINAL]";
        }
    }

    inline std::string to_str() const {
        std::string out(gexpr.to_str());

        if((min_times != 1) || (max_times != 1)) {
            out += "{";
            out += std::to_string(min_times);
            out += ",";
            if(max_times == INT_MAX) {
                out += "∞";
            } else {
                out += std::to_string(max_times);
            }
            out += "}";
        }

        return out;
    }
};

struct ProductionRule {
    std::string product;
    std::vector<ProdExpr> steps;
    std::string code;
    const utf8_byte *start_of_text;

    ProductionRule(const utf8_byte *at) : start_of_text(at) {
    }

    void add_step(ProdExpr step) {
        steps.push_back(step);
    }

    // return NULL if index is out of bounds
    const ProdExpr *step(unsigned int index) const {
        if(index < steps.size()) {
            return &steps[index];
        }
        return NULL;
    }

    std::string to_str() const {
        std::string out;
        for(auto step : steps) {
            out += step.to_str();
            out += " ";
        }

        if(!out.empty())
            out.pop_back(); // remove trailing space

        return out;
    }

};

class Productions {
    const utf8_byte *source;

    std::vector<ProductionRule>     rules;
    std::multimap<std::string, int> rules_for_product; // product  -> rule ind

    std::vector<const GrammarElement>     elements;
    std::map<const GrammarElement, int>   element_index;

    struct lr_set;
    std::vector<lr_set> states;
    std::map<const std::string, int> state_index; // keyed by set id

    void add_state(const lr_set &st) {
        state_index.insert(
            std::make_pair(st.id(), states.size())
        );
        states.push_back(st);
    }

    void record_element(const GrammarElement &nge) {
        if(element_index.find(nge) == element_index.end()) {
            element_index[nge] = elements.size();
            elements.push_back(nge);
        }
    }

    struct lr_item {
        // Modified from classic lr - in our case,
        // there's no "or" in rules so we can just
        // refer to the rule itself.  So each item
        // can be encoded as a rule ID and the step
        // within that rule:
        uint16_t rule;     // offset into rules
        uint16_t position; // offset into rule->steps

        friend bool operator<(const lr_item& left, const lr_item& right) {
            if(left.rule == right.rule)
                return left.position < right.position;
            return left.rule < right.rule;
        }

        lr_item(int rl, int pos) : rule(rl), position(pos) { }

        std::string to_str(const Productions *prds) const {
            const ProductionRule &rl = prds->rules[rule];

            const int bs = 40;
            char buf[bs];
            snprintf(buf, bs, "%s (rule %i):", rl.product.c_str(), rule);
            std::string out(buf);
            buf[bs - 1] = '\0';

            int step;
            for(step = 0; step < rl.steps.size(); ++step) {
                out += " ";
                if(step == position)
                    out += "•";
                out += rl.steps[step].to_str();
            }
            if(step == position)
                out += "•";

            return out;
        }
    };
    inline const ProdExpr &lr_item_step(const lr_item &it) {
        return rules[it.rule].steps[it.position];
    }

    struct lr_set {
        mutable std::string _id_cache;
        std::set<lr_item> items;

        lr_set() { }

        // 1-item set:
        lr_set(const lr_item &in) { items.insert(in); }

        // The id of the set is a string generated from the
        // content of the item which can be compared to determine
        // if 2 sets are identical or not.
        std::string id() const {
            if(_id_cache.length() == 0) { 
                const int len = items.size()*4 + 1; // 4 digits per item
                char buf[len];
                char *bw = buf;
                for(auto it : items) {
                    snprintf(bw, 5, "%02x%02x", it.rule, it.position);
                    bw += 4;
                }
                buf[len - 1] = '\0';
                _id_cache = std::string(buf);
            }
            return _id_cache;
        }

        void add(const lr_set &set) {
            for(auto it : set.items) {
                items.insert(it);
            }
        }

        std::string to_str(
            const Productions *prds,
            int indent = 0,
            const char *line_prefix = ""
        ) const {
            // efficient? probably not. do I care?
            std::string out;
            for(auto it : items) {
                out.append(line_prefix);
                for(int ind = 0; ind < indent; ind++) {
                    out.append("    ");
                }
                out.append(it.to_str(prds));
                out.append("\n");
            }
            return out;
        }
    };

    void lr_closure_add_rules(lr_set &set, const ProductionRule &rule, int pos) {
        const ProdExpr *right_of_dot = rule.step(pos);

        if(right_of_dot && !right_of_dot->is_terminal()) {
            // The thing to the right of the dot is a product,
            // so we need to add items for each rule that can
            // produce that (per the aho/sethi/ullman pg 222,
            // rule #2, closure procedure).
            std::string pname = right_of_dot->production_name();
            auto strl  = rules_for_product.lower_bound(pname);
            auto endrl = rules_for_product.upper_bound(pname);

            if(strl == endrl) {
                const utf8_byte *sot = rule.start_of_text;
                fail(
                    "Nothing produces «%s» "
                    "(used by rule starting with «%.20s» at line %i\n",
                    pname.c_str(), sot, line_number(source, sot)
                );
            }

            for(auto rit = strl; rit != endrl; ++rit) {
                // (these are always position 0)
                set.items.insert(lr_item(rit->second, 0));
            }
        }
    }

    // based on the closure algorithm on page 222 of Aho, Sethi, Ullman
    // (1988), but with the addition of support for the *+?! operators
    // in fpl.
    lr_set lr_closure(const lr_set &in) {
        lr_set set = in;
        int last_size;
        do {
            last_size = set.items.size();
            // NOTE this could be a lot more efficient if we started
            // at element (last_size - 1)...
            for(auto &item: set.items) {
                // support for *+?!
                //   - the "+" case is no different from the default
                //     here - we'll do the more-than-one case in the
                //     generated code (by not advancing the state)
                //   - the "*" and "?" cases:  since the expression is
                //     optional, we do need to consider what's after
                //     it as another possible start to a given match.
                //   - "!" is complicated and might be unnecessary.
                //     axe it? axing. consider it axed!
                const ProductionRule &rule = rules[item.rule];
                int pos = item.position;
                const ProdExpr *right_of_dot;
                // while loop here handles positions at eof (= NULL)
                // as well helping handle optional expressions
                // (i.e. expressions with "*" and "?"):
                while(right_of_dot = rule.step(pos)) {
                    lr_closure_add_rules(set, rule, pos);

                    // only need to keep looking for following symbols
                    // if the current one is optional. so, if this expr
                    // is not optional, we're done with this item:
                    if(right_of_dot->min_times > 0) {
                        break;
                    }
                    pos++;
                }
            }
        } while(set.items.size() > last_size); // i.e. do until we add no more

        return set;
    }

    // "goto" operation from page 224 Aho, Sethi and Ullman
    lr_set lr_goto(const lr_set &in, const GrammarElement &sym) {
        lr_set set;
        for(auto item : in.items) {
            const ProdExpr *step = rules[item.rule].step(item.position);
            if(step && step->matches(sym)) {
                // XXX need to deal with repetitions/optionals here,
                set.add(lr_item(item.rule, item.position + 1));
            }
        }
        return lr_closure(set);
    }

public:

    Productions(const utf8_byte *src) : source(src) { }

    void push_back(const ProductionRule &rule) {
        int rule_num = rules.size();
        rules.push_back(rule);

        for(const ProdExpr &step : rule.steps) {
            record_element(step.gexpr);
        }

        rules_for_product.insert(std::make_pair(rule.product, rule_num));
    }

    // returns the name of the function to use for the
    // given state
    std::string state_fn(const lr_set &state) {
        std::string fn("state_");
        int state_num = state_index[state.id()];
        fn += std::to_string(state_num);
        return fn;
    }

/*
    // returns the name to use for the public class for the generated parser
    std::string class_name() {
        return base_name();
    }
 */

    /*
       Let's say:

       struct { const utf8_byte *pos, int length, operator bool() } terminal;
       .. length <= 0 is a false value.  Alternately pos = NULL.

       struct { void *product, int grammar_element_id  } production;

       terminal scan_xxx() returns the terminal or one with length <= 0.
       can be inline.  

       state_x() {
           terminal scanned; 
           production prod;

           if(scanned = scan_xxx(...)) {
               prod = state_y();
           } else if(scanned = scan_xxx(...)) {
               prod = state_z();
           } .. etc

           if(prod.grammar_element_id == whatever element id)
           } .. else if next prod.grammar_element_id == ...) {
           } .. etc

           // .. call the production code for the rule here,
           // with ... hmm params

           return prod;
       }
    */
    std::string code_for_terminals(const lr_set &state, const std::string &ind) {
        std::string out;

        int cases_so_far = 0;
        for(auto item : state.items) {
            const ProductionRule &rule = rules[item.rule];
            const ProdExpr *right_of_dot = rule.step(item.position);
            if(right_of_dot) {
                if(right_of_dot->is_terminal()) {
                    out += ind;
                    if(cases_so_far > 0) {
                        out += "else ";
                    }

                    // if we match a terminal, we need to shift and go
                    // to a new state:
                    lr_set next_state = lr_goto(state, right_of_dot->gexpr);
                    out += "if(";
                    switch(right_of_dot->type()) {
                        case GrammarElement::TERM_EXACT:
                            out += "shift_exact(\"";
                            out += right_of_dot->terminal_string();
                            break;
                        case GrammarElement::TERM_REGEX:
                            out += "shift_re(\"";
                            out += c_str_escape(right_of_dot->terminal_string());
                            break;
                        default:
                            fail(
                                "bug: unknown type %i at %i in state %s",
                                right_of_dot->type(), item.position, state.id().c_str()
                            );
                            break;
                    }
                    out += "\")) {\n";
                    out += ind + ind + "// " + item.to_str(this) + "\n";
                    out += ind + ind + state_fn(next_state) + "();\n";
                    out += ind + "}\n";

                    cases_so_far++;
                }
            }
        }

        return out;
    }

    std::string code_for_prods(const lr_set &state, const std::string &ind) {
        std::string out;
        std::map<int, int> element_ids_done;

        for(auto item : state.items) {
            const ProdExpr *right_of_dot = rules[item.rule].step(item.position);
            if(right_of_dot) {
                if(right_of_dot->type() == GrammarElement::NONTERM_PRODUCTION) {
                    int el_id = element_index[right_of_dot->gexpr];

                    auto existing = element_ids_done.find(el_id);
                    if(existing == element_ids_done.end()) { // i.e. no existing code for this
                        lr_set next_state = lr_goto(state, right_of_dot->gexpr);
                        out += ind;
                        if(element_ids_done.size() > 0)
                            out += "else ";
                        out += "if(prd.grammar_element_id == " + std::to_string(el_id) + ") {\n";
                        out += ind + ind + "// " + item.to_str(this) + "\n";
                        out += ind + ind + "prd = " + state_fn(next_state) + "();\n";
                        out += ind + "}\n";
                        element_ids_done[el_id] = state_index[state.id()];
                    } else if(existing->second != state_index[state.id()]) {
                        // gar this error message could be more informative.
                        // we might really want to know what the current item is
                        // conflicting _with_.. hmm
                        fail(
                            "conflicting gotos for %s\n",
                            item.to_str(this).c_str()
                        );
                    }
                }
            }
        }
        return out;
    }

    std::string code_for_state(const lr_set &state) {
        const std::string ind = "    "; // indent string
        std::string out;

        out += "//\n";
        out += state.to_str(this, 1, "//");
        out += "//\n";
        out += "FPLBaseParser::product "; out += state_fn(state); out += "() {\n";
        out += ind + "product prd;\n";

        out += code_for_terminals(state, ind) + "\n";

        out += code_for_prods(state, ind) + "\n";

        // if we're at the end of the item we need to reduce
        // according to the next possible 
        out += "\n    // XXX reduce code here thanks\n";

        out += ind + "return prd;\n";
        out += "}\n"; // end of state_ function

        return out;
    }

    void generate_code(const SourceReader &src) {
        const auto no_set = state_index.end();

        // for now, we're going to consider the first rule
        // (rule 0) to be the starting point (and start with
        // the first expression in that rule, of course):
        add_state(lr_closure(lr_item(0, 0)));

        // Aho, Sethi, and Ullman page 224... uhh, modified.
        int latest_set = 0;
        while(latest_set < states.size()) {
            lr_set set = states[latest_set++];

            for(auto elem : elements) {
                lr_set state = lr_goto(set, elem);
                if(state.items.size() > 0) {
                    if(state_index.find(state.id()) == no_set) {
                        add_state(state);
                    }
                }
            }
        }

        std::string out = "#include \"fpl2cc/fpl_base_parser.h\"\n\n";
        out += "class " + src.base_name() + "_parser : FPLBaseParser {\n";

/*
        // reverse helps with inlining (I hope).
        for(auto it = states.rbegin(); it != states.rend(); ++it) {
            out += code_for_state(*it).c_str();
        }
 */
        for(lr_set state : states) {
            out += code_for_state(state).c_str();
        }

        out += "};\n"; // end of class

        printf("%s\n", out.c_str());
    }
};

void read_quantifiers(SourceReader &src, ProdExpr &expr) {
    const utf8_byte *inp = src.inpp();
    if(!inp) return; // EOF.  this is really an error..

    switch(*inp) {
        case '*':
            expr.min_times = 0;
            expr.max_times = INT_MAX;
            src.skip_bytes(1);
            break;
        case '+':
            expr.min_times = 1;
            expr.max_times = INT_MAX;
            src.skip_bytes(1);
            break;
        case '?':
            expr.min_times = 0;
            expr.max_times = 1;
            src.skip_bytes(1);
            break;
      /*
         don't need this and it's a PITA to implement in LR
         because you have to invalidate prefixes:
        case '!':
            // "!" means don't match this item.
            // it's a suffix because that's
            // easier for me to parse.
            expr.min_times = 0;
            expr.max_times = 0;
            read_pos++;
            break;
       */
        default:
            expr.min_times = 1;
            expr.max_times = 1;
            break;
    }
}

int read_expressions(SourceReader &src, ProductionRule &rule) {
    int num_read = 0;
    bool done = false;
    do {
        src.eat_space();

        const utf8_byte *inp = src.inpp();

        std::string expr_str;
        GrammarElement::Type type = GrammarElement::Type::NONE;

        switch(*inp) {
            case '\0':
                done = true;
                break; // EOF
            case '#':
                src.eat_comment();
                break;
            case '"':
                expr_str = src.read_to_byte('"');
                type     = GrammarElement::Type::TERM_EXACT;
                break;
            case '/':
                expr_str = src.read_to_byte('/');
                type     = GrammarElement::Type::TERM_REGEX;
                break;
            case '-':
                src.read_byte();
                if(src.read_byte_equalling('>')) {
                    // just scanned "->", so we're done:
                    done = true;
                } else {
                    src.error("read unexpected '-'");
                }
                break;
            default:
                // should be the name of a production
                expr_str = src.read_re("[A-Za-z_]+")[0];
                type     = GrammarElement::Type::NONTERM_PRODUCTION;
                break;
        }

        if(type != GrammarElement::Type::NONE) {
            if(expr_str.length() >= 1) {
                ProdExpr expr(expr_str, type);
                read_quantifiers(src, expr);
                rule.add_step(expr);
                num_read++;
            } else {
                // sigh c++ enums
                src.error("expected type %i but got .. nothing?", type);
            }
        }
    } while(!(done || src.eof()));

    return num_read;
}

// returns a string containing the code to "generate" when
// the production ends in a ';'
std::string default_code() {
    return "return arg1;";
}

std::string read_code(SourceReader &src) {
     // code is within "+{" "}+" brackets.
     // this is done simplistically, which means you will
     // derail it if you put +{ or }+ in a comment or
     // string or whatever.  so try not to do that.
     // oh but it needs to be minimal match, not greedy....
     src.eat_space();
     const utf8_byte *start = NULL;
     const utf8_byte *end   = NULL;
     if(src.read_byte_equalling(';')) {
         return default_code();
     } else if(src.read_byte_equalling('+') && src.read_byte_equalling('{')) {
         start = src.inpp();
         while(char byte_in = src.read_byte()) {
             if(byte_in == '}') {
                 if(src.read_byte() == '+') {
                     end = src.inpp() - 2;
                     break;
                 }
             }
         }
     }

     if(start && end) {
         return to_std_string(start, end - start);
     }

     // else error - no start of code or ';'
     src.error("expected start of code (\"+{\") or \";\"");
     return "";
}

/*
 Input is:

   <exprs or productions to match> -> <production name> { <code> }
                    or
   <exprs to match> -> <production name> ;

Also, comments.  Let's use # just cuz.

 */
void fpl2cc(const char *infn) {
    SourceReader inp(infn, fail);

    //Productions productions(inp.buffer.data());
    Productions productions(inp.inpp());
    const char *error;
    while(!inp.eof()) {
        ProductionRule rule(inp.inpp());

        // read the expressions/steps leading to the production
        // (until and including the "->")
        if(read_expressions(inp, rule)) {

            // read what the expressions above produce:
            inp.eat_space();
            const utf8_byte *start = inp.inpp();
            rule.product = inp.read_to_space();
            if(rule.product.length() <= 0) {
                fail(
                    "missing production name on line %i near %.12s\n",
                    inp.line_number(start), start
                );
            }

            // read the code for the production
            rule.code = read_code(inp);

            // add it to the set of productions
            productions.push_back(rule);
        }

        inp.eat_space();
    }

    productions.generate_code(inp);
}

void usage() {
    fprintf(stderr,
        "\nUsage:    fpl2cc <source> [target]\n\n"
        "If no [target] is specified, prints to stdout.\n\n"
    );
}

int main(int argc, const char** argv) {

    if(argc != 2) {
        usage();
        exit(1);
    } else {
        fpl2cc(argv[1]);
    }

    exit(0);
}


