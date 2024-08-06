#ifndef CODE_BLOCK_H
#define CODE_BLOCK_H

#include <fstream>
#include <set>
#include <string>
#include "util/jerror.h"
#include "util/searchpath.h"
#include "util/src_location.h"
#include "util/stringformat.h"


namespace fpl {

struct code_block {
    enum source_language {
        UNKNOWN = 0,
        DEFAULT, // effectively c++ at the moment
        REGEX,
        EXACT_MATCH, // like regex, but exact match
    } language;

    std::string source_file;
    int line;
    std::string code;

    // If this is set, it indeicates that the code block
    // is just a stub, and fpl authors are expected to
    // implement something real.
    // in the fpl grammar.
    // An example of where this might be used is if you
    // @import ansi-c;  in the ansi c fpl there are stubs
    // for enumeration constant and typedefed type tokens,
    // but in order for those to be recognized properly,
    // there needs to be some kind of symbol table lookup.
    // (this is just how c is)
    bool is_stub;

    code_block() : language(UNKNOWN), line(0), is_stub(false) { }

    code_block(
        const std::string &cd,
        source_language lang = DEFAULT,
        const std::string &file = CALLER_FILE(),
        int ln = CALLER_LINE()
    ) :
        language(lang),
        source_file(file),
        line(ln),
        code(cd),
        is_stub(false) {
    }

    /*
      constructs and returns a code block whose contents are that of the file
      specified by "filename".
      on error, the code_block returned will contain a #error directive
      describing what went wrong.
     */
    static code_block from_file(
        const std::string &in_filename,
        const Searchpath &search,
        const std::string &context_fn, int context_ln
    ) {
        std::string filename = search?search.find(in_filename):in_filename;
        std::ifstream in(filename);
        if(!in.is_open()) {
            return code_block(
                stringformat(
                    "#line {} \"{}\"\n"
                    "#error \"unable to open '{}' for reading: {}\n",
                    context_ln, context_fn,
                    filename, std::string(strerror(errno))
                ),
                DEFAULT, filename, 1
            );
        }

        using BufIt = std::istreambuf_iterator<char>;
        return code_block(
            std::string(BufIt(in.rdbuf()), BufIt()),
            DEFAULT, filename, 1
        );
    }

    operator bool() const {
        return code.length();
    }

    // we need  == at all because otherwise c++ will use the bool
    // cast for comparisons :P
    // For our purposes, ideally equivalence would be code which
    // executes identically.  however, for now/practical purposes
    // we'll go with language/string equivalence.
    bool operator==(const code_block &other) const {
         return this->language == other.language
             && this->code     == other.code;
    }

    bool operator!=(const code_block &other) const {
        return !(*this == other);
    }

    // for backward compatibility so I can just swap code_block
    // in where I used to use std::string
    code_block &operator += (const std::string &more_code) {
        code += more_code;
        return *this;
    }

    code_block &operator += (const code_block &more_code) {
        return *this += more_code.format();
    }

    void stub(
        const std::string &stub_code,
        source_language lang = DEFAULT
    ) {
        is_stub  = true;
        code     = stub_code;
        language = lang;
    }

    void append(const std::string &str) {
        code += str;
    }

    std::string location() const {
        return stringformat("{}:{}", source_file, line);
    }

    std::string source_filename() const { return source_file; }
    int         line_number()     const { return line; }

    std::string format(bool restore_line = true) const {
        std::string out;
        out += stringformat("\n#line {} \"{}\"\n", line, source_file);
        out += code;
        if(restore_line) {
            out += "\n#$LINE\n"; // restore compiler's idea of source file/pos
        }
        return out;
    }

    std::string to_str() const {
        return format(false);
    }

private:
    static bool is_production_name_char(const char ch) {
        return (ch == '_')              ||
               (ch >= 'A' && ch <= 'Z') ||
               (ch >= 'a' && ch <= 'z') ||
               (ch >= '0' && ch <= '9');
    }

    static bool maybe_name_start(const std::string &str, size_t pos) {
        // start of string definitely could be the start of a name
        if(pos == 0) return true;
        
        char ch_before = str[pos - 1];
        if(is_production_name_char(ch_before)) {
             // apparently we're within the name of another variable - 
             // eg at "bar" within  "foobar":
            return false;
        } else {
            // the character before the one we're looking at isn't
            // part of a normal variable name, but it might be something
            // indicating a member of something else (eg foo.bar or foo->bar)
            if(ch_before == '.') return false; // foo.bar

            if(ch_before == '>') {
                // check for ->bar vs possible 2>bar
                return pos < 2 || str[pos - 2] != '-';
            }
        }
        return true;
    }

public:

    //
    // For each argument name in the set passed, mangle the code
    // text as follows:
    //  <argname>\[([0-9+])\] -> <argname>.val($1)
    //  <argname>[^@] -> <argname>.val()
    //  <argname>@ -> argname.
    // 
    // This means that the stack slice looks like an array of whatever
    // type is expected for that variable, but it's a magic array
    // where the name itself resolves to the first element (so that
    // simple things like wcalc can just deal in the arg names),
    // but, if you want metadata about the argument you can access it
    // via the "@" pseudo operator.
    //
    // Perhaps obviously, this only dtrt for code blocks for rule
    // actions. It's a bit of an ugly hack.  The right thing would
    // be not to have to write rule actions in c++, but that's a
    // whole other can of worms so I'm doing this for now.
    //
    void mangle_stack_slice_args(const std::set<std::string> args) {
        for(auto arg : args) {
            const size_t argl = arg.length();
            size_t pos = 0;
            while((pos = code.find(arg, pos)) != std::string::npos) {
                // we found something matching the name of the arg, but
                // make sure it matches the _start_ of the arg:
                size_t endp = pos + argl;
                if(maybe_name_start(code, pos)) {
                    // now check what comes right after:
                    if(code[endp] == '@') {
                        // author wants metadata: just change the '@' to '.',
                        // and we'll expect it to result in a call to
                        // whatever the method in the stack slice is:
                        code[endp] = '.';
                    } else if(code[endp] == '[') {
                        size_t end_brace = endp + 1;
                        while(code[end_brace] && code[end_brace] != ']')
                            end_brace++;
                        if(code[end_brace] != ']') {
                            jerror::error(stringformat(
                                "no end brace found on {}\n", arg
                            ));
                        }
                        size_t subl = end_brace - endp - 1;
                        std::string subs = stringformat(
                            ".val({})", code.substr(endp + 1, subl)
                        );
                        code.replace(endp, end_brace - endp + 1, subs);
                        endp += subs.length();
                    } else if(!is_production_name_char(code[endp])) {
                        // plain production name - default to 0th element:
                        std::string subs = ".val()";
                        code.insert(endp, subs);
                        endp += subs.length();
                    } // else it's not something to expand
                }
                pos = endp;
            }
        }
    }
};

}

#endif // CODE_BLOCK_H
