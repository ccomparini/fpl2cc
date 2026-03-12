#ifndef CODE_BLOCK_H
#define CODE_BLOCK_H

#include <fstream>
#include <set>
#include <string>
#include <string.h> // for strerr
#include <string_view>
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

    // If this is set, it indicates that the code block
    // is just a stub, and fpl authors are expected to
    // implement something real.
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
        if(*this) {
            return stringformat("{}:{}", source_file, line);
        } else {
            return stringformat("(no code)");
        }
    }

    std::string source_filename() const { return source_file; }
    int         line_number()     const { return line; }

    // formats the code with the assumption that it's c/c++.
    // phase this out in favor of jemp/templates etc.
    std::string format(bool restore_line = true, std::string_view arg_container="") const {
        std::string out;
        out += stringformat("\n#line {} \"{}\"\n", line, source_file);
        if(arg_container.length())
            out += substitute_at_syntax(code, arg_container);
        else
            out += code;
        if(restore_line) {
            out += "\n#$LINE\n"; // restore compiler's idea of source file/pos
        }
        return out;
    }

    std::string to_str() const {
        return format(false);
    }

    //
    // Returns a copy of the source string with the substitutions:
    //  <argname>@<member name> -> arg_container.<argname>.<member name>
    //  @@ -> @
    // (the latter of which is so you can escape an '@')
    //
    // This allows fpl authors to access methods like .count(), .position(),
    // etc in the ReductionParameters.
    //
    static std::string substitute_at_syntax(
        std::string_view source,
        std::string_view containerName)
    {
        std::string result;
        result.reserve(source.size());

        size_t pos = 0;
        while (pos < source.size()) {
            size_t atPos = source.find('@', pos);
            if (atPos == std::string_view::npos) {
                result.append(source.substr(pos));
                break;
            }

            // handle @@ escape
            if (atPos + 1 < source.size() && source[atPos + 1] == '@') {
                result.append(source.substr(pos, atPos - pos));
                result.append("@");
                pos = atPos + 2;
                continue;
            }

            // scan backwards to find a valid c++ identifier before @
            size_t nameEnd = atPos;
            size_t nameStart = nameEnd;
            while (nameStart > pos && (std::isalnum(source[nameStart-1]) || source[nameStart-1] == '_'))
                --nameStart;

            // if no identifier before @, leave it alone
            if (nameStart == nameEnd) {
                result.append(source.substr(pos, atPos - pos + 1));
                pos = atPos + 1;
                continue;
            }

            std::string_view argName = source.substr(nameStart, nameEnd - nameStart);

            // append everything before the arg name, then the substitution
            result.append(source.substr(pos, nameStart - pos));
            result.append(containerName);
            result.append(".");
            result.append(argName);
            result.append(".");

            pos = atPos + 1;
        }

        return result;
    }

};

std::string to_string(const code_block::source_language &l) {
    switch(l) {
        case code_block::UNKNOWN:
            return "UNKNOWN";
        case code_block::DEFAULT:
            return "DEFAULT";
        case code_block::REGEX:
            return "REGEX";
        case code_block::EXACT_MATCH:
            return "EXACT_MATCH";
    }
    return stringformat("Invalid source language id 0x{:x}", int(l));
}

}

#endif // CODE_BLOCK_H
