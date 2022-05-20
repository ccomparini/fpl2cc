#ifndef CODE_BLOCK_H
#define CODE_BLOCK_H

namespace fpl {

struct code_block {
    std::string source_file;
    int line;
    std::string code;

    code_block() : line(0) { }

//    static const CodeBlock none; // i.e. no code

    code_block(
        const std::string &cd,
        const std::string &file = CALLER_FILE(),
        int ln = CALLER_LINE()
    ) :
        source_file(file),
        line(ln),
        code(cd) {
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
                ), filename, 1
            );
        }

        using BufIt = std::istreambuf_iterator<char>;
        return code_block(
            std::string(BufIt(in.rdbuf()), BufIt()), filename, 1
        );
    }

    operator bool() const {
        return code.length();
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

    void append(const std::string &str) {
        code += str;
    }

    std::string location() const {
        return stringformat("{}:{}", source_file, line);
    }

    std::string format(bool restore_line = true) const {
        std::string out;
        out += "\n#line " + std::to_string(line) + " \"" + source_file + "\"\n";
        out += code;
        if(restore_line) {
            out += "\n#$LINE\n"; // restore compiler's idea of source file/pos
        }
        return out;
    }

    // As format() but wrapped in { } so that if the code is
    // declares a local variable or such, that variable will
    // be scoped.
    // This will probably only work with code fragments.
    std::string format_scoped() const {
        return "\n{\n" + format() + "\n}\n";
    }

};

}

#endif // CODE_BLOCK_H
