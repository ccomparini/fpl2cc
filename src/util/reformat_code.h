#ifndef REFORMAT_CODE
#define REFORMAT_CODE

// reformats the generated code to fix indents and what have you.
// this is fairly rough but does result in more or less readable
// code for most cases.
// ... this should also be done in fpl.
// fn is the name of the file this will be written to, and is
// used in "restoring" line numbers
static std::string reformat_code(
    const std::string &code, const std::string &fn
) {
    int indent_lev = 0;
    int line_no = 1;
    bool comment_mode = false;

    std::string output;

    // nice to have:
    //   - don't indent "public:", "private:" etc.. maybe anything
    //     which looks like a label?
    //   - trim space in parameter lists
    // the problem is that the way this works now, the label
    // etc will already have been copied by the time we realize
    // it's a label.  maybe a better approach would be to recognize
    // a full line, and only then indent/copy it..

    const std::string::size_type code_length = code.size();
    std::string::size_type inp = 0;
    while(inp < code_length) {

        if(code[inp] == '/' && code[inp + 1] == '*' ) {
            // block comment - copy verbatim:
            output += "/"; output += "*";
            for(inp += 2; inp < code_length; inp++) {
                if(code[inp] == '*' && code[inp + 1] == '/' ) {
                    output += "*"; output += "/";
                    inp += 2;
                    break;
                } else {
                    output += code[inp];
                    if(code[inp] == '\n') {
                        line_no++;
                        // indent comments as well:
                        for(int ind = 0; ind < indent_lev; ind++)
                            output += "    ";
                    }
                }
            }
        } else if(code.compare(inp, 2, "//") == 0) {
            // line comment - also copy verbatim:
            while(code[inp] != '\n') {
                output += code[inp++];
            }
        } else if(code.compare(inp, 7, "#$LINE\n") == 0) {
            inp += 6;
            // this is a pseudo-macro we generated to restore
            // the actual current output line after embeddding
            // some code from another file/place:
            output += "#line " + std::to_string(line_no)
                    + " \"" + fn + "\"";
        } else if(code.compare(inp, 2, "{\n") == 0) {
            // only count '{' at end of line, which is a
            // good-enough hack to avoid issues with
            // quoted { or comments with { or whatever.
            indent_lev++;
            inp++; // (only "eat" the '{' - '\n' is handled next loop)
            output += "{";
        } else if(code[inp] == '\n') {
            // skip spaces at start of line (since we're reindenting)
            while(inp < code_length && isspace(code[inp])) {
                // (.. but preserve newlines)
                if(code[inp++] == '\n') {
                    output += "\n";
                    line_no++;
                }
            }

            if(code[inp] == '}') {
                // this is in here as a counterpart to the
                // "only count '{' at end of line thing:
                // only count '}' if it's the start of a
                // new line (excluding spaces):
                indent_lev--;
            }
            // now do the indent:
            for(int ind = 0; ind < indent_lev; ind++)
                output += "    ";
        } else {
            output += code[inp++];
        }
    }
    return output;
}

#endif // REFORMAT_CODE

