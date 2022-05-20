#ifndef REDUCER_H
#define REDUCER_H

#include <string>
#include <set>

#include "code_block.h"

namespace fpl {

/*
  reducer implements an abstracted reduce function.

  Authors can declare such a function as:
   '+' ~ <reducer name> '(' <arguments> ')' <code block>

  Arguments is a space-separated list of names corresponding
  by name to the steps of the rule which it reduces.  Argument
  order does not matter!  If you have two rules with the same
  product, and one has steps "foo bar bat" and the other has
  "bar foo", both are candidates to match a reducer with arguments
  "bar foo bat".

 */
class reducer {

    std::string production_name;
    std::set<std::string> args;
    code_block code;
public:

    reducer() { }

    reducer(
        const std::string &name,
        const std::set<std::string> arg,
        const code_block &cd
    ) :
        production_name(name),
        args(arg),
        code(cd)
    { }

    operator bool() const {
        return code;
    }

    bool argument_matches(const std::string &var_name) const {
        return args.contains(var_name);
    }

    std::string name() const {
        return production_name;
    }

    std::string format_code() const {
        return code.format(false);
    }

    std::set<std::string> required_arguments() const {
        return args;
    }

    std::string to_str() const {
        std::string out("+");
        out += production_name;
        out += "(";
        int ind = 0;
        for(auto arg : args) {
            if(ind++ == 0)
                out += arg;
            else
                out += " " + arg;
        }
        out += ") at ";
        out += code.location();
        return out;
    }
};
}

#endif // REDUCER_H
