#ifndef PRODUCTION_RULE_H
#define PRODUCTION_RULE_H

#include "code_block.h"
#include "grammar_element.h"
#include "reducer.h"

#include "util/c_str_escape.h"
#include "util/stringformat.h"

#include <string>
#include <vector>

namespace fpl {

class production_rule {
public:
    struct step {
        grammar_element gexpr;
        std::string varname; // if set, name of this expression in reduce code

        int min_times;
        int max_times;

        bool eject; // if set, don't pass this to reduce code

        step(const std::string &str, grammar_element::Type tp)
            : gexpr(str,tp), min_times(1), max_times(1),
              eject(false)
        { }

        friend bool operator<(const step& left, const step& right) {
            if(left.gexpr.compare(right.gexpr) == 0) {
                if(left.min_times == right.min_times)
                    return left.max_times < right.max_times;
                else
                    return left.min_times < right.min_times;
            }
            return left.gexpr < right.gexpr;
        }

        inline bool is_single() const {
            return((min_times == 1) && (max_times == 1));
        }

        inline bool matches(const grammar_element &other) const {
            return gexpr.compare(other) == 0;
        }

        inline std::string variable_name() const {
            if(varname.length()) {
                return varname;
            } else if(gexpr.type == grammar_element::NONTERM_PRODUCTION) {
                // if it's a production, default the variable name to
                // the name of that production, so that you don't have
                // to name everything explicitly but can still do
                // the abtracted implementation match thing:
                return gexpr.expr;
            }
            return "";
        }

        inline grammar_element::Type type() const {
            return gexpr.type;
        }

        inline bool is_terminal() const {
            return gexpr.is_terminal();
        }

        inline bool is_optional() const {
            return min_times == 0;
        }

    // XXX this is actually going to mean don't put on the stack in the first place
        inline bool skip_on_reduce() const {
            return eject;
        }

        inline std::string production_name() const {
            if(gexpr.type == grammar_element::NONTERM_PRODUCTION) {
                return gexpr.expr;
            } else {
                return "[NOT A PRODUCTION]";
            }
        }

        // returns a c-source-code-ready string for the terminal
        inline std::string terminal_string() const {
            if(is_terminal()) {
                return c_str_escape(gexpr.expr);
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

            if(varname.length())
                out += ":" + varname;

            if(eject)
                out += "^";

            return out;
        }
    };

private:
    std::string prod;
    std::vector<step> rsteps;
    code_block   code_for_rule; // inlined reduce code, if any
    reducer      abs_impl;      // abstracted implementation, if any
    std::string file;
    int         line;
    int         rulenum;

public:

    production_rule(const std::string fn, int ln) :
        file(fn),
        line(ln),
        rulenum(-1)
    {
    }

    void set_rulenum(int num) {
        rulenum = num;
    }

    std::string rule_fn() const {
        return stringformat("rule_{}", rulenum);
    }

    // makes a better rule_meta method; probably a better
    // name in general, but I'm keeping rule_fn for now
    // because it's more grepable.
    std::string name() const {
        return rule_fn();
    }

    void add_step(step step) {
        rsteps.push_back(step);
    }

    inline int num_steps() const { return rsteps.size(); }

    // the result of a given step may or may not be passed
    // to the reduction code.  this tells how many are.
    inline int num_reduce_params() const {
        int num = 0;
        for(auto st : rsteps) {
            if(!st.skip_on_reduce()) num++;
        }
        return num;
    }

    // returns NULL if index is out of bounds
    const step *nth_step(unsigned int index) const {
        if(index < rsteps.size()) {
            return &rsteps[index];
        }
        return nullptr;
    }

    bool foldable() const {

        // the rule is "foldable" if it has exactly one step
        // and that step is not optional, repeated, or ejected.
        if(num_steps() != 1) return false;
        const step *st = nth_step(0);
        return (st->min_times == 1) && (st->max_times == 1) && (!st->eject);
    }

    const std::vector<step> &steps() const {
        return rsteps;
    }

    const std::string &product(const std::string &pr) {
        prod = pr;
        return prod;
    }

    const std::string &product() const {
        return prod;
    }

    grammar_element product_element() const {
        return grammar_element(product(), grammar_element::NONTERM_PRODUCTION);
    }

    int line_number() const {
        return line;
    }

    std::string filename() const {
        return file;
    }

    std::string location() const {
        return filename() + ":" + std::to_string(line_number());
    }

    // returns the variable name for the given step number.
    // rudely asserts if the step isn't valid
    std::string varname(int stepi) const {
        if(const step *st = nth_step(stepi)) {
            std::string name = st->variable_name();
            if(name.length() == 0) {
                name = "arg_" + std::to_string(stepi);
            }
            return name;
        }

        std::string error = stringformat(
            "BUG: invalid step number {} in rule {}\n", stepi, to_str()
        ).c_str();

        assert(error.c_str());
        return error;
    }

    code_block default_code() const {
        // XXX  bug:
        // the following should work, but does not (apparently because of
        // the optionalness on leading_ws).  It should work because there's
        // exactly one argument - the terminal '@-')
        //  leading_ws?^ '@-' -> subst_start ;
        // actually the bug is at the caller of this, or in foldable().
        if(!foldable()) {
            // if it's not foldable, at this point we're
            // considering the code to be too complex
            // to come up with a default.  So return a
            // false block:
            return code_block();
        }

        // start the code block with a comment referring to this
        // line in this source file (fpl2cc.cc), to reduce puzzlement
        // about where this mixed-generated code comes from:
        std::string code("// " THIS_LINE "\n");

        // the default now is to just return the first parameter:
        code += "return " + varname(0) + ";\n";

        return code;
    }

    void code(const code_block &cd) {
        code_for_rule = cd;
    }

    code_block code() const {
        return code_for_rule;
    }

    reducer abstracted_reducer() const {
        return abs_impl;
    }

    // this is the _actual_ reduction code, after factoring
    // in abstracted reducers and defaults and whatnot.
    code_block reduce_code() const {

        // abstracted reducers take priority over inline code.
        // this is so that fpl authors can import grammar
        // from whatever they want, and override actions
        // as necessary for their particular applications
        reducer red = abstracted_reducer();
        if(red)
            return red.code();
        else if(code_for_rule)
            return code_for_rule;
        else if(foldable())
            return default_code();

        // if none of the above apply, return a false
        // code block.  callers can print an error or
        // fill something in or whatever they think is
        // appropriate.
        return code_block();
    }

    void set_reducer(const reducer &red) {
        abs_impl = red;
    }

    const char *product_c_str() const {
        return product().c_str();
    }

    std::string to_str() const {
        std::string out;
        for(auto step : rsteps) {
            out += step.to_str();
            out += " ";
        }

        out += "-> " + product();

        return out;
    }
};

}

#endif // PRODUCTION_RULE_H

