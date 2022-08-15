#ifndef PRODUCTION_RULE_H
#define PRODUCTION_RULE_H

#include "code_block.h"
#include "grammar_element.h"
#include "reducer.h"

#include "util/c_str_escape.h"
#include "util/jerror.h"
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


        inline bool is_single() const {
            return((min_times == 1) && (max_times == 1));
        }

        inline bool matches(const grammar_element &other) const {
            return gexpr.compare(other) == 0;
        }

        inline bool is_named() const {
            return variable_name().length() > 0;
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
    std::vector<step>     rsteps;
    std::vector<int>      psteps;        // index of step for parameter number
    std::set<std::string> step_vars;     // for finding conflicts
    code_block            code_for_rule; // inlined reduce code, if any
    reducer               abs_impl;      // abstracted implementation, if any
    std::string           file;
    int                   line;
    int                   rulenum;

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

    void add_step(step st) {
        int stepi = rsteps.size();

        // if this step will be passed to the reduce function....
        if(!st.skip_on_reduce()) {
            // ... then we need a name for the variable representing it.
            // if the step already knows what its variable name should be,
            // we just use that, but otherwise assign it a name based on
            // it's index within the set of steps for this rule:
            if(!st.variable_name().length()) {
                st.varname = "arg_" + std::to_string(stepi);
            }

            // .. and now make sure that that name doesn't collide with
            // the name of another step:
            const std::string name = st.variable_name();
            if(step_vars.contains(name)) {
                jerror::warning(stringformat(
                    "duplicate name '{}' in step {} {}",
                    name, stepi, location()
                ));
            }
            step_vars.insert(name);

            // and, finally, register the index of the step for the
            // nth parameter:
            psteps.push_back(stepi);
        }

        rsteps.push_back(st);
    }

    inline int num_steps() const { return rsteps.size(); }

    // the result of a given step may or may not be passed
    // to the reduction code.  this tells how many are.
    inline int num_reduce_params() const {
        return psteps.size();
    }

    inline const std::set<std::string> reduce_params() const {
        return step_vars;
    }

    inline int reduce_param_step_num(
        unsigned int pind, src_location ca = CALLER()
    ) const {
        if(pind >= psteps.size()) {
            jerror::error(stringformat(
                "index {} out of bounds in {}", pind, ca
            ));
            pind = 0; // so that there's some chance we don't go out of bounds
        }
        return psteps[pind];
    }

    // ..and this is how you can get the step for a given parameter
    // (by parameter position)
    inline const step &reduce_param(
        unsigned int index, src_location ca = CALLER()
    ) const {
        return rsteps[reduce_param_step_num(index, ca)];
    }

    // returns NULL if index is out of bounds
    const step *nth_step(unsigned int index) const {
        if(index < rsteps.size()) {
            return &rsteps[index];
        }
        return nullptr;
    }

    // if this rule could potentially be a type alias,
    // returns the name of the product to which it could
    // be aliased.
    // otherwise returns an empty string.
    std::string potential_type_alias() const {

        // the rule is a potential alias if it has
        // exactly one reduce parameter.... 
        if(num_reduce_params() == 1) {
            const step &rp = reduce_param(0);
            // and it's not optional or multiple.
            // multiples need to be represented with a different
            // type (eg array of x instead of x), and optionals
            // would only work if there's a way to guarantee a
            // default or something is created in the absence
            // of anything, which, at present, there is not.
            // ACTUALLY optional might work!  try it.
            //  x? -> foo ;  if the reducer is actually called, a custom
            // reducer could fill in the default for the x.
            // huh what if you had
            //  x? -> x ;
            // ... for filling in defaults.  the mind reels. try it.
            if(rp.is_single() && !rp.is_optional()) {
                // for now it has to be a nonterminal too:
                if(!rp.is_terminal()) {
                    return rp.production_name();
                }
            }
        }
        return "";
    }

    const std::vector<step> &steps() const {
        return rsteps;
    }

    bool needs_reducer() const {
        if(reduce_code())
            return false; // don't need - we have one

        for(int stepi : psteps) {
            step st = rsteps[stepi];

            // if the step is either multiple or optional,
            // someone needs to write a reducer to tell
            // us how to handle it (for now, anyway):
            if(!st.is_single())
                return true;
        }

        return false;
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

    void code(const code_block &cd) {
        code_for_rule = cd;
    }

    code_block code() const {
        return code_for_rule;
    }

    reducer abstracted_reducer() const {
        return abs_impl;
    }

    // returns a code_block representing any reducer specified
    // in the fpl (eg inline or from an abstracted reducer),
    // or a false code block if no such thing was specified.
    code_block reduce_code() const {

        // abstracted reducers take priority over inline code.
        // this is so that fpl authors can import grammar
        // from whatever they want, and override actions
        // as necessary for their particular applications.
        reducer red = abstracted_reducer();
        if(red)
            return red.code();
        else if(code_for_rule)
            return code_for_rule;

        // no specific reduce code for this rule:
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

