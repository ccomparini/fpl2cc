#ifndef PRODUCTION_RULE_H
#define PRODUCTION_RULE_H

#include "code_block.h"
#include "grammar_element.h"
#include "reducer.h"

#include "util/c_str_escape.h"
#include "util/jerror.h"
#include "util/src_location.h"
#include "util/stringformat.h"

#include <string>
#include <vector>

namespace fpl {

class production_rule {
    public: struct step; private:

    std::string prod;
    std::vector<step>     rsteps;
    std::vector<int>      psteps;        // index of step for parameter number
    std::set<std::string> step_vars;     // for finding conflicts
    code_block            code_for_rule; // reduce code, if known yet
    reducer               abs_impl;      // abstracted implementation, if any
    std::string           file;
    int                   line;
    grammar_element::Type prod_type;      // production, subrule, or none
    int                   rulenum;        // assigned when added to productions
    int                   parent_rulenum; // (or -1 if not subrule)
    int                   parent_cpos;    // step relative to end of parent

public:

    production_rule(
        const std::string fn, int ln,
        grammar_element::Type tp = grammar_element::NONTERM_PRODUCTION
    ) :
        file(fn),
        line(ln),
        prod_type(tp),
        rulenum(-1),
        parent_rulenum(-1),
        parent_cpos(0) {
    }

    production_rule() :
        line(0),
        prod_type(grammar_element::NONE),
        rulenum(-1),
        parent_rulenum(-1),
        parent_cpos(0) {
    }

    operator bool() const {
        return prod_type != grammar_element::Type::NONE;
    }
    
    void set_rule_number(int num) {
        rulenum = num;
    }

    int rule_number() const {
        return rulenum;
    }

    void set_parent(int rulenum, int ctd) {
        parent_rulenum = rulenum;
        parent_cpos = ctd;
    }

    int parent_rule_number() const {
        return parent_rulenum;
    }

    int parent_countdown_pos() const {
        return parent_cpos;
    }

    std::string rule_fn() const {
        return stringformat("rule_{}", rulenum);
    }

    struct step {
        grammar_element gexpr;
        std::string varname; // if set, name of this expression in reduce code

        struct quantifier {
            bool optional;
            bool multiple;
            quantifier() : optional(false), multiple(false) { }
        } qty;

        bool eject; // if set, don't pass this to reduce code

        step() : gexpr("", grammar_element::Type::NONE), eject(true) {
        }

        step(
            const std::string &expr_str,
            grammar_element::Type tp,
            const std::string vn = ""
        ) : gexpr(expr_str,tp), eject(false), varname(vn) {
        }

        operator bool() const {
            return gexpr.type != grammar_element::Type::NONE;
        }

        inline bool is_single() const {
            return !(qty.optional || qty.multiple);
        }

        inline bool is_optional() const {
            return qty.optional;
        }

        inline bool is_multiple() const {
            return qty.multiple;
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
            // else caller will need to fill in something contextually
            // appropriate:
            return "";
        }

        inline grammar_element::Type type() const {
            return gexpr.type;
        }

        inline bool is_nonterminal() const {
            return gexpr.is_nonterminal();
        }

        inline bool skip_on_reduce() const {
            return eject;
        }

        inline std::string production_name() const {
            if(is_nonterminal()) {
                return gexpr.expr;
            } else {
                return "[NOT A PRODUCTION]";
            }
        }

        inline std::string to_str() const {
            std::string out(gexpr.to_str());

            if(qty.optional && qty.multiple) {
                out += "*";
            } else if(qty.optional) {
                out += "?";
            } else if(qty.multiple) {
                out += "+";
            }

            if(varname.length())
                out += ":" + varname;

            if(eject)
                out += "^";

            return out;
        }
    };

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

    void resolve_step(
        int stepi, const std::string prod, src_location caller = CALLER()
    ) {
        if(stepi < 0 || stepi >= rsteps.size()) {
            jerror::warning(stringformat(
                "step {} is out of range at {}",
                stepi, caller
            ));
        } else {
            grammar_element &ge = rsteps[stepi].gexpr;
            ge.resolve_to(prod, caller);
        }
    }

    inline int num_steps() const { return rsteps.size(); }

    // the result of a given step may or may not be passed to the
    // reduction code (for example, "ejected" parameters are not
    // passed).  this tells how many are actually passed.
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

    // returns a false step if index is out of bounds
    step nth_step(unsigned int index) const {
        if(index < rsteps.size()) {
            return rsteps[index];
        }
        return step();
    }

    step nth_from_end(unsigned int index) const {
        return nth_step(rsteps.size() - index);
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
            if(rp.is_single() && !rp.is_optional()) {
                // for now it has to be a nonterminal too:
                if(rp.is_nonterminal()) {
                    return rp.production_name();
                }
            }
        }
        return "";
    }

    const std::vector<step> &steps() const {
        return rsteps;
    }

    const std::string &product(const std::string &pr) {
        prod = pr;
        return prod;
    }

    // Returns the name of the product for purposes of state generation.
    // Subexpressions get a unique "dummy" product.  Normal rules will
    // have the name of the thing the rule produces.
    const std::string &product() const {
        return prod;
    }

    // Returns a grammar element representing the result of matching
    // this rule.  Normally, this will simply be the grammar
    // element for the rule's product.
    grammar_element product_element() const {
        return grammar_element(product(), prod_type);
    }

    bool is_subexpression() const {
        return prod_type == grammar_element::Type::NONTERM_SUBEXPRESSION;
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
        code_block out;

        // abstracted reducers take priority over inline code.
        // this is so that fpl authors can import grammar
        // from whatever they want, and override actions
        // as necessary for their particular applications.
        reducer red = abstracted_reducer();
        if(red)
            out = red.code();
        else if(code_for_rule)
            out = code_for_rule;
        // else return a false code block

        out.mangle_stack_slice_args(reduce_params());
        return out;
    }

    void set_reducer(const reducer &red) {
        abs_impl = red;
    }

    std::string to_str() const {
        std::string out;
        for(auto step : rsteps) {
            out += step.to_str();
            out += " ";
        }

        out += "-> ";
        if(parent_rulenum >= 0) {
            out += stringformat("rule {}:{}", parent_rulenum, parent_cpos);
        } else {
            out += product();
        }

        return out;
    }
};

}

#endif // PRODUCTION_RULE_H

