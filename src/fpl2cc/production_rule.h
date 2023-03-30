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
        grammar_element::Type tp = grammar_element::NONTERM_PRODUCTION,
        std::string product = "" // (current parsing reads the prod later)
    ) :
        prod(product),
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

        bool eject;  // if set, don't pass this to reduce code
        bool invert; // invert match (!"foo" = match anything but "foo")

        static step &false_step() {
            static step fs;
            return fs;
        }

        step() :
            gexpr("", grammar_element::Type::NONE),
            eject(true),
            invert(false) {
        }

        step(
            const std::string &expr_str,
            grammar_element::Type tp,
            const std::string vn = ""
        ) :
            gexpr(expr_str,tp),
            varname(vn),
            eject(false),
            invert(false) {
        }

        operator bool() const {
            return gexpr.type != grammar_element::Type::NONE;
        }

        bool is_single() const {
            return !(qty.optional || qty.multiple);
        }

        bool is_optional() const {
            return qty.optional;
        }

        bool is_multiple() const {
            return qty.multiple;
        }

        bool matches(const grammar_element &other) const {
            return gexpr.compare(other) == 0;
        }

        bool is_named() const {
            return variable_name().length() > 0;
        }

        std::string variable_name() const {
            if(varname.length()) {
                return varname;
            } else if(
                gexpr.type == grammar_element::NONTERM_PRODUCTION ||
                gexpr.type == grammar_element::TERM_CUSTOM
             ) {
                // if it's a production or custom terminal, we can use that
                // name as the variable name so that authors don't have to
                // name everything explicitly but can still do the abstracted
                // implementation match thing:
                return gexpr.expr;
            }
            // else caller will need to fill in something contextually
            // appropriate:
            return "";
        }

        // returns the name of the custom scanner used by this
        // step, or empty string if there's no such thing:
        std::string custom_scanner_name() const {
            if(gexpr.type == grammar_element::TERM_CUSTOM) {
                return gexpr.expr;
            }
            return "";
        }

        grammar_element::Type type() const {
            return gexpr.type;
        }

        bool is_nonterminal() const {
            return gexpr.is_nonterminal();
        }

        bool skip_on_reduce() const {
            return eject;
        }

        std::string production_name() const {
            if(is_nonterminal()) {
                return gexpr.expr;
            } else {
                return "[NOT A PRODUCTION]";
            }
        }

        std::string to_str() const {
            std::string out;

            if(invert) {
                // if you see this, inverse has not been resolved yet:
                out += "¡";
            }

            out += gexpr.to_str();

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

    void check_duplicate_varname(const std::string name, int step_num) const {
        if(step_vars.contains(name)) {
            jerror::warning(stringformat(
                "duplicate name '{}' in step {} {}",
                name, step_num, location()
            ));
        }
    }

    void add_step(step st) {
        int stepi = rsteps.size();

        // if this step will be passed to the reduce function....
        if(!st.skip_on_reduce()) {
            std::string name = st.variable_name();

            // .. then step will need a variable name.  If no name is
            // assigned, assign it a name based on its index within
            // the set of steps for this rule:
            if(!name.length()) {
                name       = stringformat("arg_{}", stepi);
                st.varname = name;
            }

            // If the previous parameter has the same name, we "meld" them
            // into one parameter by not adding another pstep.  This allows
            // authors to write (for example):
            //    '('^ (arg (','^ arg)*)? ')'^ -> argument_list;
            // and then:
            //    +argument_list(arg) +{ arg@foreach ... }+
            //
            // This will only work if the reduce types are the same,
            // of course, but it covers useful cases.
            std::string prior_pname;
            if(auto prior_step = reduce_param(psteps.size() - 1)) {
                prior_pname = prior_step.variable_name();
            }

            // (prior_pname will be the empty string if no prior param)
            if(prior_pname != name) {
                // No melding fr this one:  go ahead and add it
                check_duplicate_varname(name, stepi);
                step_vars.insert(name);
                psteps.push_back(stepi);
            } // else it's melded with the prior parameter
        }

        rsteps.push_back(st);
    }

    void resolve_placeholder(
        int stepi, const std::string prod, src_location caller = CALLER()
    ) {
        if(stepi < 0 || stepi >= rsteps.size()) {
            jerror::warning(stringformat(
                "step {} is out of range at {}",
                stepi, caller
            ));
        } else {
            grammar_element &ge = rsteps[stepi].gexpr;
            ge.resolve_placeholder(prod, caller);
        }
    }

    // A "custom" terminal implemented as a regular expression is
    // (in the target parser) equivalent to a normal regex terminal.
    // So, if a given step is (or resolves to) a regex TERM_CUSTOM,
    // we convert it to a regular TERM_REGEX, thus saving some
    // special cases down the line.
    void resolve_regex_custom(int stepi, const std::string regex) {
        if(stepi < 0 || stepi >= rsteps.size()) {
            jerror::warning(stringformat(
                "step {} is out of range in resolve_regex_custom",
                stepi
            ));
        } else {
            grammar_element &ge = rsteps[stepi].gexpr;
            if(ge.type == grammar_element::TERM_CUSTOM) {
                // set the variable name to the name of the
                // custom scanner, so that reducers can
                // refer to it by that:
                if(!rsteps[stepi].varname.length()) {
                    rsteps[stepi].varname = ge.expr;
                }

                // convert to a regex expression:
                ge.expr = regex;
                ge.type = grammar_element::TERM_REGEX;
            } else {
                jerror::warning(stringformat(
                    "step {} is {} and not a custom terminal",
                    stepi, ge
                ));
            }
        }
    }

    // Steps are _actually_ inverted by changing the grammar element
    // type to the inverse type.  This is because, in the target parser,
    // inverse matches are generally completely different code.
    void resolve_inverts() {
        for(int stepi = 0; stepi < rsteps.size(); ++stepi) {
            step &st = rsteps[stepi];
            if(st.invert) {
                st.gexpr.invert_type();
                st.invert = false;
            }
        }
    }

    int num_steps() const { return rsteps.size(); }

    // the result of a given step may or may not be passed to the
    // reduction code (for example, "ejected" parameters are not
    // passed).  this tells how many are actually passed.
    int num_reduce_params() const {
        return psteps.size();
    }

    const std::set<std::string> reduce_params() const {
        return step_vars;
    }

    unsigned reduce_param_step_num(
        unsigned int pind, src_location ca = CALLER()
    ) const {
        if(pind >= psteps.size()) {
            return -1;
        }
        return psteps[pind];
    }

    // ..and this is how you can get the step for a given parameter
    // (by parameter position)
    const step &reduce_param(
        unsigned int index, src_location ca = CALLER()
    ) const {
        unsigned stepi = reduce_param_step_num(index, ca);
        if(stepi < rsteps.size())
            return rsteps[reduce_param_step_num(index, ca)];
        return step::false_step();
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

    // if this rule has exactly one reduce parameter,
    // return a reference to the step for that parameter.
    // otherwise, return a false step.
    const step &single_param() const {
        if(num_reduce_params() == 1) {
            return reduce_param(0);
        }
        return step::false_step();
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

    bool is_potential_type_alias() const {
        return potential_type_alias() != "";
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

