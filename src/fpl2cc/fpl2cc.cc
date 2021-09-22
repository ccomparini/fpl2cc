#include <climits>
#include <list>
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

#include "fpl_reader.h"


void fail(const char *fmt...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    exit(1);
}

static int num_warnings = 0;
void warn(const char *fmt...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);

    num_warnings++;
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

  If there's any repetition,... XXX handle repetition,
  possibly in the state code itself.

  Code blocks should use a normal "return" statement to return a pointer
  to a Jest::Member.

 */

/*
 TODO
  - sort out termination.
  - sort out specifying entry rules:
    - allow specifying within the fpl
    x fix the implementation - need to first generate a set
      for all the possible entries
  - make it so that if you forget the "return" in a rule, it at
    least returns something. Maybe have the generated code print
    a warning, too?
  - detect conflicts (again)
  - operator precedence:  it's a PITA to do the whole intermediate
    productions precedence thing.  So, make it part of FPL.
    Ideas:
      - have later rules take precedence. (or earlier?)
        How to do it without creating too many states?
        Group precedences within (say) parens?
  x detect orphaned items
  x sort out the whole thing where states are returning useless
    products.   I guess state functions should just manipulate
    the .... parser state directly...?  that's what's actually being
    used, anyway.
  - the "new Product" thing is going to leak memory.  fix that.
    (pass on to the fpl author somehow?  or just don't declare it
    and don't worry about copies?)
  - repetition:  optional counts perhaps already work;  max_times
    is not implemented.  do them by boiling any foo* or foo+ or
    whatever down to a single item (with subitems).  this makes
    passing them to reduce code much saner.
  x eliminate duplicate state transitions
  - buffering the entire input is busted for things like stdin.
    stream instead;  but possibly fix that via chicken/egging it
    and generate the new parser with this.
  - Document:
    - the fact that you must supply "produces" (or have a default?)
    - the fact that the "produces" type must be to_string compatible;
      you can make it so by creating such a function in +{ }+.
 */

struct Options {
    std::string src_fpl;
    bool generate_main;
    bool debug;
    bool single_step;
    std::list<std::string> entry_points;

    // janky, but good enough:
    std::string _fail;
    Options(int argc, const char* const* argv) :
        generate_main(false),
        debug(false),
        single_step(false)
    {

        _fail = "";
        for(int argi = 1; argi < argc; argi++) {
            const char *arg = argv[argi];

            if(!arg)    continue; // should not be able to happen, but...
            if(!arg[0]) continue; // I guess ignore blank args

            if(arg[0] == '-') {
                if(arg[1] == '-') {
                    // double dash '--foo' style args:
                    std::string opt(arg + 2); // (+2 skips dashes)
                    std::string val;
                    size_t pos = opt.find_first_of("=");
                    if(pos != std::string::npos) {
                        val = opt.substr(pos + 1);
                        opt = opt.substr(0, pos);
                    }
                    if(opt == "debug") {
                        debug = true;
                    } else if(opt == "debug-single-step") {
                        debug = true;
                        single_step = true;
                    } else if(opt == "entry") {
                        // specifies an entry rule (i.e. a parsing
                        // starting point)
                        if(val.empty()) {
                            argi++;
                            if(argi < argc) {
                                val = std::string(argv[argi]);
                            }
                        }
                        if(val.empty()) {
                            _fail = "--generate-main requires a value.";
                        }
                        entry_points.push_back(std::string(val));
                    } else if(opt == "generate-main") {
                        generate_main = true;
                    } else {
                        _fail = "Unknown option: "; _fail += opt;
                    }
                } else {
                    // single-dash arg:
                    _fail = "Unknown option: "; _fail += arg;
                }
            } else {
                if(src_fpl.length() != 0) {
                    _fail = "only one source fpl is supported at present";
                } else {
                    src_fpl = arg;
                }
            }
        }
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
the type of thing produced (string/GrammarElement) and typically
a code block (string).  Each item has a minimum and maximum
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

    std::string nonterm_id_str() const {
        if(type == NONTERM_PRODUCTION)
            return "NontermID::_" + expr;

        // the ID passed isn't a nonterminal.
        // returning a string like this should
        // at least give something to grep for:
        return "Error: " + expr + " is not a nonterminal";
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

    inline bool is_single() const {
        return((min_times == 1) && (max_times == 1));
    }

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

        return out;
    }
};

class ProductionRule {
    std::string prod;
    std::vector<ProdExpr> steps;
    std::string code_str;
    size_t start_of_text;

public:

    ProductionRule(size_t at_byte) : start_of_text(at_byte) {
    }

    void add_step(ProdExpr step) {
        steps.push_back(step);
    }

    inline int num_steps() const { return steps.size(); }

    // returns NULL if index is out of bounds
    const ProdExpr *step(unsigned int index) const {
        if(index < steps.size()) {
            return &steps[index];
        }
        return NULL;
    }

    const std::string &product(const std::string &pr) {
        prod = pr;
        return prod;
    }

    const std::string &product() const {
        return prod;
    }

    GrammarElement product_element() const {
        return GrammarElement(product(), GrammarElement::NONTERM_PRODUCTION);
    }

    int line_number(const fpl_reader &inp) const {
        return inp.line_number(start_of_text);
    }

    std::string default_code() const {
        std::string out;

        // perhaps default could be:
        //  - if one arg, just return that arg.  this allows aliasing.
        //  - if more than one arg, return aggregate XXX do this part
        // .. or have the fpl author specify somehow.
// FIXME this only makes sense if there's only one argument
// and it's already reduced.
        out += "return arg[0].val();\n";

        return out;
    }

    std::string code(const std::string &src) {
        code_str = src;
        return code();
    }

    std::string code() const {
        if(code_str.length() > 0) {
            return code_str;
        }
        return default_code();
    }

    const char *product_c_str() const {
        return product().c_str();
    }

    std::string to_str() const {
        std::string out;
        for(auto step : steps) {
            out += step.to_str();
            out += " ";
        }

        out += "-> " + product();

        return out;
    }

/*
    const &std::vector<ProdExpr> steps() const {
        return steps;
    }
 */
};

class Productions {
    fpl_reader inp;

    std::string reduce_type;
    std::string preamble;

    std::vector<ProductionRule>     rules;
    std::multimap<std::string, int> rules_for_product; // product -> rule ind

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

        static const uint16_t no_rule = 0xffff;

        // this allows lr_items to be used as keys
        // in things like std::map
        friend bool operator<(const lr_item& left, const lr_item& right) {
            if(left.rule == right.rule)
                return left.position < right.position;
            return left.rule < right.rule;
        }

        // "false" indicates an invalid or non-item.
        operator bool() const { 
            return rule != no_rule;
        }

        // constructs a false item:
        lr_item() : rule(no_rule), position(0) { }

        lr_item(int rl, int pos) : rule(rl), position(pos) { }

        static lr_item from_id(uint32_t id) {
            return lr_item((id >> 16) & 0xffff, id & 0xffff);
        }

        uint32_t id() const {
            return rule << 16 | position;
        }

        std::string to_str(const Productions *prds) const {
            const ProductionRule &rl = prds->rules[rule];

            const int bs = 40;
            char buf[bs];
            snprintf(buf, bs, "%s (rule %i):", rl.product_c_str(), rule);
            buf[bs - 1] = '\0';
            std::string out(buf);

            int step;
            for(step = 0; step < rl.num_steps(); ++step) {
                if(step == position)
                    out += "•";
                out += " ";
                out += rl.step(step)->to_str();
            }
            if(step == position)
                out += "•";

            return out;
        }
    };

    // an lr_set is a set of lr items.
    // each state is represented by an lr_set.
    struct lr_set {
        mutable std::string _id_cache;
        std::set<lr_item> items;

        lr_set() { }

        // 1-item set:
        lr_set(const lr_item &in) { items.insert(in); }

        // The id of the set is a string generated from the
        // content of the items which can be compared to determine
        // if 2 sets are identical or not.
        std::string id() const {
            if(_id_cache.length() == 0) { 
                // I hate assert but it's been bugging me that the size of
                // the rule/position could change and cause this to break, so:
                const int len_each = 2*(sizeof(lr_item::rule) + sizeof(lr_item::position));
                assert(8 == len_each);
                const int len = items.size()*len_each + 1;
                char buf[len];
                char *bw = buf;
                for(auto it : items) {
                    snprintf(
                        bw, len_each+1, "%04x%04x", it.rule, it.position
                    );
                    bw += len_each;
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

        // returns an lr_item representing the reduction for
        // this set/state, or a false-valued lr_item if there's
        // no such reduction.
        lr_item reduction_item(const Productions *prds) const {
            for(auto it : items) {
                const ProductionRule &rl = prds->rules[it.rule];
                if(rl.num_steps() == it.position) {
                    return it;
                }
            }
            return lr_item(); // no reduction here
        }

        std::string to_str(
            const Productions *prds,
            const char *line_prefix = "",
            const char *line_suffix = "\n"
        ) const {
            // efficient? probably not. do I care?
            std::string out;
            for(auto it : items) {
                out.append(line_prefix);
                out.append(it.to_str(prds));
                out.append(line_suffix);
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
                fail(
                    "Error at line %i: Nothing produces «%s»\n",
                    rule.line_number(inp), pname.c_str()
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
                //   x "!" is complicated, so I got rid of it
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
        } while(set.items.size() > last_size); // i.e. until we add no more

        return set;
    }

    // "goto" operation from page 224 Aho, Sethi and Ullman.
    // given a current state and a lookahead item, returns
    // the next state.
    lr_set lr_goto(const lr_set &in, const GrammarElement &sym) {
        lr_set set;
        for(auto item : in.items) {
            const ProdExpr *step = rules[item.rule].step(item.position);
            if(step && step->matches(sym)) {
                set.add(lr_item(item.rule, item.position + 1));
            }
        }
        return lr_closure(set);
    }

    // used for detecting conflicts
    std::string transition_id(const ProdExpr &pexp, const lr_set &to) {
        char buf[40]; // 40 is arbitrarily larger than the 5 we'll need
        snprintf(buf, 40, "%0x_", element_index[pexp.gexpr]);
        std::string out(buf);
        out += to.id();
        return out;
    }

public:

    Productions(fpl_reader &src) : inp(src) {
        // element 0 is a null element and can be used to
        // indicate missing/uninitialized elements or such.
        // we count it as a nonterminal so that it can be
        // accessed via the enum etc.
        record_element(
            GrammarElement("_fpl_null", GrammarElement::NONTERM_PRODUCTION)
        );
    }

    // returns the code for an inline to_string function for...
    // things which are already strings!  yay!
    // this is effectively c++ compiler appeasement.
    std::string to_string_identity() {
        return "\ninline std::string to_string(const std::string &in) {\n"
               "    return in;\n"
               "}\n";
    }

    bool parse_directive(const std::string &in) {
        std::regex  re("produces\\s*=?\\s*(.+)\\s*$");
        std::cmatch matched;
        if(std::regex_search(in.c_str(), matched, re)) {
            reduce_type = matched[1];
            add_preamble(to_string_identity());
            return true;
        }

        return false;
    }

    void push_rule(const ProductionRule &rule) {
        int rule_num = rules.size();
        rules.push_back(rule);

        for(int stp = 0; stp < rule.num_steps(); stp++) {
            record_element(rule.step(stp)->gexpr);
        }
        record_element(rule.product_element());

        rules_for_product.insert(std::make_pair(rule.product(), rule_num));
    }

    void add_preamble(const std::string &code) {
        preamble += "\n";
        preamble += code;
    }

    // returns the name of the function to use for the
    // given state
    std::string state_fn(const lr_set &state, bool fully_qualified = false) {
        int state_num = state_index[state.id()];
        return state_fn(state_num, fully_qualified);
    }

    std::string state_fn(int state_num, bool fully_qualified = false) {
        std::string fn("state_");

        fn += std::to_string(state_num);

        if(fully_qualified)
            fn = fq_member_name(fn);

        return fn;
    }

    std::string rule_fn(int rule_ind, bool fq = false) {
        std::string fn("rule_");
        fn += std::to_string(rule_ind);

        if(fq)
            fn = fq_member_name(fn);

        return fn;
    }

    // for debugging, generate code which stops for input
    // for each state.  user hits return to go to the next
    // step.  this I found easier than lldb command line.
    // I tried running lldb in visual studio code but for
    // some reason it would hang on the first breakpoint
    // with the subproc spinning at 100% cpu, which was not
    // useful. so here's my punt:
    std::string debug_single_step_code(const Options &op, const lr_set &st) {
        std::string out;
        if(op.debug) {
            std::string sfn = state_fn(st);
            out += "fprintf(stderr, \"%p entering " + sfn + ":\\n\", this);\n";
            out += "fprintf(stderr, \"%s\", base_parser.to_str().c_str());\n";
            if(op.single_step) {
                out += "char stopper;  std::cin.get(stopper);\n";
            }
        }
        return out;
    }

    std::string code_for_rule(const Options &opts, int rule_ind) {
        const ProductionRule &rule = rules[rule_ind];
        std::string out;

        // parameters (this may be too complicated):
        //  - the rule itself
        //  - the current parser state
        //  x arg[] array.  callers figure out length based on rule.
        //  x uhh... argX for each arg, with the appropriate type
        //    so that fpl authors don't have to convert each Product arg..
        out += reduce_type + " " + rule_fn(rule_ind) + "(";
        out += "Product arg[]";
        for(int stind = 0; stind < rule.num_steps(); stind++) {
            std::string arg_name = "arg_" + std::to_string(stind);
            const ProdExpr *expr = rule.step(stind);
            if(!expr) {
                fail("Bug: no expression for %s in %s",
                    arg_name.c_str(), rule.to_str().c_str()
                );
            } else if(!expr->is_single()) {
                // the argument is either optional or can repeat or
                // both, so we can't pass it simply.  so, instead,
                // pass it as a product:
                out += ", Product " + arg_name;
            } else if(expr->is_terminal()) {
                out += ", std::string " + arg_name;
            } else {
                out += ", " + reduce_type + " " + arg_name;
            }
        }
        out += ") {\n" + rule.code();
        out += "\n}\n";
        return out;
    }

    // returns code for reduce within a given state
    std::string production_code(
        const Options &opts,
        const lr_set &state,
        int rule_ind
    ) {
        std::string out;
        const ProductionRule &rule = rules[rule_ind];

        //   In what form should the production code be provided?
        // What should we provide for the author of the production
        // code?
        //   For the moment, production code will need to be written
        // in c++.  Authors of the production code should provide
        // a matching header which defines their product type,
        // as well as including anything else they're going to need
        //   In the spirit of jest, we should provide as much visibility
        // into the state of the compiler as possible.  Obviously we
        // need to pass in the generated products.  Can we show the
        // whole stack?
        // pass, named:
        //  - const &parser (with stack popped or not?)
        //  - arguments from the stack:

        //  - one argument per element (step) in the rule.
        //    to handle repetition, boil any foo+ or foo*
        //    down to a single item containing an array of
        //    other items.  it's the only sane way.
        //    implement by making wildcard things implicitly
        //    make their own elements. (i.e. if there's foo+
        //    or foo*, those each get their own nonterminals).
        std::string ns_str = std::to_string(rule.num_steps());
        out += "\n";
        out += "    State next_state;\n";
        out += "    Product args[" + ns_str + "];\n";
        out += "    for(int aind = " + ns_str + " - 1; aind >= 0; --aind) {\n";
        out += "        StackEntry ste = base_parser.lr_pop();\n";
        out += "        next_state = ste.state;\n";
        out += "        args[aind] = ste.product;\n";
        out += "    }\n";
        out += "    " + reduce_type + " result = " + rule_fn(rule_ind);
        out +=     "(args";
        for(int stind = 0; stind < rule.num_steps(); stind++) {
            std::string arg = ", args[" + std::to_string(stind) + "]";
            const ProdExpr *expr = rule.step(stind);
            if(!expr) {
                fail("Bug: no expression for step %i in %s",
                    stind, rule.to_str().c_str()
                );
            } else if(!expr->is_single()) {
                out += arg;
            } else if(expr->is_terminal()) {
                out += arg + ".term_str()";
            } else {
                out += arg + ".val()";
            }
        }
        out +=     ");\n";
        // XXX what deletes the product?  this is awful.
        out += "    base_parser.set_product(new Product(result, "
             + rule.product_element().nonterm_id_str()
             + "));\n";

        return out;
    }

    std::string state_goto(const lr_set &in, const GrammarElement &sym) {
        std::string out;
        return out;
    }

    std::string args_for_shift(const lr_set &state, const ProdExpr &expr) {
        lr_set next_state = lr_goto(state, expr.gexpr);
        std::string el_id(std::to_string(element_index[expr.gexpr]));
        if(expr.is_terminal()) {
            return "\"" + expr.terminal_string() + "\""
                 + ", " + el_id
                 + ", &" + state_fn(next_state, true);
        } else {
            return expr.gexpr.nonterm_id_str()
                 + ", &" + state_fn(next_state, true);
        }
    }

    void reduce_reduce_conflict(int r1, int r2) {
        warn("reduce/reduce conflict:\n    %s line %i\n vs %s line %i\n",
            rules[r1].to_str().c_str(), rules[r1].line_number(inp),
            rules[r2].to_str().c_str(), rules[r2].line_number(inp)
        );
    }

    void shift_reduce_conflict(int r1, int r2) {
        // XXX fill in; use
        warn("OH HAI SHIFT REDUCE CONFLICT YESSSSSSS\n");
    }

    // reports what is probably a shift/shift conflict,
    // which would probably only happen due to a bug in
    // this program...
    void other_conflict(lr_item item1, lr_item item2) {
        warn("conflict:\n    %s\n vs %s\n",
           item1.to_str(this).c_str(), item2.to_str(this).c_str()
        );
    }

    std::string code_for_state(const Options &opts, const lr_set &state) {
        std::string out;
        std::string sfn = state_fn(state);

        out += "//\n";
        out += state.to_str(this, "// ");
        out += "//\n";
        out += "void " + sfn + "() {\n";
        out += "    Product prd;\n";
        out += debug_single_step_code(opts, state);

        out += "    if(0) {\n"; // now everything past this can be "else if"

        // let's think about conflicts, shall we?
        // We should:
        //   x if there's more than one reduce, report a reduce/reduce conflict
        //     (but fail open and blat out some code anyway.  actually, would be
        //     nice to annotate the generated code...)
        //   - if there's an existing symbol id -> next state for this symbol
        //     x if the next state is the same, skip this transition because
        //       it's redundant (can we eliminate this earlier than here? yes.)
        //     - else there's either a shift/shift conflict, which makes no
        //       sense, or it's a shift/reduce conflict (assuming one of the
        //       states we're going to is just a reduce).  either way, report
        //       it.
        //   NOTE:  if there's a shift/reduce conflict, we will resolve it:
        //      - first by longest match (i.e. shift instead of reducing).
        //        example:  if() ...  vs if() ... else ...;
        //        if() .. else is longer so we shift.
        //      - next by operator precedence.. XXX implement
        //   ... I think we still want to report it.  or do we?
        //       
        //   - record symbol id -> next state
        //

        lr_item reduce_item;
        std::map<int, int> transition; // grammar element id -> state number
        std::map<int, lr_item> item_for_el_id;
        for(lr_item item : state.items) {
            const ProductionRule &rule = rules[item.rule];
            const ProdExpr *right_of_dot = rule.step(item.position);
            if(!right_of_dot) {
                if(reduce_item) {
                    reduce_reduce_conflict(item.rule, reduce_item.rule);
                } else {
                    reduce_item = item;
                }
            } else {
                lr_set next_state = lr_goto(state, right_of_dot->gexpr);
                int el_id = element_index[right_of_dot->gexpr];

                auto existing = transition.find(el_id);
                if(existing != transition.end()) {
                    if(existing->second == state_index[next_state.id()]) {
                        // already have a case for this transition.
                        // no problem, no need to generate another copy;
                        // just move on to the next item.
                        continue;
                    } else {
                        // ... shift/shift conflict - probably can't happen.
                        other_conflict(item, item_for_el_id[existing->first]);
                    }
                }

                const GrammarElement::Type type = right_of_dot->type();
                switch(type) {
                    // shifts
                    case GrammarElement::TERM_EXACT:
                        out += "} else if(base_parser.shift_exact(";
                        break;
                    case GrammarElement::TERM_REGEX:
                        out += "} else if(base_parser.shift_re(";
                        break;
                    case GrammarElement::NONTERM_PRODUCTION:
                        out += "} else if(base_parser.shift_nonterm(";
                        break;
                    case GrammarElement::NONE:
                        // .. this pretty much implies a bug in fpl2cc:
                        fail(
                            "Missing/unknown grammar element (id: %i %s)",
                            type, right_of_dot->to_str().c_str()
                        );
                        break;
                }
                // XXX redundant goto and other calculations here. restructure.
                out += args_for_shift(state, *right_of_dot) + ")) {\n";

                transition[el_id] = state_index[next_state.id()];
                item_for_el_id[el_id] = item;

                out += "    // transition ID: " + transition_id(
                    *right_of_dot, lr_goto(state, right_of_dot->gexpr)
                ) + "\n";
            }
        }

        out += "} else {\n";
        if(!reduce_item) {
            // XXX cooler would be to reduce this to an error
            // also much cooler would be to have a comprehensible message
            out += "    base_parser.error(\"unexpected input in " + sfn + "\\n\");\n";
        } else {
            out += production_code(opts, state, reduce_item.rule);
        }

        out += "}\n";

        out += "}\n"; // end of state_ function

        return out;
    }

    // reformats the generated code to fix indents and what have you.
    // this is fairly rough but does result in more or less readable
    // code for most cases.
    std::string reformat_code(const std::string code) {
        const int chars_per_indent = 4;
        int indent = 0;
        int newlines = 0;

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
        for(std::string::size_type inp = 0; inp < code_length; inp++) {
            if(code[inp] == '{') {
                // only count '{' at end of line, which is a
                // good-enough hack to avoid issues with
                // quoted { or comments with { or whatever.
                if(code[inp + 1] == '\n') {
                    indent += chars_per_indent;
                }
            } else if(code[inp] == '}') {
                // counterpart to the above good-enough hack:
                // only count '}' if it's the start of a
                // new line:
                if(newlines > 0)
                    indent -= chars_per_indent;
            } else if(code[inp] == '\n') {
                // skip spaces at start of line - we'll convert them
                // to the correct indent later.
                while(inp < code_length && isspace(code[inp])) {
                    // (.. but preserve newlines)
                    if(code[inp] == '\n') newlines++;
                    inp++;
                }
                if(inp < code_length)
                    inp--;
                continue;
            }

            if(newlines > 0) {
                while(newlines > 0) {
                    output.push_back('\n');
                    --newlines;
                }

                for(int ind = 0; ind < indent; ind++)
                    output.push_back(' ');
            }

            output.push_back(code[inp]);

        }
        return output;
    }

    // returns an enum string for nonterminals.
    // this makes the generated code easier to read - eg
    // instead of "if(prd.grammar_element_id == 62)" we can
    // do "if(prd.grammar_element_id == NontermID::_decimal_constant)"
    std::string nonterm_enum() {
        std::string out;
        std::string nonterm_str_guts;

        out += "typedef enum {\n";
        for(int el_id = 0; el_id < elements.size(); ++el_id) {
            if(elements[el_id].type != GrammarElement::NONTERM_PRODUCTION) {
                // include terminals as comments.  we don't need them
                // in the enum (and how would they be named, anyway?),
                // but it's nice to be able to see all the grammar
                // elements in one place:
                out += "// " + elements[el_id].to_str();
            } else {
                // prefix with underscore as a hack to avoid keyword
                // collisions:
                std::string name("_" + elements[el_id].to_str());
                out += name;
                nonterm_str_guts += "case " + std::to_string(el_id);
                nonterm_str_guts += ": return \"" + name + "\";\n";
            }

            out += " = ";
            out += std::to_string(el_id);

            // sigh.. languages which don't allow trailing comma..
            if(el_id + 1 < elements.size()) 
                out += ",\n";
            else
                out += "\n";
        }
        out += "} NontermID;\n\n";
        out += "static std::string nonterm_str(int id) {\n";
        out += "    switch(id) {\n";
        out += nonterm_str_guts;
        out += "    }\n";
        out += "    return \"not a nonterm .. err XXX \";\n";
        out += "}\n\n";
        return out;
    }

    std::string state_to_string() {
        std::string out("static std::string state_to_str(State st) {\n");
        for(auto st: states) {
            out += "if(&" + state_fn(st, true) + " == st) ";
            out += "return \"" + state_fn(st) + "\";\n";
        }
        out += "    return \"<not a state>\";\n";
        out += "}\n";

        return out;
    }

    std::string parser_class_name() {
        return inp.base_name() + "_parser";
    }

    // returns fully-qualified name of a member of the parser class
    std::string fq_member_name(const std::string &mem) {
        return parser_class_name() + "::" + mem;
    }

    std::string code_for_main(const std::string &parser_class) {
        std::string out("\n\n");

        // the main() generated here is pretty much just a test stub.
        // if people want something fancier, they can make their own.
        out += "int main(int argc, const char **argv) {\n";
        out += "    if(argc < 2) {\n";
        // XXX this is weak;  make the reader able to read stdin
        out += "        fpl_reader::default_fail(";
        out += "            \"Please provide a source file name.\\n\"";
        out += "        );\n";
        out += "    }\n";
        // XXX this is also weak; handle more than one source
        out += "    fpl_reader inp(argv[1]);\n";
        out +=      parser_class + " parser(inp);\n";
        out += "    using namespace std;\n";
        out += "    printf(\"result: %s\\n\", to_string(parser.parse()).c_str());\n";
        out += "}\n";

        return out;
    }

    void generate_states(const Options &opts) {
        std::list<std::string> wanted = opts.entry_points;

        if(rules.empty()) {
            fail("No rules found");
        }

        if(wanted.empty()) {
            wanted.push_back(rules[0].product());
        }

        lr_set entry_set;
        for(auto entry_prod : wanted) {
            auto strl  = rules_for_product.lower_bound(entry_prod);
            auto endrl = rules_for_product.upper_bound(entry_prod);
            if(strl == endrl) {
                fail("Can't find entry rule for '%s'\n", entry_prod.c_str());
            }
            for(auto rit = strl; rit != endrl; ++rit) {
                entry_set.items.insert(lr_item(rit->second, 0));
            }
        }
        add_state(lr_closure(entry_set));


        // Aho, Sethi, and Ullman page 224... uhh, modified.
        const auto no_set = state_index.end();
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
    }

    void generate_code(const Options &opts) {
        const std::string parser_class = parser_class_name();
        // "base" is probably the wrong term.. XXX
        const std::string base_parser_class = "FPLBaseParser<" + parser_class + ">";

        generate_states(opts);

        std::string out;
        out += "#include <iostream>\n"; // XXX needed?
        // preamble has to come before the fpl headers
        // because (to my surpise) things like to_string
        // functions (called by the template) have to be
        // declared before the template (not just before
        // template instantiation.. ?) (did I miss something?)
        out += preamble;
        out += "#include \"fpl2cc/fpl_reader.h\"\n";
        out += "#include \"fpl2cc/fpl_base_parser.h\"\n";
        out += "\n";
        // parser_class is now really parser_impl_class or such...
        out += "class " + parser_class + " {\n";
        // Can't do the private declarations first because the
        // FPLBaseParser template needs to know about state methods
        // and such to determine the types it needs, but c++ won't
        // let you predeclare such methods.
        out += "public:\n";
        out += "    // state() and reduce_type tell FPLBaseParser what types to use\n";
        out += "    void state();\n"; // this must match state_x methods; XXX document/typedef?
        out += "    static " + reduce_type + " reduce_type;\n"; // for telling the FPLBaseParser.. as above XXX document
        out += "private:\n";
        out += "    using FPLBP = " + base_parser_class + ";\n";
        out += "    using State = FPLBP::State;\n";
        out += "    using Product = FPLBP::Product;\n";
        out += "    using StackEntry = FPLBP::StackEntry;\n";
        out += "    FPLBP base_parser;\n";
        out += "public:\n";

        out += nonterm_enum();

        for(int rnum = 0; rnum < rules.size(); rnum++) {
            out += code_for_rule(opts, rnum);
        }

        out += state_to_string();

        for(lr_set state : states) {
            out += code_for_state(opts, state).c_str();
        }

        out += "    " + parser_class + "(fpl_reader &src) : base_parser(src) { }\n";
        out += "    inline " + reduce_type + " parse() {\n";
        out += "        return base_parser.parse(*this);\n";
        out += "    };\n";

        out += "};\n"; // end of class

        if(opts.generate_main) {
            out += code_for_main(parser_class);
        }

        printf("%s\n", reformat_code(out).c_str());

        report_unused_rules();
    }

    void report_unused_rules() {
        bool used[rules.size()];
        for(int rind = 0; rind < rules.size(); rind++) {
            used[rind] = false;
        }
        for(lr_set state : states) {
            for(lr_item item : state.items) {
                used[item.rule] = true;
            }
        }
        for(int rind = 0; rind < rules.size(); rind++) {
            if(!used[rind]) {
                const ProductionRule &rule = rules[rind];
                warn(
                    "Rule producing %s on line %i is unused\n",
                    rule.product().c_str(), rule.line_number(inp)
                );
            }
        }
    }
};

void read_quantifiers(fpl_reader &src, ProdExpr &expr) {
    const utf8_byte *inp = src.inpp();

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



int read_expressions(fpl_reader &src, ProductionRule &rule) {
    int num_read = 0;
    bool done = false;
    do {
        src.eat_space();

        const utf8_byte *inp = src.inpp();

        if(!*inp)
            break;

        std::string expr_str;
        GrammarElement::Type type = GrammarElement::Type::NONE;

        switch(*inp) {
            case '\0':
                done = true;
                break; // EOF
            case '#':
                // line comment - just skip
                src.read_line();
                break;
            case '"':
            case '\'':
                expr_str = src.read_to_byte(*inp);
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
                // should be the name of a production.
                // production names must start with a letter, and
                // thereafter may contain letters, digits, or underscores.
                expr_str = src.read_re("[A-Za-z][A-Za-z0-9_]+")[0];
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

std::string read_code(fpl_reader &src) {
     // code is within "+{" "}+" brackets.
     // this is done simplistically, which means you will
     // derail it if you put +{ or }+ in a comment or
     // string or whatever.  so try not to do that.
     src.eat_space();
     const utf8_byte *start = NULL;
     const utf8_byte *end   = NULL;
     if(src.read_byte_equalling(';')) {
         return ""; // default code generated on the fly
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

   <exprs or productions to match> -> <production name> +{ <code> }+
                    or
   <exprs to match> -> <production name> ;

Also, comments.  Let's use # just cuz.

 */
void fpl2cc(const Options &opts) {
    fpl_reader inp(opts.src_fpl, fail);

    Productions productions(inp);
    do {
        inp.eat_space();
        if(*inp.inpp() == '#') {
            inp.read_line();
        } else if(*inp.inpp() == '+') {
            // inlined/general code - goes at the top of the generated
            // code.  Use to define types or whatever.
            std::string code = read_code(inp);
            if(code.length() > 0) {
                productions.add_preamble(code);
            }
        } else if(inp.read_byte_equalling('@')) {
            std::string line = inp.read_line();
            if(!productions.parse_directive(line)) {
                fail("Unknown directive: %s\n", line.c_str());
            }
        } else {
            ProductionRule rule(inp.current_position());
            if(read_expressions(inp, rule)) {
                // .. we've read the expressions/steps leading to
                // the production (including the "->").
                // read what the expressions above produce:
                inp.eat_space();
                const utf8_byte *start = inp.inpp();
                rule.product(inp.read_to_space());
                if(rule.product().length() <= 0) {
                    fail(
                        "missing production name on line %i near %.12s\n",
                        rule.line_number(inp), start
                    );
                }

                // read the code for the production
                rule.code(read_code(inp));

                // add it to the set of productions
                productions.push_rule(rule);
            }
        }
    } while(!inp.eof());

    productions.generate_code(opts);

}

void usage() {
    fprintf(stderr,
        "\nUsage:    fpl2cc [options] <source> [target]\n\n"
        "If no [target] is specified, prints to stdout.\n\n"
    );
}

int main(int argc, const char** argv) {
    Options opts(argc, argv);

    if(opts._fail.length()) {
        fprintf(stderr, "%s\n", opts._fail.c_str());
        usage();
        exit(1);
    } else {
        fpl2cc(opts);
    }

    if(num_warnings > 0) {
        fprintf(stderr, "%i warnings\n", num_warnings);
    }

    exit(0);
}


