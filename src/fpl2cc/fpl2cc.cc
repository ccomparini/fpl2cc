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

#define CODE_BLOCK(str) CodeBlock(std::string(__FILE__), __LINE__, str)

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
  - way to do specialized scans. ~scan_function_name maybe?
    or scan classes.
  - "^" to refer to prior rules?  think it over. Also note in general the
    lower precedent rules are earlier in the file so maybe another symbol.
    and one would scope it to the file, I guess?
  - sub-fpls to implement particular constructs, from another file?
    maybe `sub_fpl.fpl` -> name_of_target_from_sub ;
    Then you can (eg) rip from c;
    OR implement stuff like embedding formatting in strings.
  - repetition/optionals
  - multi-pass for comments etc. HOW!?  you thought about this
    before and perhaps had a plan. Also for things like "#" modifier.
    how about filters on fpl reader or whatever.  layered.  each layer
    is the input to the one above.  so the "passes" happen simultaneously.
    BUT you have to be able to examine the state of lower layers (eg
    to find the current comment or whatever).  that's the key notion.
    (uhh, see above: "specialized scans")
  o detect conflicts (again)
  - operator precedence:  it's a PITA to do the whole intermediate
    productions precedence thing.  So, make it part of FPL.
    Ideas:
      - have later rules take precedence. (or earlier?)
        How to do it without creating too many states?
        Group precedences within (say) parens?
      - OR somehow within a rule specify the order
        of precedence.  preferably in some relative fashion...
        @precedecence( ... )?  multi line, same prec grouped
        by being on the same line?
  - the "new Product" thing is going to leak memory.  fix that.
    (pass on to the fpl author somehow?  or just don't declare it
    and don't worry about copies?).  Probably can delete when the
    stack is popped (or after)
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
    FILE *out;
    std::string output_fn;
    bool generate_main;
    bool debug;
    bool single_step;
    std::list<std::string> entry_points;

    std::list<std::string> errors;
    inline void error(const std::string &errst) {
        errors.push_back(errst);
    }

    // janky, but good enough:
    Options(int argc, const char* const* argv) :
        out(stdout),
        output_fn("«stdout»"),
        generate_main(false),
        debug(false),
        single_step(false)
    {
        for(int argi = 1; argi < argc; argi++) {
            // c++ -- :P
            #define SCAN_VALUE() \
                if(val.empty()) { \
                    argi++; \
                    if(argi < argc) { \
                        val = std::string(argv[argi]); \
                    } \
                }

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
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--generate-main requires a value.");
                        entry_points.push_back(std::string(val));
                    } else if(opt == "generate-main") {
                        generate_main = true;
                    } else if(opt == "out") {
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--out requires a value.");
                        output_fn = val;
                        out = fopen(output_fn.c_str(), "w");
                        if(!out) {
                            error(
                                "can't open '" + output_fn + "' for write: " +
                                strerror(errno)
                            );
                        }
                    } else {
                        error("Unknown option: --" + opt);
                    }
                } else {
                    // single-dash arg:
                    error("Unknown option: " + std::string(arg));
                }
            } else {
                if(src_fpl.size() != 0) {
                    error("only one source fpl is supported at present");
                } else {
                    src_fpl = arg;
                }
            }
            #undef SCAN_VALUE
        }
    }
};

struct CodeBlock {
    std::string source_file;
    int line;
    std::string code;

    CodeBlock() : line(0) { }

    CodeBlock(const std::string &file, int ln, const std::string &cd) :
        source_file(file),
        line(ln),
        code(cd) {
    }

    operator bool() const {
        return code.length();
    }

    void append(const std::string &str) {
        code += str;
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

    inline bool is_optional() const {
        return min_times == 0;
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
    CodeBlock code_for_rule;
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

    CodeBlock default_code() const {
        std::string code;

        int first_nonterm = -1;
        int num_nonterms = 0;
        for(int sti = 0; sti < steps.size(); sti++) {
            if(!steps[sti].is_terminal()) {
                num_nonterms++;
                if(first_nonterm < 0)
                    first_nonterm = sti;
            }
        }

        if(num_nonterms >= 1) {
            if(num_nonterms != 1) {
                warn(
                    "default code for nontrivial rule %s probaby won't dtrt\n",
                    to_str().c_str()
                );
            }

            // if there's exactly one nonterminal, I think returning it
            // is a sane-enough default.  this allows for aliasing
            // (where you have just foo -> bar) as well as doing the
            // right thing in some simple cases like parenthesized
            // expressions.
            // If there's more than one nonterm, we still need to return
            // something, so:
            code = "return arg_" + std::to_string(first_nonterm) + ";";
        } else {
            // this rule has terminals only.  maybe the reduce_type has
            // a constructor which can do something sane (i.e. convert
            // from a string or set of strings).  if not, there will
            // be a compile error which, hopefully, the fpl author can
            // figure out.
            code = "return reduce_type(";
            for(int sti = 0; sti < steps.size(); sti++) {
                code += "arg_" + std::to_string(sti);
                if(sti < steps.size() - 1)
                    code += ", ";
            }
            code += ");";
        }


        return CodeBlock(std::string(__FILE__), __LINE__, code);

/*
        This was a fun attempt but doesn't dtrt except in trivial
        cases, and is hard to debug in cases where it doesn't work
        (i.e. doesn't compile) because of deceptive line numbers
        being reported etc.  So let's not do this this way.

        An alternate would be to allow to supply default code in
        an @ directive or such.  Or make this kind of thing optional
        in some other way.

        // XXX the file and line here needs to be the generated file/line, or
        // it's too hard to figure out where we're messed up.  we should
        // add a comment, though, telling where in this file generated the code.
        // ... actually maybe giving the fpl source location is the next best thing.
        // but errf this has no access to the source.
        CodeBlock block = CODE_BLOCK("return ");
        std::string code;
        for(int argi = 0; argi < steps.size(); argi++) {
            // XXX how to deal with repetition?
            // this will definitely dtwt with repetition on terminals
            const ProdExpr &arg = steps[argi];
            const std::string argvar("arg_" + std::to_string(argi));
            switch(arg.gexpr.type) {
                case GrammarElement::TERM_EXACT:
                    // let's guess it might be a straight up operator,
                    // and also one which is valid in our target language!
                    code += arg.gexpr.expr;
                    break;
                case GrammarElement::TERM_REGEX:
                    // possibly this is something like a constant or
                    // variable name or something.
                    // possibly the reduce type knows what to do?
                    code += "reduce_type(" + argvar + ")";
                    break;
                case GrammarElement::NONTERM_PRODUCTION:
                    // this is already reduced, so try just using/returning it
                    code += argvar;
                    break;
                default:
                    // XXX maybe warn here?
                    break;
            }

            if(argi < steps.size() - 1) {
                code += " ";
            } else {
                code += ";\n";
            }
        }
        block.append(code);
        return block;

            // perhaps default could be:
            //  - if one nonterminal, return that one nonterminal.  this allows
            //    aliasing and stuff like '(' foo ')' to do what's intuitive.
            //  - if two nonterminals separated by a terminal, return
            //  - if more than one arg, return aggregate XXX do this part
            // .. or have the fpl author specify somehow.
 */
    }

    void code(const CodeBlock &cd) {
        code_for_rule = cd;
    }

    CodeBlock code() const {
        if(code_for_rule) {
            return code_for_rule;
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
};

class Productions {
    fpl_reader inp;

    std::string reduce_type;
    std::string preamble;
    std::list<std::string> goal; // goal is any of these

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
        // in things like std::map.
        // it also determines the order of the items
        // such that earlier items
        // are ordered earlier here.
        friend bool operator<(const lr_item& left, const lr_item& right) {

            // same rule?  earlier position in the rule
            // comes first.
            if(left.rule == right.rule)
                return left.position < right.position;

            // otherwise items from earlier rules come earlier.
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
private:
        mutable std::string _id_cache;
        std::set<lr_item> items;
public:

        lr_set() { }

        // an lr_item is a 1-item set:
        lr_set(const lr_item &in) {
            items.insert(in);
        }

        // The id of the set is a string generated from the content
        // of the items which can be compared to determine if 2 sets
        // are identical (within these Productions) or not.
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

        const std::set<lr_item> &iterable_items() const {
            return items;
        }

        int size() const {
            return items.size();
        }

        void add_item(const lr_item &it) {
            items.insert(it);
        }

        void add_set(const lr_set &set) {
            for(auto it : set.items) {
                //items.insert(it);
                add_item(it);
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
                //set.add_item(lr_item(rit->second, 0));
                add_expanded(set, lr_item(rit->second, 0));
            }
        }
    }

    // based on the closure algorithm on page 222 of Aho, Sethi, Ullman
    // (1988), but with the addition of support for the *+?! operators
    // in fpl.
    //  http://www.cs.ecu.edu/karl/5220/spr16/Notes/Bottom-up/lr0item.html
    //   1) All members of S are in the closure(S).
    //   2) Suppose closure(S) contains item A → α⋅Bβ, where B is a nonterminal.
    //      Find all productions B → γ1, …, B → γn with B on the left-hand side.
    //      Add LR(0) items B → ⋅γ1, … B → ⋅γn to closure(S).
    //  What is the point of the closure? LR(0) item E → E + ⋅ T indicates that the
    //  parser has just finished reading an expression followed by a + sign. In
    //  fact, E + are the top two symbols on the stack.
    //  Now, the parser is looking to see if there is a T next. (It does not
    //  predict that there is a T next. It is just considering that as a
    //  possibility.) But that means it should be looking for something that
    //  is the right-hand side of a production for T. So we add items for T
    //  with the dot at the beginning.
    lr_set lr_closure(const lr_set &in) {
        lr_set set = in;
        int last_size;
        do {
            last_size = set.size();
            // NOTE this could be a lot more efficient if we started
            // at element (last_size - 1)...
            for(auto &item: set.iterable_items()) {
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
        } while(set.size() > last_size); // i.e. until we add no more

        return set;
    }

    // adds the given lr_item to the lr_set, plus any other
    // related items to cover optionalness/repetition
    // XXX OK so perhaps:
    //   - code generation deals with optionals and repeats
    //   - modify goto such that it can understand to enter
    //     states beginning with optionals
    void add_expanded(lr_set &set, const lr_item &it) {

        const ProductionRule &rule = rules[it.rule];
        for(int pos = it.position; pos <= rule.num_steps(); pos++) {
            set.add_item(lr_item(it.rule, pos));
            const ProdExpr *expr = rule.step(pos);
            if(!expr || !expr->is_optional()) {
                // end of rule or the items is not optional so we
                // don't need to look after:
                break;
            }
        }
    }

    // "goto" operation from page 224 Aho, Sethi and Ullman.
    // given a current state and a lookahead item, returns
    // the next state (i.e. the set of items which might appear
    // next)
    lr_set lr_goto(const lr_set &in, const GrammarElement &sym) {
        lr_set set;
        for(auto item : in.iterable_items()) {
            const ProdExpr *step = rules[item.rule].step(item.position);
            if(step && step->matches(sym)) {
                add_expanded(set, lr_item(item.rule, item.position + 1));
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

        parse_fpl();
    }

    // returns the code for an inline to_string function for...
    // things which are already strings!  yay!
    // this is effectively c++ compiler appeasement.
    CodeBlock to_string_identity() {
        return CODE_BLOCK(
            "\n#ifndef TO_STRING_HACK\n"
            "#define TO_STRING_HACK\n"
            "inline std::string to_string(const std::string &in) {\n"
            "    return in;\n"
            "}\n"
            "#endif // TO_STRING_HACK\n"
        );
    }

    bool parse_directive(const std::string &in) {
        // "produces" is the only directive so far...
        std::regex  re("produces\\s*=?\\s*(.+)\\s*$");
        std::cmatch matched;
        if(std::regex_search(in.c_str(), matched, re)) {
            reduce_type = matched[1];
            if(reduce_type == "std::string") {
                add_preamble(to_string_identity());
            }
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

    void add_preamble(const CodeBlock &code) {
        preamble += code.format();
    }

    static void read_quantifiers(fpl_reader &src, ProdExpr &expr) {
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
            default:
                expr.min_times = 1;
                expr.max_times = 1;
                break;
        }
    }

    // production names must start with a letter, and
    // thereafter may contain letters, digits, or underscores.
    static inline std::string read_production_name(fpl_reader &src) {
        return src.read_re("[A-Za-z][A-Za-z0-9_]*")[0];
    }

    // .. imports relevant rules into this and returns the name of
    // the top level production created
    std::string parse_import(fpl_reader &src, ProductionRule &rule) {
        // importing another fpl source.
        // syntax: '`' filename /`(:production_to_import)?/
        // what it needs to do:
        //  - create a sub-parser for the production. how to fold
        //    that into states?  possibly just fold the whole
        //    sub-parse in, which would be easiest.

        std::string filename(src.read_to_byte('`'));
        fpl_reader inp(filename, fail);
        std::string prod_name;
        if(src.read_byte_equalling(':')) {
            prod_name = read_production_name(src);
        }

        // consider keeping this around so we don't have to re-read it
        // if something else wants to import from the same fpl
        Productions subs(inp);

        return import_rules(subs, prod_name);
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
                case '`':
                    // parse/import the sub-fpl, and use whatever it produces:
                    expr_str = parse_import(src, rule);
                    type     = GrammarElement::Type::NONTERM_PRODUCTION;
                    break;
                default:
                    // should be the name of a production.
                    expr_str = read_production_name(src);
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

    static CodeBlock read_code(fpl_reader &src) {
         // code is within "+{" "}+" brackets.
         // this is done simplistically, which means you will
         // derail it if you put +{ or }+ in a comment or
         // string or whatever.  so try not to do that.
         src.eat_space();
         size_t start = -1;
         size_t end   = -1;

         if(src.read_byte_equalling('+') && src.read_byte_equalling('{')) {
             start = src.current_position();
             while(char byte_in = src.read_byte()) {
                 if(byte_in == '}') {
                     if(src.read_byte() == '+') {
                         end = src.current_position() - 2;
                         break;
                     }
                 }
             }
         }

         if((start >= 0) && (end > 0)) {
             return CodeBlock(
                 src.filename(),
                 src.line_number(start),
                 src.read_range(start, end)
             );
         }

         // else error - no start of code or ';'
         src.error("expected start of code (\"+{\") or \";\"");
         return CodeBlock();
    }

    void parse_fpl() {
        do {
            inp.eat_space();
            if(*inp.inpp() == '#') {
                inp.read_line();
            } else if(*inp.inpp() == '+') {
                // inlined/general code - goes at the top of the generated
                // code.  Use to define types or whatever.
                CodeBlock code(read_code(inp));
                if(code) {
                    add_preamble(code);
                }
            } else if(inp.read_byte_equalling('@')) {
                std::string line = inp.read_line();
                if(!parse_directive(line)) {
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

                    inp.eat_space();

                    // next we expect either ';' or a code block.
                    // if it's ';' we read it and move on;  otherwise
                    // it's a code block for the rule.
                    if(!inp.read_byte_equalling(';'))
                        rule.code(read_code(inp));

                    push_rule(rule);
                }
            }
        } while(!inp.eof());
    }

    std::string import_rules(const Productions &from, const std::string &pname) {
        std::string src_fn = from.inp.filename();
        if(from.reduce_type != reduce_type) {
            fail("Incompatible reduce type '%s' in %s (expected %s)\n",
                from.reduce_type.c_str(), src_fn.c_str(),
                reduce_type.c_str()
            );
        }

        // import any preamble as well, as it may be necessary for the rules.
        // hmm.. too bad we can't scope this and/or figure out if it's necessary
        // to import in the first place... (can we scope?)
        preamble += "\n" + from.preamble;

        // these are the names of the products whose rules (and elements)
        // we need to import:
        std::list<std::string> all_wanted;
        std::set<std::string> already_wanted;
        std::string import_as;
        if(pname.length()) {
            import_as = pname;
            all_wanted.push_back(pname);
            already_wanted.insert(pname);
        } else {
            // no particular production specified.  import the
            // default production, but use the base name of the
            // fpl file to refer to it
            import_as = from.inp.base_name();
            std::string def_prd(rules[0].product());
            all_wanted.push_back(def_prd);
            already_wanted.insert(def_prd);
        }

        // NOTE we're assuming here that we can append to a list
        // while iterating it and have things dtrt.  I believe
        // this is a reasonable thing to ask from std::list,
        // but have no documentation/spec saying it's ok.
        for(auto wanted : all_wanted) {
            // NOTE:  no scoping currently.  so rule names can
            // collide and cause mayhem... hmm
            auto strl  = from.rules_for_product.lower_bound(wanted);
            auto endrl = from.rules_for_product.upper_bound(wanted);
            if(strl == endrl) {
                fail("No rule for '%s' in %s\n",
                    wanted.c_str(), src_fn.c_str()
                );
            }
            for(auto rit = strl; rit != endrl; ++rit) {
                ProductionRule rule = from.rules[rit->second];
                push_rule(rule);

                // any rules needed to generate the rule we just pushed are
                // also relevant:
                for(int stepi = 0; stepi < rule.num_steps(); ++stepi) {
                     const ProdExpr *step = rule.step(stepi);
                     if(step && !step->is_terminal()) {
                         if(!already_wanted.count(step->production_name())) {
                             all_wanted.push_back(step->production_name());
                             already_wanted.insert(step->production_name());
                         }
                     }
                }
            }
        }

        return import_as;
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
            out += "fprintf(stderr, \"%s\", base_parser.to_str().c_str());\n";
            if(op.single_step) {
                out += "getchar();\n";
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
        out += ") {\n";
        out += "// " + rule.to_str() + "\n";
        out += rule.code().format(false);
        out += "\n}\n";
        // restore line number after end of function so that
        // compiler warnings about stuff like lack of return
        // value show the line in the fpl source (which is
        // where the fpl author has to fix it).
        // (actually, it'll show the line after... calling it
        // good enough for now)
        out += "\n#$LINE\n";
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
        if(opts.debug) {
            out += "    using namespace std;\n";
        }
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
                   ", " + el_id
                 + ", " + std::to_string(expr.min_times)
                 + ", " + std::to_string(expr.max_times)
                 + ", &" + state_fn(next_state, true);
        } else {
            return expr.gexpr.nonterm_id_str()
                 + ", " + std::to_string(expr.min_times)
                 + ", " + std::to_string(expr.max_times)
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

    std::string code_for_shift(const lr_set &state, const ProdExpr *right_of_dot) {
        std::string out;

        // new way, handling repetition/optionals:
// XXX what if there's more than one optional item to the right of the dot?
        // - if right_of_dot is optional:
        //   - ....

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

/*
        out += "    // transition ID: " + transition_id(
            *right_of_dot, lr_goto(state, right_of_dot->gexpr)
        ) + "\n";
 */

        return out;
    }

    std::string code_for_state(const Options &opts, const lr_set &state) {
        std::string out;
        std::string sfn = state_fn(state);

        out += "//\n";
        out += state.to_str(this, "// ");
        out += "//\n";
        out += "void " + sfn + "() {\n";
        out += debug_single_step_code(opts, state);

        out += "    if(0) {\n"; // now everything past this can be "else if"

        //   NOTE:  if there's a shift/reduce conflict, we will resolve it:
        //      - first by longest match (i.e. shift instead of reducing).
        //        example:  if() ...  vs if() ... else ...;
        //        if() .. else is longer so we shift.
        //      - next by operator precedence.. XXX implement
        //   ... I think we still want to report it.  or do we?
        //

        lr_item reduce_item;
        std::map<int, int> transition; // grammar element id -> state number
        std::map<int, lr_item> item_for_el_id;
        std::vector<lr_item> optionals;
        for(lr_item item : state.iterable_items()) {
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
                        // no problem, no need to generate another copy
                        // of the same case - just move on to the next item.
                        continue;
                    } else {
                        // ... shift/shift conflict, which makes no sense,
                        // and afaict means there's a bug someplace:
                        other_conflict(item, item_for_el_id[existing->first]);
                    }
                }

                out += code_for_shift(state, right_of_dot);

                if(right_of_dot->is_optional())
                    optionals.push_back(item);

                transition[el_id] = state_index[next_state.id()];
                item_for_el_id[el_id] = item;
            }
        }

        // at this point, we can't really handle grammars where there's
        // more than one possible optional thing in a given state.  in
        // theory, I think we could determine which optional thing to
        // shift based on lookahead or some other hints, but for now,
        // if this happens, we just give a warning and plow on.
        if(optionals.size() > 1) {
            std::string which;
            for(auto opt : optionals) {
                which += "    " + opt.to_str(this) + "\n";
            }
            warn("Ambiguity in %s:\n%s\n", sfn.c_str(), which.c_str());
        }

        out += "} else {\n";

        if(reduce_item) {
            out += "    fprintf(stderr, \"" + sfn + " is going to reduce to a %s\\n\", \"" + c_str_escape(reduce_item.to_str(this)) + "\");\n";
            out += production_code(opts, state, reduce_item.rule);
            out += "    fprintf(stderr, \"%i items on stack after reduce\\n\", base_parser.lr_stack_size());\n";
//        } else if(state.id() != states[0].id()) { 
        } else {
            // XXX cooler would be to reduce this to an error.
            // also much cooler would be to have a comprehensible message.
            // errf so state.to_str can of course have all kinds of embedded
            // quotes and stuff which breaks source code strings.
            //out += "    base_parser.error(\"unexpected input.  here's where we think we were:\\n\"\n";
            //out += "    " + state.to_str(this, "    \"        ", "\"\n") + "\n";
            //out += "    \"\\n\");\n";
            // XXX bug - we can end up looping forever here because
            // we eat no input.....
            out += "    base_parser.error(\"unexpected input in " + sfn + "\\n\");\n";
/*
out += "    exit(1);\n";
        } else {
// XXX this doesn't quite work, because 
            // state 0 is the entry point, so it'll be at the bottom of the
            // stack (and possibly elsewhere).  in fpl, we don't necessarily
            // parse to end of file.  therefore, for fpl, this is where we'll
            // handle accepting.
            out += "    const Product *res = base_parser.result();\n";
            for(auto gl : goal) {
                out += "    if(res && res->grammar_element_id() == _" + gl + ") {\n";
                out += "        fprintf(stderr, \"accepting with stack size %i and a " + gl + " for a result\\n\", base_parser.lr_stack_size());\n";
                out += "        base_parser.lr_accept();\n";
                out += "    }\n";
            }
 */
        }

        out += "}\n"; // end of reduce/accept section

        out += "}\n"; // end of state_ function

        return out;
    }

    // reformats the generated code to fix indents and what have you.
    // this is fairly rough but does result in more or less readable
    // code for most cases.
    std::string reformat_code(const std::string &code, const std::string &fn) {
        const int chars_per_indent = 4;
        int indent = 0;
        int newlines = 0;
        int cur_line = 1;
        bool preprocessor_mode = false;

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
                    if(code[inp] == '\n') {
                        newlines++;
                        // (.. and track the current line number)
                        cur_line++;
                    }
                    inp++;
                }
                if(inp < code_length)
                    inp--;
                preprocessor_mode = false;
                continue;
            } else if(code[inp] == '#') {
                preprocessor_mode = true;
            } else if(code[inp] == '$' && preprocessor_mode) {
                // if we see $LINE in anything which looks like
                // a preprocessor directive, expand it out into
                // a #line directive with the current file and line.
                // this lets the code generator reset the line number
                // after issuing its own #line directives, which it
                // does for code imported into the fpl etc.
                // (as with everything here, it's not perfect and can
                // be broken by (eg) a '#' embedded in a string or
                // whatever, but for the moment I'm not worrying)
                // (IRL if this ever generates jest code we'll have
                // a jest directive to restore the line/file position
                // or such.  probably actually track both actual line/file
                // position, and "original" line/position..)
                if(code.compare(inp + 1, 4, "LINE") == 0) {
                    output += "line " + std::to_string(cur_line + 1) 
                            + " \"" + fn + "\"";
                    inp += 5;
                    // add newline after in a way which won't mess up
                    // the count:
                    newlines++;
                    continue;
                }
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
        std::string is_terminal_guts;

        out += "typedef enum {\n";
        for(int el_id = 0; el_id < elements.size(); ++el_id) {
            is_terminal_guts += "case " + std::to_string(el_id);
            if(elements[el_id].type == GrammarElement::NONTERM_PRODUCTION) {
                std::string name(elements[el_id].to_str());
                // prefix with underscore as a hack to avoid keyword
                // collisions:
                out += "_" + name;
                nonterm_str_guts += "case " + std::to_string(el_id);
                nonterm_str_guts += ": return \"" + name + "\";\n";
                if(el_id == 0) {
                    // element ID 0 is a special case and counts as terminal:
                    is_terminal_guts += ": return true;\n";
                } else {
                    is_terminal_guts += ": return false;\n";
                }
            } else {
                // include terminals as comments.  we don't need them
                // in the enum (and how would they be named, anyway?),
                // but it's nice to be able to see all the grammar
                // elements in one place:
                out += "// " + elements[el_id].to_str();
                is_terminal_guts += ": return true;\n";
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
        out += "    return std::to_string(id) + \" is not a nonterm.\";\n";
        out += "}\n\n";

        out += "static bool is_terminal(int id) {\n";
        out += "    switch(id) {\n";
        out += is_terminal_guts;
        out += "    }\n";
        out += "    fprintf(stderr, \"invalid terminal id: %i\\n\", id);\n";
        out += "    return false;\n";
        out += "}\n\n";
        return out;
    }

    std::string state_to_string() {
        std::string out("static std::string state_to_str(State st) {\n");
        out += "if(!st) return \"NULL\";";
        for(auto st: states) {
            out += "if(&" + state_fn(st, true) + " == st) ";
            out += "return \"" + state_fn(st) + "\";\n";
        }
        out += "    return \"<not a state>\";\n";
        out += "}\n";

        return out;
    }

    std::string parser_class_name() {
        std::string base;
        for(auto chr : inp.base_name()) {
            switch(chr) {
                case '-':
                    base += '_';
                    break;
                default:
                    base += chr;
                    break;
            }
        }

        return base + "_parser";
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

    void generate_states(const std::list<std::string> &wanted) {

        if(rules.empty()) {
            fail("No rules found\n");
        }


        lr_set entry_set;
        for(auto entry_prod : wanted) {
            auto strl  = rules_for_product.lower_bound(entry_prod);
            auto endrl = rules_for_product.upper_bound(entry_prod);
            if(strl == endrl) {
                fail("Can't find entry rule for '%s'\n", entry_prod.c_str());
            }
            for(auto rit = strl; rit != endrl; ++rit) {
                // entry_set.add_item(lr_item(rit->second, 0));
                add_expanded(entry_set, lr_item(rit->second, 0));
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
                if(state.size() > 0) {
                    if(state_index.find(state.id()) == no_set) {
                        add_state(state);
                    }
                }
            }
        }
    }

    std::string generate_code(const Options &opts) {
        const std::string parser_class = parser_class_name();
        // "base" is probably the wrong term..
        const std::string base_parser_class = "FPLBaseParser<" + parser_class + ">";

        goal = opts.entry_points;
        if(goal.empty()) {
            // no particular goal products specified, so we default
            // to whatever the first rule produces:
            goal.push_back(rules[0].product());
        }

        //generate_states(opts.entry_points);
        generate_states(goal);

        std::string out;
        out += "#include <string>\n";
        // preamble has to come before the fpl headers
        // because (to my surpise) things like to_string
        // functions (called by the template) have to be
        // declared before the template (not just before
        // template instantiation.. ?) (did I miss something?)
        out += preamble;
        out += "#line " + std::to_string(__LINE__) + " \"" + __FILE__ + "\"\n";
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

        report_unused_rules();

        return reformat_code(out, opts.output_fn);
    }

    void report_unused_rules() {
        bool used[rules.size()];
        for(int rind = 0; rind < rules.size(); rind++) {
            used[rind] = false;
        }
        for(lr_set state : states) {
            for(lr_item item : state.iterable_items()) {
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


/*
 Input is:

   <exprs or productions to match> -> <production name> +{ <code> }+
                    or
   <exprs to match> -> <production name> ;

Also, comments.  Let's use # just cuz.

 */
// XXX Coment above is outdated and probably in the wrong place

void fpl2cc(const Options &opts) {
    fpl_reader inp(opts.src_fpl, fail);

    // parse the input file into a set of productions:
    Productions productions(inp);

    std::string output = productions.generate_code(opts);

    // uhh... this is easy, if hokey:
    if(opts.out) {
        fprintf(opts.out, "%s\n", output.c_str());
    } else {
        fail("no open output - fail\n");
    }
}

void usage() {
    fprintf(stderr,
        "\nUsage:    fpl2cc [options] <source> [target]\n\n"
        "If no [target] is specified, prints to stdout.\n\n"
    );
}

int main(int argc, const char** argv) {
    Options opts(argc, argv);

    if(opts.errors.size()) {
        for(auto fail : opts.errors) {
            fprintf(stderr, "%s\n", fail.c_str());
        }
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


