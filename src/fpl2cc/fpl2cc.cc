#include <cassert>
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
#include "util/src_location.h"

enum ExitVal {
    OK = 0,
    FAIL = 1,
    BAD_ARGS = 2
};


void fail(const std::string &msg) {
    if(msg[msg.length() - 1] != '\n') {
        fprintf(stderr, "%s\n", msg.c_str());
    } else {
        fprintf(stderr, "%s", msg.c_str());
    }

    exit(ExitVal::FAIL);
}

static int num_warnings = 0;
void warn(const std::string &msg) {
    // indent warnings so that errors stand out:
    if(msg[msg.length() - 1] != '\n') {
        fprintf(stderr, "       %s\n", msg.c_str());
    } else {
        fprintf(stderr, "       %s", msg.c_str());
    }

    num_warnings++;
}

inline std::string to_str(bool b) {
    if(b) return "true";
    return "false";
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
   - double-quoted string ("xxx") - match text
   - regular expression within slashes (eg /0x[0-9a-fA-F]+/)
   - names of other productions as plain text (no spaces)

  Expressions may be followed (no space) by one of *, +, or ?
  to mean 0-or-more, 1-or-more, or 0-or-1 respectively.

  Comments start with "#" and go to the end of the line.

  All input tokens are separated by space.

  Code blocks:

  Code blocks are enclosed in +{ }+.

  Code blocks access their corresponding expressions via pseudo-positional
  argument variables named arg[0..x] (so the first is called arg0, second
  arg1, etc).  The type of each argument variable depends on the types
  and (possible) repetitions of the expressions:
    - quoted string      -> std::string
    - regular expression -> std::smatch  XXX this is lies presently
    - production name    -> Jest::Member

  Code blocks should use a normal "return" statement to return a pointer
  to a Jest::Member.

 */

/*
 TODO/fix

  - error handling and messaging is _terrible_ right now (with
    the default, anyway)
  - bug:  if everything in your fpl grammar is optional, it generates
    some kind of infinite loop, I guess looking for nothing.
    eg:
      foo* -> done ;
      'a' -> foo ;
  - get jest in here :D
  - lots of stuff is misnamed:
    - "num_args" should be count or size or something sensible
    - @comment_style and @separator - maybe just have @elider and
      allow multiple?  default also doesn't have to be space.
      lots of modern stuff puts space in the grammar.
  - if a generated fpl encounters an unexpected anything, it stops
    parsing (by design).  this could be used for incremental parsing
    in cases where you are parsing a buffer as it's being filled
    (such as parsing network input or even just from the command line).
    the current buffering framework doesn't allow that, though.

  - ~scan_function.  regexes suck for quoted strings and the like
    (need non-greedy match, escaped quotes, etc).  maybe have a
    standard lib of scan functions for things like quoted strings
    with escaping or whatever.
  - parens to join steps for purposes of repetition, so as to easily
    support old-style languages which don't allow trailing commas
    and such.  eg allow:
       foo (',' foo)* ->  foo_list ;
  - precedence.  maybe an @prec (... )? or ^other_rule?

  - timings, so we can see if this or that is faster.
  o document the fpl (see docs dir)

 */

struct Options {
    std::string src_fpl;
    FILE *out;
    std::string output_fn;

    bool debug;
    std::list<std::string> entry_points;
    bool generate_main;
    bool help;
    bool single_step;
    bool dump_states;

    std::list<std::string> errors;
    inline void error(const std::string &errst) {
        errors.push_back(errst);
    }

    // janky, but good enough:
    Options(int argc, const char* const* argv) :
        out(stdout),
        output_fn("«stdout»"),
        debug(false),
        generate_main(false),
        help(false),
        single_step(false),
        dump_states(false)
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
                    } else if(opt == "debug-dump-states") {
                        dump_states = true;
                    } else if(opt == "entry") {
                        // specifies an entry rule (i.e. a parsing
                        // starting point)
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--entry requires a value.");
                        entry_points.push_back(std::string(val));
                    } else if(opt == "generate-main") {
                        generate_main = true;
                    } else if(opt == "help") {
                        help = true;
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

    std::string to_str() const {
        std::string out;

        out += "    src: " + src_fpl + "\n";
        out += "    output: " + output_fn + "\n";
        out += "    generate_main: " + ::to_str(generate_main) + "\n";
        out += "    debug: " + ::to_str(debug) + "\n";
        out += "    single_step: " + ::to_str(single_step) + "\n";
        if(entry_points.size() > 0) {
            out += "    entry points:\n";
            for(auto entry : entry_points) {
                out += "        " + entry + "\n";
            }
        }

        return out;
    }
};

struct CodeBlock {
    std::string source_file;
    int line;
    std::string code;

    CodeBlock() : line(0) { }

    CodeBlock(
        const std::string &cd,
        const std::string &file = CALLER_FILE(),
        int ln = CALLER_LINE()
    ) : 
        source_file(file),
        line(ln),
        code(cd) {
    }

    operator bool() const {
        return code.length();
    }

    // for backward compatibility so I can just swap CodeBlocks
    // in where I used to use std::string
    CodeBlock &operator += (const std::string &more_code) {
        code += more_code;
        return *this;
    }

    CodeBlock &operator += (const CodeBlock &more_code) {
        return *this += more_code.format();
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

    // As format() but wrapped in { } so that if the code is
    // declares a local variable or such, that variable will
    // be scoped.
    // This will probably only work with code fragments.
    std::string format_scoped() const {
        return "\n{\n" + format() + "\n}\n";
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

    static const char *Type_to_str(Type t) {
        static const char *strs[] = {
            "NONE",
            "TERM_EXACT",
            "TERM_REGEX",
            "NONTERM_PRODUCTION",
        };
        if(t > NONE && t <= NONTERM_PRODUCTION) {
            return strs[t];
        }
        return "invalid GrammarElement::Type";
    }

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
        return "error: " + expr + " is not a nonterminal";
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
    std::string file;
    int         line;

public:

    ProductionRule(const fpl_reader &rdr, size_t at_byte) :
        file(rdr.filename()),
        line(rdr.line_number(at_byte))
    {
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

    int line_number() const {
        return line;
    }

    std::string filename() const {
        return file;
    }

    std::string location() const {
        return filename() + " line " + std::to_string(line_number());
    }

    // XXX maybe rename "code" to "action"
    CodeBlock default_code() const {
        // start the code block with a comment referring to this
        // line in this source file (fpl2cc.cc), to reduce puzzlement
        // about where this mixed-generated code comes from:
        std::string code("// " THIS_LINE "\n");

        int first_single_nonterm = -1;
        int num_single_nonterms = 0;
        for(int sti = 0; sti < steps.size(); sti++) {
            if(!steps[sti].is_terminal() && steps[sti].is_single()) {
                num_single_nonterms++;
                if(first_single_nonterm < 0)
                    first_single_nonterm = sti;
            }
        }

        if(num_single_nonterms >= 1) {
            if(num_single_nonterms != 1) {
                warn(stringformat(
                    "default for nontrivial rule {} probably won't dtrt\n",
                    to_str()
                ));
            }

            // if there's exactly one nonterminal, I think returning it
            // is a sane-enough default.  this allows for aliasing
            // (where you have just foo -> bar) as well as doing the
            // right thing in some simple cases like parenthesized
            // expressions.
            // If there's more than one nonterm, we still need to return
            // something, so just do the first one (we warned in this case,
            // above).
            code = "return arg_" + std::to_string(first_single_nonterm) + ";";
        } else {
            // this rule has terminals only so they'll come in as strings
            // or stack slices full of strings.
            // maybe the reduce type has a constructor which can do
            // something sane with a string?
            if(steps.size() > 1) {
                warn(stringformat(
                    "default for rule {} probably won't dtrt\n",
                    to_str()
                ));
            }
            code += "std::string out;\n";
            code += "for(int ind = 0; ind < args.num_args; ++ind) {\n";
            code += "    out += args[ind].product.term_str();\n";
            code += "    if(ind < args.num_args - 1) out += \" \";\n";
            code += "}\n";
            code += "return out;";
        }

        return CodeBlock(code, filename(), line_number());

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

    CodeBlock code(const CodeBlock &def) const {
        if(code_for_rule) {
            return code_for_rule;
        } else if(def) {
            // (caller provided code, so return that)
            return def;
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
    CodeBlock default_action;
    CodeBlock post_parse;
    CodeBlock post_reduce;
    CodeBlock separator_code;
    std::list<CodeBlock> comment_code;
    bool default_main;
    std::string preamble;
    std::list<CodeBlock> parser_members;
    std::list<std::string> goal; // goal is any of these

    std::vector<ProductionRule>     rules;
    std::multimap<std::string, int> rules_for_product; // product -> rule ind

    std::vector<GrammarElement>     elements;
    std::map<GrammarElement, int>   element_index;

    struct lr_set;
    std::vector<lr_set> states;
    std::map<std::string, int> state_index; // keyed by set id

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
        // such that earlier items (in the fpl source)
        // are ordered earlier.
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

        // returns true if this state only reduces
        bool reduce_only(const Productions *prods) const {
            for(lr_item item : iterable_items()) {
                const ProductionRule &rule = prods->rules[item.rule];
                if(rule.step(item.position)) {
                    // this item is not at the end of the rule,
                    // so it's some kind of shift, so this is
                    // not pure reduce.
                    return false;
                }
            }
            return true;
        }

        std::string to_str(
            const Productions *prds,
            const std::string &line_prefix = "",
            const std::string &line_suffix = "\n"
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
                fail(stringformat(
                    "error in {}: Nothing produces «{}»\n",
                    rule.location(), pname
                ));
            }

            for(auto rit = strl; rit != endrl; ++rit) {
                // (these are always position 0)
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
                //   - the "*" and "?" cases:  since the expression is
                //     optional, we do need to consider what's after
                //     it as another possible start to a given match.
                //   - the "+" case is no different from the default
                //     here, since this deals only with the first
                //     successor symbol.
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
    // related items to cover optionalness.
    // (repetition is handled in the goto)
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

    // "goto" operation from page 224 Aho, Sethi and Ullman,
    // augmented to allow repetition (eg from * and + operators).
    // given a current state and a lookahead item, returns
    // the next state (i.e. the set of items which might appear
    // next)
    lr_set lr_goto(const lr_set &in, const GrammarElement &sym) {
        lr_set set;
        for(auto item : in.iterable_items()) {
            const ProdExpr *step = rules[item.rule].step(item.position);
            if(step && step->matches(sym)) {

                add_expanded(set, lr_item(item.rule, item.position + 1));
                if(step->max_times > 1) {
                    // ...if it can be repeated:
                    add_expanded(set, lr_item(item.rule, item.position));
                }
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

    Productions(fpl_reader &src) : inp(src), default_main(false) {
        // element 0 is a null element and can be used to
        // indicate missing/uninitialized elements or such.
        // we count it as a nonterminal so that it can be
        // accessed via the enum etc.
        record_element(
            GrammarElement("_fpl_null", GrammarElement::NONTERM_PRODUCTION)
        );

        // default the reduce type to string.
        // this will (mostly?) work with defaults if
        // you just want to sketch out a grammar and
        // not have to specify any particular code.
        reduce_type = "std::string";

        parse_fpl();
    }


    // expects/scans a +{ }+ code block for the named directive.
    // the named directive is essentially for error reporting.
    inline CodeBlock code_for_directive(const std::string &dir) {
        CodeBlock code = read_code(inp);
        if(!code) {
            fail(stringformat("expected a code block for @{}\n", dir));
        }
        return code;
    }

    void add_comment_style(const std::string &style) {
        // comment style files are relative to this source:
        fs::path fn(__FILE__);
        fn.replace_filename("comment/" + style + ".inc");

        comment_code.push_back(CodeBlock(load_file(fn), fn, 1));
    }

    void parse_directive(const std::string &dir) {
        if(dir == "comment_style") {
            std::string style = inp.read_re("\\s*(.+)\\s*")[1];
            if(!style.length()) {
                warn("no comment style specified");
            } else {
                add_comment_style(style);
            }
        } else if(dir == "default_action") {
            default_action = code_for_directive(dir);
        } else if(dir == "default_main") {
            default_main = true;
        } else if(dir == "internal") {
            // a code block which goes in the "private" part
            // of the parser class itself.
            // XXX wtb better name?  in the jest version of this,
            // it won't necessarily be internal
            if(CodeBlock mem = code_for_directive(dir)) {
                parser_members.push_back(mem);
            }
        } else if(dir == "post_parse") {
            post_parse = code_for_directive(dir);
        } else if(dir == "post_reduce") {
            post_reduce = code_for_directive(dir);
        } else if(dir == "produces") {
            reduce_type = inp.read_re("\\s*(.+)\\s*")[1];
        } else if(dir == "separator") {
            if(separator_code)
                warn("@separator overrides existing separator code\n");
            separator_code = code_for_directive(dir);
        } else {
            fail(stringformat("Unknown directive: '{}'\n", dir));
        }
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
        switch(src.peek()) {
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

    static inline std::string read_directive(fpl_reader &src) {
        return src.read_re("([A-Za-z][A-Za-z0-9_]+)\\s*")[1];
    }

    // reads the specified file and returns its contents as a string.
    // calls fail() (and returns enpty string) on error.
    // this should be some kind of standard library thingo.
    static std::string load_file(const std::string &infn) {

        // blatant copypasta from fpl_reader
        std::ifstream in(infn);
        if(!in.is_open()) {
            fail(stringformat("can't open '{}': {}\n", infn, strerror(errno)));
            return "";
        } 

        in.seekg(0, std::ios::end);   
        size_t filesize = in.tellg();
        in.seekg(0, std::ios::beg);

        // .. except... apparently the appended '\0' in the fpl_reader
        // version ends up being spurous and extra end makes an
        // embedded newline in the string.  d'oh.
        char buf[filesize];
        in.read(buf, filesize);
        std::string out(buf, filesize);

        return out;
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

        std::string filename(src.parse_string());
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
            src.eat_separator();

            std::string expr_str;
            GrammarElement::Type type = GrammarElement::Type::NONE;

            const utf8_byte inch = src.peek();
            switch(inch) {
                case '\0':
                    done = true;
                    break; // EOF
                case '#':
                    // line comment - just skip
                    src.read_line();
                    break;
                case '"':
                case '\'':
                    expr_str = src.parse_string();
                    type     = GrammarElement::Type::TERM_EXACT;
                    break;
                case '/':
                    expr_str = src.parse_string();
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
                case '}':
                    // this can happen, especially if there's a '}+'
                    // embedded in a code block.
                    if(src.read_byte_equalling('+'))
                        src.error("stray '}+'.  perhaps there's }+ embedded in a code block");
                    else
                        src.error("unmatched '}'");
                    break;
                default:
                    // should be the name of a production.
                    expr_str = read_production_name(src);
                    if(!expr_str.length()) {
                        src.error(stringformat(
                            "expected production name for rule '{}'\n"
                            " starting at {}",
                            rule.to_str(), rule.location()
                        ));
                    }
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
                    // XXX show the type in some non-numeric way here
                    src.error(stringformat(
                        "expected type {} = {} but got .. nothing?\n",
                        GrammarElement::Type_to_str(type), type
                    ));
                }
            }
        } while(!(done || src.eof()));

        return num_read;
    }

    static CodeBlock read_code(fpl_reader &src) {
         // code is within "+{" "}+" brackets.
         // we don't know anything about the grammar of the code
         // within the brackets (presently), so you will derail
         // it if you put unmatched +{ or }+ in a comment or
         // string or whatever.  sorry.  try not to do that.
         // matched +{ }+ it will handle ok.
         src.eat_separator();

         size_t start = src.current_position();
         if(!src.read_exact_match("+{")) {
             src.error(stringformat(
                 "expected start of code (\"+{{\") but got «{}»",
                 src.debug_peek()
             ));
             return CodeBlock();
         }

         std::string code_str;
         bool found_terminator = false;
         char byte_in;
         while(byte_in = src.read_byte()) {
             if(byte_in == '}') {
                 if(src.peek() == '+') {
                     src.read_byte();
                     found_terminator = true;
                     break;
                 }
             }
             code_str += byte_in;
         }

         if(!found_terminator) {
             src.error(stringformat(
                 "Expected code block terminator ('}}+') but got byte 0x{}",
                 to_hex(byte_in)
             ));
         }

         return CodeBlock(code_str, src.filename(), src.line_number(start));
    }

    void parse_fpl() {
        do {
            inp.eat_separator();
            if(inp.peek() == '#') {
                inp.read_line();
            } else if(inp.peek() == '+') {
                // inlined/general code - goes at the top of the generated
                // code.  Use to define types or whatever.
                CodeBlock code(read_code(inp));
                if(code) {
                    add_preamble(code);
                }
            } else if(inp.read_byte_equalling('@')) {
                std::string directive = read_directive(inp);
                parse_directive(directive);
            } else if(inp.read_byte_equalling('}')) {
                // likely what happened is someone put a }+ inside
                // a code block.  anyway a floating end brace is 
                // wrong..
                inp.error("unmatched '}'\n");
            } else {
                ProductionRule rule(inp, inp.current_position());
                if(read_expressions(inp, rule)) {
                    // .. we've read the expressions/steps leading to
                    // the production (including the "->").
                    // read what the expressions above produce:
                    inp.eat_separator();
                    rule.product(inp.read_to_separator());
                    if(rule.product().length() <= 0) {
                        fail(stringformat(
                            "missing production name at {}\n",
                            rule.location()
                        ));
                    }

                    inp.eat_separator();

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
            fail(stringformat(
                "Incompatible reduce type '{}' in {} (expected {})\n",
                from.reduce_type, src_fn, reduce_type
            ));
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
                fail(stringformat("No rule for '{}' in {}\n", wanted, src_fn));
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

    #define rule_meta_str(mem) \
        std::string("static const char *" #mem "() {\n") +\
            "return \"" + rule.mem() + "\";\n" \
        "}\n"
    #define rule_meta_int(mem) \
        std::string("static int " #mem "() {\n") +\
            "return " + std::to_string(rule.mem()) + ";\n" \
        "}\n"
    std::string rule_metadata(int rule_ind) {
        const ProductionRule &rule = rules[rule_ind];
        std::string out;
        out += "struct {\n";
        out += rule_meta_str(product);
        out += rule_meta_int(num_steps);
        out += rule_meta_int(line_number);
        out += rule_meta_str(filename);
        out += rule_meta_str(location);
        out += "} this_rule;\n";
        return out;
    }
    #undef rule_meta_int
    #undef rule_meta_str

    std::string code_for_rule(const Options &opts, int rule_ind) {
        const ProductionRule &rule = rules[rule_ind];
        std::string out;

        std::string rule_name = rule_fn(rule_ind);
        out += reduce_type + " " + rule_name + "(";

        // "simple" parameters:
        // If the expression for the step has a min/max times of
        // anything other than exactly 1 (is_single()), we pass
        // it as a stack slice.
        // Otherwise, if it's a terminal, pass it as a string,
        // or (if it's the result of a reduce) as whatever the reduce
        // type is.  This is complicated for the code generator,
        // but simplifies life for the fpl author, especially
        // for trivial cases.
        for(int stind = 0; stind < rule.num_steps(); stind++) {
            std::string arg_name = "arg_" + std::to_string(stind);
            const ProdExpr *expr = rule.step(stind);
            if(!expr) {
                fail(stringformat("Bug: no expression for {} in {}",
                    arg_name, rule.to_str()
                ));
            } else if(!expr->is_single()) {
                // the argument is either optional or can repeat or
                // both, so we can't pass it simply.  so pass it
                // as a slice:
                out += "const FPLBP::StackSlice &";
            } else if(expr->is_terminal()) {
                out += "std::string ";
            } else {
                out += reduce_type + " ";
            }
            out += arg_name;
            out += ", ";
        }

        // last parameter is the slice of the stack with
        // everything we're popping.  this lets the fpl author
        // get things like the line number for a given argument
        // or whatever.  (actully, the "simple" positional
        // parameters below can be implemented via this,
        // and in the jest version might be).  It's last only
        // because that simplifies the generating code
        out += "const FPLBP::StackSlice &args";

        out += ") {\n";
        out += "// " + rule.to_str() + "\n";
        out += rule_metadata(rule_ind);
        if(opts.debug) {
            out += "fprintf(stderr, \"reducing by " + rule_name + "\\n\");\n";
        }
        out += rule.code(default_action).format(false);
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

        //   For the moment, production code will need to be written
        // in c++.  Authors of the production code should provide
        // a matching header which defines their product type,
        // as well as including anything else they're going to need
        //   In the spirit of jest, we should provide as much visibility
        // into the state of the compiler as possible.  Obviously we
        // need to pass in the generated products.  Can we show the
        // whole stack?
        // pass, named:
        //  - const &parser (do this?)
        //  - arguments from the stack:
        //    - one argument per element (step) in the rule.
        //      each such argument is a slice of the stack,
        //      holding where in the stack the relevant entries
        //      are, and how many there are.

        // in c++ the order in which arguments for a function are
        // evaluated is not deterministic (?!), so we need to make some
        // temps for the arguments.  We go backward because the item
        // at the top of the stack will be the last argument, and
        // the item one down from the top of the stack the second
        // to last, etc.
        out += "int frame_start = base_parser.lr_top();\n";
        out += "int pos = frame_start;\n"; // (pos gets updated as we go)
        for(int stind = rule.num_steps() - 1; stind >= 0; --stind) {
            const ProdExpr *expr = rule.step(stind);
            std::string argname = "arg_" + std::to_string(stind);
            if(!expr) {
                fail(stringformat(
                    "Bug: no expression for step {} in {}",
                    stind, rule.to_str()
                ));
            } else {
                std::string eid_str = std::to_string(element_index[expr->gexpr]);
                std::string max_str = std::to_string(expr->max_times);
                out += "FPLBP::StackSlice " + argname
                     + "(base_parser, " + eid_str + ", " + max_str + ", pos);\n";
            }
        }

        // now one slice for all the arguments.  to rule them all.
        out += "FPLBP::StackSlice args(base_parser, pos + 1, frame_start - pos);\n";

        // generates the call to the reduction rule:
        out += "    " + reduce_type + " result = " + rule_fn(rule_ind) + "(";
        for(int stind = 0; stind < rule.num_steps(); stind++) {
            const ProdExpr *expr = rule.step(stind);
            std::string argname = "arg_" + std::to_string(stind);
            out += argname;
            if(expr->is_single()) {
                if(expr->is_terminal()) {
                    out += ".term_str()";
                } else {
                    out += ".val()";
                }
            }
            out += ", ";
        }
        out += " args);\n";

        // we've called the reduction rule.  There might be something
        // we're supposed to do with the result:
        if(post_reduce) {
            out += "\n{\n" + post_reduce.format() + "\n}\n";
        }

        // this is what actually pops the stack. note we pop after
        // the reduce (mainly to minimize moves, but also so the
        // stack is more intact for error/bug analysis)
        out += "base_parser.lr_top(pos);\n";

        if(opts.debug) {
            out += "fprintf(stderr, \"... finished reducing\\n\");\n";
        }

        out += "    base_parser.set_product(Product(result, "
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
        warn(stringformat(
            "reduce/reduce conflict:\n    {} line {}\n vs {} at {}\n",
            rules[r1].to_str().c_str(), rules[r1].location().c_str(),
            rules[r2].to_str().c_str(), rules[r2].location().c_str()
        ));
    }

    // reports what is probably a shift/shift conflict,
    // which would probably only happen due to a bug in
    // this program...
    void other_conflict(lr_item item1, lr_item item2) {
        warn(stringformat(
           "conflict:\n    {}\n vs {}\n",
           item1.to_str(this).c_str(), item2.to_str(this).c_str()
        ));
    }

    std::string code_for_shift(const lr_set &state, const ProdExpr *right_of_dot) {
        std::string out;

        const GrammarElement::Type type = right_of_dot->type();
        switch(type) {
            // shifts
            case GrammarElement::TERM_EXACT:
                out += "} else if(base_parser.shift_exact(separator_length,";
                break;
            case GrammarElement::TERM_REGEX:
                out += "} else if(base_parser.shift_re(separator_length,";
                break;
            case GrammarElement::NONTERM_PRODUCTION:
                out += "} else if(base_parser.shift_nonterm(";
                break;
            case GrammarElement::NONE:
                // .. this pretty much implies a bug in fpl2cc:
                fail(stringformat(
                    "Missing/unknown grammar element (id: {} {})",
                    type, right_of_dot->to_str()
                ));
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
            warn(stringformat("Ambiguity in {}:\n{}\n", sfn, which));
        }

        out += "} else {\n";

        if(reduce_item) {
            //out += "    fprintf(stderr, \"" + sfn + " is going to reduce to a %s\\n\", \"" + c_str_escape(reduce_item.to_str(this)) + "\");\n";
            out += production_code(opts, state, reduce_item.rule);
            //out += "    fprintf(stderr, \"%i items on stack after reduce\\n\", base_parser.lr_stack_size());\n";
        } else {
            // since we want to be able to do partial parses, if we
            // don't see input we expect, it's not necessarily
            // an error - we might have parsed whatever was wanted
            // (potentially somewhere back in the stack) and would
            // just need to rewind the input to jsut after whatever
            // we wanted.  Of course, it still might be an error!
            // So the strategy is:  terminate parsing and let the 
            // caller (of the parser) decide what to do.
            out += "    base_parser.terminate();\n";
        }

        out += "}\n"; // end of reduce/accept section

        out += "}\n"; // end of state_ function

        return out;
    }

    // reformats the generated code to fix indents and what have you.
    // this is fairly rough but does result in more or less readable
    // code for most cases.
    // ... this should also be done in fpl.
    std::string reformat_code(const std::string &code, const std::string &fn) {
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
        out += "if(!st) return \"NULL\";\n";
        for(auto st: states) {
            out += "if(&" + state_fn(st, true) + " == st) ";
            out += "return \"" + state_fn(st) + "\";\n";
        }
        out += "    return \"<not a state>\";\n";
        out += "}\n";

        return out;
    }

    std::string is_goal() {
        std::string out("static bool is_goal(int id) {\n");
        out += "    switch(id) {\n";
        for(auto gstr : goal) {
            out += "        case NontermID::_" + gstr + ": return true;\n";
        }
        out += "        default: return false;\n";
        out += "    }\n";
        out += "    return false;\n";
        out += "}\n";
        return out;
    }

    /*
        separator_method returns a method which should return
        the number of bytes (possibly 0) to skip in order to
        elide whatever token separator(s) (such as spaces or
        comments) are at the *inp pointer passed.  The method
        will be composed from whatever's in @comment_style and
        @separator directives.  If no such directives are specified,
        default is space separation.
     */
    // "separator" might be a misnomer in all this.  it's more like
    // anything to elide before the tokenization code sees the input.
    // rename all the "separator" to "elide"?  (including directives)
    CodeBlock separator_method() {
        CodeBlock out(
            "static size_t separator_length(const utf8_byte *inp) {\n"
        );

        if(separator_code) {
            out += separator_code.format_scoped();
        } else {
            // default is space separation:
            // (maybe make this explicit?)
            out += CodeBlock(
                "if(size_t len = space_length(inp)) {\nreturn len;\n}\n"
            );
        }

        // Comments are effectively separators so code for them
        // gets tacked on here:
        for(auto comc : comment_code) {
            out += comc.format_scoped();
        }

        // if nothing returned a length yet, no separator
        out += "    return 0;";
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

    std::string default_main_code(const std::string &parser_class) {
        std::string out("\n\n");

        // the main() generated here is pretty much just a test stub.
        // if people want something fancier, they can make their own.
        out += "int main(int argc, const char **argv) {\n";
        out += "    if(argc < 2) {\n";
        // XXX this is weak;  make the reader able to read stdin
        // XXX oh I think we can now.  yay.
        out += "        fpl_reader::default_fail(\n";
        out += "            \"Please provide a source file name.\\n\"\n";
        out += "        );\n";
        out += "    }\n";
        // XXX this is also weak; handle more than one source
        out += "    fpl_reader inp(argv[1]);\n";
        out +=      parser_class + " parser(inp);\n";
        out += "    using namespace std;\n";
        out += "    auto result = parser.parse();\n";
        //out += "    printf(\" %s\\n\", to_string(result).c_str());\n";
        //out += "    fprintf(stderr, \"parser state:\\n%s\\n\", parser.to_str().c_str());\n";
        out += "    return parser.error_count()?-1:0;\n";
        out += "}\n\n";

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
                fail(stringformat(
                    "Can't find entry rule for '{}'\n", entry_prod
                ));
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

        if(rules.size() <= 0) {
            fail("No rules found\n");
        }

        const std::string parser_class = parser_class_name();
        // "base" is probably the wrong term..
        const std::string base_parser_class = "FPLBaseParser<" + parser_class + ">";

        std::string out("/*\n");
        out += opts.to_str();

        goal = opts.entry_points;
        if(goal.empty()) {
            // no particular goal products specified, so we default
            // to whatever the first rule produces:
            goal.push_back(rules[0].product());
            out += "default entry point: " + rules[0].product() + "\n";
        }
        out += " */\n\n";

        generate_states(goal);

        out += "#include <string>\n";

        // XXX make a separate @to_string so it doesn't have
        //  to be preamble. or predeclare it?
        // preamble has to come before the fpl headers
        // because (to my surpise) things like to_string
        // functions (called by the template) have to be
        // declared before the template (not just before
        // template instantiation.. ?) (did I miss something?)
        out += "\n// preamble:\n";
        out += preamble;
        out += "// end preamble\n\n";
        out += "#line " + std::to_string(__LINE__) + " \"" + __FILE__ + "\"\n";
        out += "#include \"fpl2cc/fpl_reader.h\"\n";
        out += "#include \"fpl2cc/fpl_base_parser.h\"\n";
        out += "\n\n";

        // parser_class is now really parser_impl_class or such...
        out += "class " + parser_class + " {\n";
        // Can't do the private declarations first because the
        // FPLBaseParser template needs to know about state methods
        // and such to determine the types it needs, but c++ won't
        // let you predeclare such methods.
        out += "public:\n";
        out += "    // state() and reduce_type tell FPLBaseParser what types to use\n";
        out += "    void state();\n"; // this must match state_x methods; XXX document/typedef?
        out += "    int error_count() { return base_parser.error_count(); }\n";
        out += "    static " + reduce_type + " reduce_type;\n"; // for telling the FPLBaseParser.. as above XXX document
        out += "private:\n";
        out += "    using FPLBP = " + base_parser_class + ";\n";
        out += "    using State = FPLBP::State;\n";
        out += "    using Product = FPLBP::Product;\n";
        out += "    using StackEntry = FPLBP::StackEntry;\n";
        out += "    FPLBP base_parser;\n";
        for(auto mem : parser_members) {
            out += mem.format();
        }
        out += "public:\n";

        out += nonterm_enum();
        out += state_to_string();
        out += is_goal();

        out += separator_method().format();

        for(int rnum = 0; rnum < rules.size(); rnum++) {
            out += code_for_rule(opts, rnum);
        }

        for(lr_set state : states) {
            out += code_for_state(opts, state).c_str();
        }

        out += "    " + parser_class + "(fpl_reader &src) : base_parser(src) { }\n";
        out += "    std::string to_str() { return base_parser.to_str(); }\n";
        out += "    inline " + reduce_type + " parse() {\n";
        out += "        auto result = base_parser.parse(*this);\n";
        if(post_parse) {
            out += post_parse.format();
        }
        out += "        return result;\n";
        out += "    };\n";

        out += "};\n"; // end of class

        if(opts.generate_main || default_main) {
            out += default_main_code(parser_class);
        }

        report_unused_rules();

        return reformat_code(out, opts.output_fn);
    }

    // debugging:
    void dump_states() {
        //for(lr_set state : states) {
        for(int stind = 0; stind < states.size(); stind++) {
            printf("state %i:\n%s\n",
                stind, states[stind].to_str(this, "    ").c_str()
            );
        }
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
                warn(stringformat(
                    "Rule producing {} on line {} is unused\n",
                    rule.product(), rule.line_number()
                ));
            }
        }
    }
};

// returns an exit()-appropriate status (i.e. 0 on success)
ExitVal fpl2cc(const Options &opts) {
    fpl_reader inp(opts.src_fpl, fail);

    // parse the input file into a set of productions:
    Productions productions(inp);

    std::string output = productions.generate_code(opts);

    // states are generated as a side effect of generate_code,
    // which is not great, but I'm not going to fix it right now,
    // so dump_states has to go after generate_code():
    if(opts.dump_states)
        productions.dump_states();

    // uhh... this is easy, if hokey:
    if(opts.out) {
        fprintf(opts.out, "%s\n", output.c_str());
    } else {
        fail("no open output - fail\n");
        return ExitVal::BAD_ARGS;
    }

    return ExitVal::OK;
}

void usage() {
    fprintf(stderr,
        "\nUsage:    fpl2cc [options] <source>\n\n"
        "If no [target] is specified, prints to stdout.\n\n"
        "Options:\n"
    );
    // .. these decriptions suck...
    fprintf(stderr, "        --debug - emebed debug blather in target code\n");
    fprintf(stderr, "        --debug-single-step - as above plus pauses\n");
    fprintf(stderr, "        --debug-dump-states - print generated states\n");
    fprintf(stderr, "        --entry=<product> - specify a target production\n");
    fprintf(stderr, "        --generate-main - generate main() function\n");
    fprintf(stderr, "        --help - show this page\n");
    fprintf(stderr, "        --out=<fn> - write to fn instead of stdout\n");
}

int main(int argc, const char** argv) {
    Options opts(argc, argv);
    ExitVal status = ExitVal::FAIL;

    if(opts.errors.size()) {
        for(auto fail : opts.errors) {
            fprintf(stderr, "%s\n", fail.c_str());
        }
        usage();
        status = ExitVal::BAD_ARGS;
    } else {
        if(opts.help)
            usage();

        status = fpl2cc(opts);
    }

    if(num_warnings > 0) {
        fprintf(stderr, "fpl2cc: %i warnings\n", num_warnings);
    }

    exit(status);
}


