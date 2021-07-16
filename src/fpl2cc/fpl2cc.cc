#include <climits>
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

  If there's any repetition,... XXX

  Code blocks should use a normal "return" statement to return a pointer
  to a Jest::Member.

 */

/*
 TODO

  x fix the thing where it does 'class ./foo' if you tell it ./foo.fpl
  - buffering the entire input is busted for things like stdin.
    stream instead;  but possibly fix that via chicken/egging it
    and generate the new parser with this.
  - Sort out reducing.  I think reducing is wrong presently.  huh can it
    just simply return on reduce?  only if the callers know what to do,
    because it's the callers which will have to slap in the code for
    the matched rule.  I think.  hah.  (the way it is now, state functions
    need to see stuff above them on the stack, which ain't gonna work)
 */

struct Options {
    std::string src_fpl;
    bool generate_main;

    // janky, but good enough:
    std::string _fail;
    Options(int argc, const char* const* argv) {
        generate_main = false;

        _fail = "";
        for(int argi = 1; argi < argc; argi++) {
            const char *arg = argv[argi];
            if(!arg[0]) continue; // I guess ignore blank args

            if(arg[0] == '-') {
                if(arg[1] == '-') {
                    // double dash '--foo' style args:
                    const char *optarg = arg + 2;
                    if(!strcmp(optarg, "generate_main")) {
                        generate_main = true;
                    } else {
                        _fail = "Unknown option: "; _fail += optarg;
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
and a code block (string).  Each item has a minimum and maximum
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

struct ProductionRule {
    std::string product;
    std::vector<ProdExpr> steps;
    std::string code;
    const utf8_byte *start_of_text;

    ProductionRule(const utf8_byte *at) : start_of_text(at) {
    }

    void add_step(ProdExpr step) {
        steps.push_back(step);
    }

    // returns NULL if index is out of bounds
    const ProdExpr *step(unsigned int index) const {
        if(index < steps.size()) {
            return &steps[index];
        }
        return NULL;
    }

    std::string to_str() const {
        std::string out;
        for(auto step : steps) {
            out += step.to_str();
            out += " ";
        }

        if(!out.empty())
            out.pop_back(); // remove trailing space

        return out;
    }

};

class Productions {
    fpl_reader inp;

    std::vector<ProductionRule>     rules;
    std::multimap<std::string, int> rules_for_product; // product  -> rule ind

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

        friend bool operator<(const lr_item& left, const lr_item& right) {
            if(left.rule == right.rule)
                return left.position < right.position;
            return left.rule < right.rule;
        }

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
            snprintf(buf, bs, "%s (rule %i):", rl.product.c_str(), rule);
            buf[bs - 1] = '\0';
            std::string out(buf);

            int step;
            for(step = 0; step < rl.steps.size(); ++step) {
                out += " ";
                if(step == position)
                    out += "•";
                out += rl.steps[step].to_str();
            }
            if(step == position)
                out += "•";

            return out;
        }
    };
    inline const ProdExpr &lr_item_step(const lr_item &it) {
        return rules[it.rule].steps[it.position];
    }

    // an lr_set is a set of lr items.
    // it is it's own special class mostly so that we can
    // compare them to find the set of parser states.
    // (each state can be represented by an lr_set)
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
                const int bytes_per_item = 8;
                const int len = items.size()*bytes_per_item + 1;
                char buf[len];
                char *bw = buf;
                for(auto it : items) {
                    snprintf(
                        bw, bytes_per_item+1, "%04x%04x", it.rule, it.position
                    );
                    bw += bytes_per_item;
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

        std::string to_str(
            const Productions *prds,
            int indent = 0,
            const char *line_prefix = ""
        ) const {
            // efficient? probably not. do I care?
            std::string out;
            for(auto it : items) {
                out.append(line_prefix);
                for(int ind = 0; ind < indent; ind++) {
                    out.append("    ");
                }
                out.append(it.to_str(prds));
                out.append("\n");
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
                const utf8_byte *sot = rule.start_of_text;
                fail(
                    "Nothing produces «%s» "
                    "(used by rule starting with «%.20s» at line %i\n",
                    pname.c_str(), sot, inp.line_number(sot)
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
                //   - "!" is complicated and might be unnecessary.
                //     axe it? axing. consider it axed!
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
        } while(set.items.size() > last_size); // i.e. do until we add no more

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

    std::string transition_id(const ProdExpr &pexp, const lr_set &to) {
        char buf[40];
        snprintf(buf, 40, "%0x_", element_index[pexp.gexpr]);
        std::string out(buf);
        out += to.id();
        return out;
    }

public:

    Productions(fpl_reader &src) : inp(src) { }

    void push_back(const ProductionRule &rule) {
        int rule_num = rules.size();
        rules.push_back(rule);

        for(const ProdExpr &step : rule.steps) {
            record_element(step.gexpr);
        }

        rules_for_product.insert(std::make_pair(rule.product, rule_num));
    }

    // returns the name of the function to use for the
    // given state
    std::string state_fn(const lr_set &state) {
        std::string fn("state_");
        int state_num = state_index[state.id()];
        fn += std::to_string(state_num);
        return fn;
    }

    /*
       Let's say:

       struct { const utf8_byte *pos, int length, operator bool() } terminal;
       .. length <= 0 is a false value.  Alternately pos = NULL.

       struct { void *product, int grammar_element_id  } production;

       terminal scan_xxx() returns the terminal or one with length <= 0.
       can be inline.  

       state_x() {
           terminal scanned; 
           production prod;

           if(scanned = scan_xxx(...)) {
               prod = state_y();
           } else if(scanned = scan_xxx(...)) {
               prod = state_z();
           } .. etc

           if(prod.grammar_element_id == whatever element id)
           } .. else if next prod.grammar_element_id == ...) {
           } .. etc

           // .. call the production code for the rule here,
           // with ... hmm params

           return prod;
       }
    */
    std::string code_for_terminals(const lr_set &state) {
        std::string out;
        std::map<std::string, uint32_t> done;
        int conflict_count = 0;

        out += "FPLBaseParser::terminal shifted;\n";

        for(auto item : state.items) {
            const ProductionRule &rule = rules[item.rule];
            const ProdExpr *right_of_dot = rule.step(item.position);
            if(right_of_dot) {
                if(right_of_dot->is_terminal()) {

                    if(done.size() > 0) {
                        out += "else ";
                    }

                    // if we match a terminal, we need to shift and go
                    // to a new state:
                    lr_set next_state = lr_goto(state, right_of_dot->gexpr);
                    // (extra parens avoids a warning.. sigh)
                    out += "if((shifted = ";
                    switch(right_of_dot->type()) {
                        case GrammarElement::TERM_EXACT:
                            out += "shift_exact(\"";
                            out += right_of_dot->terminal_string();
                            break;
                        case GrammarElement::TERM_REGEX:
                            out += "shift_re(\"";
                            out += right_of_dot->terminal_string();
                            break;
                        default:
                            fail(
                                "bug: unknown type %i at %i in state %s",
                                right_of_dot->type(), item.position, state.id().c_str()
                            );
                            break;
                    }
                    out += "\"))) {\n";
out += "    fprintf(stderr, \"" + state_fn(state) + " shifted terminal " + right_of_dot->terminal_string() + " of '%s' to " + state_fn(next_state) + "\\n\", shifted.to_str().c_str());\n";
                    out += "// " + item.to_str(this) + "\n";
                    std::string trans_id = transition_id(*right_of_dot, next_state);
                    uint32_t exists = done[trans_id];
                    if(exists) {
                        lr_item other = lr_item::from_id(exists);
                        if(other.rule != item.rule) {
                            conflict_count++;
                            out += "// XXX conflicts with "
                                 + other.to_str(this) + "\n";
                        }
                    }
                    out += "prd = " + state_fn(next_state) + "();\n";
                    out += "}\n";

                    if(!exists)
                        done[trans_id] = item.id();
                }
            }
        }

        if(conflict_count > 0) {
            fprintf(stderr,
                "Warning: %i terminal conflicts.  See generated code.\n",
                conflict_count
            );
        }

        return out;
    }

    std::string code_for_prods(const lr_set &state) {
        std::string out;
        std::map<int, int> element_ids_done;

        for(auto item : state.items) {
            const ProdExpr *right_of_dot = rules[item.rule].step(item.position);
            if(right_of_dot) {
                if(right_of_dot->type() == GrammarElement::NONTERM_PRODUCTION) {
                    int el_id = element_index[right_of_dot->gexpr];

                    auto existing = element_ids_done.find(el_id);
                    if(existing == element_ids_done.end()) {
                        // (i.e. no existing code for this)
                        lr_set next_state = lr_goto(state, right_of_dot->gexpr);
                        if(element_ids_done.size() > 0)
                            out += "else ";
                        out += "if(prd.grammar_element_id == NontermID::_"
                               + right_of_dot->gexpr.to_str() + ") {\n";
                        out += "    // " + item.to_str(this) + "\n";
                        out += "    prd = " + state_fn(next_state) + "();\n";
out += "    fprintf(stderr, \"" + state_fn(state) + " shifted nonterminal " + right_of_dot->terminal_string() + " to " + state_fn(next_state) + "\\n\");\n";
                        out += "}\n";
                        element_ids_done[el_id] = state_index[state.id()];
                    } else if(existing->second != state_index[state.id()]) {
                        // gar this error message could be more informative.
                        // we might really want to know what the current item is
                        // conflicting _with_.. XXX
                        fail(
                            "conflicting gotos for %s\n",
                            item.to_str(this).c_str()
                        );
                    }
                }
            }
        }
        return out;
    }

    std::string code_for_handling_reduce(const lr_set &state) {
        std::string out;
        // 2 possible cases here:
        //  - there exists explicit code for the production
        // if(state.

        return out;
    }

    std::string code_for_state(const lr_set &state) {
        std::string out;

        out += "//\n";
        out += state.to_str(this, 1, "//");
        out += "//\n";
        out += "FPLBaseParser::product "; out += state_fn(state); out += "() {\n";
        out += "product prd;\n";

out += "    fprintf(stderr, \"entering state " + state_fn(state) + "\\n\");\n";
        out += code_for_terminals(state) + "\n";

        // ... or this is really the gotos
out += "    fprintf(stderr, \"mebbe gotos for " + state_fn(state) + "\\n\");\n";
        out += code_for_prods(state) + "\n";

        // if we're at the end of the item we need to reduce
        // according to the next possible ...... XXX
out += "    fprintf(stderr, \"ok like we are near the reduce for " + state_fn(state) + "\\n\");\n";
        out += "    // XXX reduce code here thanks\n"
               "    if(--prd.reduce_count == 0) {\n";
        out += code_for_handling_reduce(state);
        out += "    }\n";

        out += "    return prd;\n";
        out += "}\n"; // end of state_ function

        return out;
    }

    // ... this mostly just tries to fix indent
    std::string format_code(const std::string code) {
        const int chars_per_indent = 4;
        int indent = 0;
        int new_lines = 0;

        std::string output;

        const std::string::size_type code_length = code.size();
        for(std::string::size_type inp = 0; inp < code_length; inp++) {
            if(code[inp] == '{') {
                // good-enough hack to avoid issues with
                // quoted { or comments with { or whatever:
                // only count '{' at end of line
                if(code[inp + 1] == '\n') {
                    indent += chars_per_indent;
                }
            } else if(code[inp] == '}') {
                // counterpart to the above good-enough hack:
                // only count '}' if it's immediately after
                // an indent
                if(new_lines > 0)
                    indent -= chars_per_indent;
            } else if(code[inp] == '\n') {
                // skip spaces;  we'll convert them to the correct indent:
                while(inp < code_length && isspace(code[inp])) {
                    // (.. but preserve newlines)
                    if(code[inp] == '\n') new_lines++;
                    inp++;
                }
                if(inp < code_length)
                    inp--;
                continue;
            }

            if(new_lines > 0) {
                while(new_lines > 0) {
                    output.push_back('\n');
                    --new_lines;
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
    // instead of "if(prd.grammar_element_id == 62)" we
    // can do "if(prd.grammar_element_id == NontermID::_decimal_constant)"
    std::string nonterm_enum() {
        std::string out;

        out += "typedef enum {\n";
        for(int el_id = 0; el_id < elements.size(); ++el_id) {
            // include terminals as comments.  we don't need them
            // in the enum, but it's nice to be able to see all the
            // grammar elements in one place
            if(elements[el_id].type != GrammarElement::NONTERM_PRODUCTION)
                out += "// ";
            else
                out += "_"; // hack to avoid keyword collisions

            out += elements[el_id].to_str();
            out += " = ";
            out += std::to_string(el_id);

            // sigh.. languages which don't allow trailing comma..
            if(el_id + 1 < elements.size()) 
                out += ",\n";
            else
                out += "\n";
        }
        out += "} NontermID;\n\n";
        return out;
    }

    std::string parser_class_name(const Options &opts, const fpl_reader &src) {
        return src.base_name() + "_parser";
    }

    std::string code_for_main(const std::string &parser_class) {
        std::string out("\n\n");

        // the main() generated here is pretty much just a test stub.
        // if people want something fancier, they can make their own.
        out += "int main(int argc, const char **argv) {\n";
        out += "    if(argc < 2) {\n";
        out += "        fpl_reader::default_fail(";
        out += "            \"Please provide a source file name.\\n\"";
        out += "        );\n";
        out += "    }\n";
        out += "    fpl_reader inp(argv[1]);\n";
        out +=      parser_class + " parser(inp);\n";
        out += "    parser.parse();\n";
        out += "}\n";

        return out;
    }

    void generate_code(const Options &opts, const fpl_reader &src) {
        const auto no_set = state_index.end();
        const std::string parser_class = parser_class_name(opts, src);

        // for now, we're going to consider the first rule
        // (rule 0) to be the starting point (and start with
        // the first expression in that rule, of course):
        add_state(lr_closure(lr_item(0, 0)));

        // Aho, Sethi, and Ullman page 224... uhh, modified.
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

        std::string out;
        out += "#include \"fpl2cc/fpl_reader.h\"\n";
        out += "#include \"fpl2cc/fpl_base_parser.h\"\n";
        out += "\nclass " + parser_class + " : FPLBaseParser {\n";

        out += nonterm_enum();

/*
        // reverse helps with inlining (I hope).
        for(auto it = states.rbegin(); it != states.rend(); ++it) {
            out += code_for_state(*it).c_str();
        }
 */
        for(lr_set state : states) {
            out += code_for_state(state).c_str();
        }

        out += "\npublic:\n";
        out += "    " + parser_class + "(fpl_reader &src) : FPLBaseParser(src) { }\n";
        out += "    void parse() { state_0(); };\n";

        out += "};\n"; // end of class

        if(opts.generate_main) {
            out += code_for_main(parser_class);
        }

        printf("%s\n", format_code(out).c_str());
    }
};

void read_quantifiers(fpl_reader &src, ProdExpr &expr) {
    const utf8_byte *inp = src.inpp();
    if(!inp) return; // EOF.  this is really an error..

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

void eat_comment(fpl_reader &src) {
    size_t nll;
    while(!(nll = src.newline_length(src.inpp()))) {
        src.skip_char();
    }
    src.skip_bytes(nll); // line comment includes the terminating newline
}

int read_expressions(fpl_reader &src, ProductionRule &rule) {
    int num_read = 0;
    bool done = false;
    do {
        src.eat_space();

        const utf8_byte *inp = src.inpp();

        std::string expr_str;
        GrammarElement::Type type = GrammarElement::Type::NONE;

        switch(*inp) {
            case '\0':
                done = true;
                break; // EOF
            case '#':
                eat_comment(src);
                break;
            case '"':
            case '\'':
                expr_str = src.read_to_byte(*inp);
                type     = GrammarElement::Type::TERM_EXACT;
                break;
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
                // should be the name of a production
                expr_str = src.read_re("[A-Za-z_]+")[0];
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

// returns a string containing the code to "generate" when
// the production ends in a ';'
std::string default_code() {
    return "return arg1;";
}

std::string read_code(fpl_reader &src) {
     // code is within "+{" "}+" brackets.
     // this is done simplistically, which means you will
     // derail it if you put +{ or }+ in a comment or
     // string or whatever.  so try not to do that.
     // oh but it needs to be minimal match, not greedy....
     src.eat_space();
     const utf8_byte *start = NULL;
     const utf8_byte *end   = NULL;
     if(src.read_byte_equalling(';')) {
         return default_code();
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
    while(!inp.eof()) {
        ProductionRule rule(inp.inpp());

        // read the expressions/steps leading to the production
        // (until and including the "->")
        if(read_expressions(inp, rule)) {

            // read what the expressions above produce:
            inp.eat_space();
            const utf8_byte *start = inp.inpp();
            rule.product = inp.read_to_space();
            if(rule.product.length() <= 0) {
                fail(
                    "missing production name on line %i near %.12s\n",
                    inp.line_number(start), start
                );
            }

            // read the code for the production
            rule.code = read_code(inp);

            // add it to the set of productions
            productions.push_back(rule);
        }

        inp.eat_space();
    }

    productions.generate_code(opts, inp);

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

    exit(0);
}


