#ifndef PRODUCTIONS_H
#define PRODUCTIONS_H

#include "code_block.h"
#include "options.h"
#include "production_rule.h"
#include "reducer.h"

#include "util/c_str_escape.h"
#include "util/jerror.h"
#include "util/src_location.h"
#include "util/stringformat.h"

#include <list>
#include <map>
#include <string>

namespace fpl {

class productions;
std::string fpl_x_parser(const productions &, const options &);

class productions {

    fpl_reader_p  inp;
    const options opts;

    std::string reduce_type; // default reduce type
    std::map<std::string, std::string> type_for_product; // (reduce type for particular product)

    std::list<std::string> imports; // filenames
    code_block default_action;
    code_block post_parse;
    code_block post_reduce;
    std::list<code_block> separator_code;
    bool default_main;
    code_block main_guts;
    std::list<code_block> preamble;
    std::list<code_block> parser_members;
    std::list<std::string> goal; // goal is any of these

    std::vector<production_rule>     rules;
    std::multimap<std::string, int> rules_for_product; // product -> rule ind

    std::vector<grammar_element>     elements;
    std::map<grammar_element, int>   element_index;

    std::list<reducer> reducers;

    class lr_set;
    std::vector<lr_set> states;
    std::map<std::string, int> state_index; // keyed by set id


    static void warn(const std::string &msg) { jerror::warning(msg); };

    // report an error relative to the file for the reader passed
    static void error(
        const fpl_reader &rdr, size_t pos, const std::string &msg
    ) {
        jerror::error(rdr.format_error_message(pos, msg).c_str());
    }

    // report an error at the given SourcePosition
    static void error(const SourcePosition where, const std::string &msg) {
        error(where.reader(), where.position(), msg);
    }

    // report an error at the current position in the given reader.
    // this is a hokey hack...
    static void error(const fpl_reader &rdr, const std::string &msg) {
        error(rdr, rdr.current_position(), msg);
    }

    // report an error at the current position in the current input
    void error(const std::string &msg) const {
        error(*inp, inp->current_position(), msg);
    }

    // call this if there's a fatal bug detected at execution time:
    static void internal_error(
        const std::string &msg, src_location caller = CALLER()
    ) {
        jerror::error(stringformat("Internal error: {} at {}", msg, caller));
        exit(1);
    }

    void add_state(const lr_set &st) {
        state_index.insert(
            std::make_pair(st.id(), states.size())
        );
        states.push_back(st);
    }

    // records the fact that the given grammar element exists
    void record_element(const grammar_element &nge) {
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
        uint16_t rule;     // offset into rules (i.e. rule number)
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

        std::string to_str(const productions *prds) const {
            if(!prds) return "NULL productions";

            const production_rule &rl = prds->rules[rule];

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
                out += rl.nth_step(step)->to_str();
            }
            if(step == position)
                out += "•";

            return out;
        }
    };

    // an lr_set is a set of lr items.
    // each state is represented by an lr_set.
    class lr_set {
        mutable std::string _id_cache;
        std::set<lr_item> items;
    public:

        lr_set() { }

        // an lr_item can count as a 1-item set:
        lr_set(const lr_item &in) {
            items.insert(in);
        }

        // The id of the set is a string generated from the content
        // of the items which can be compared to determine if 2 sets
        // are identical (within these productions) or not.
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

        // adds the given lr_item to the lr_set, plus any other
        // related items to cover optionalness.
        // (repetition is handled in the goto)
        // the rule passed is the rule in the productions for the item
        // (i.e. the rule in which we're expanding)
        void add_expanded(const lr_item &it, const production_rule &rule) {
            if(!it) return;

            for(int pos = it.position; pos <= rule.num_steps(); pos++) {
                add_item(lr_item(it.rule, pos));
                const production_rule::step *expr = rule.nth_step(pos);
                if(!expr || !expr->is_optional()) {
                    // end of rule or the items is not optional so we
                    // don't need to look after:
                    break;
                }
            }
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
        lr_item reduction_item(const productions *prds) const {
            for(auto it : items) {
                const production_rule &rl = prds->rules[it.rule];
                if(rl.num_steps() == it.position) {
                    return it;
                }
            }
            return lr_item(); // no reduction here
        }

        // returns true if this state only reduces
        bool reduce_only(const productions *prods) const {
            for(lr_item item : iterable_items()) {
                const production_rule &rule = prods->rules[item.rule];
                if(rule.nth_step(item.position)) {
                    // this item is not at the end of the rule,
                    // so it's some kind of shift, so this is
                    // not pure reduce.
                    return false;
                }
            }
            return true;
        }

        std::string to_str(
            const productions *prds,
            const std::string &line_prefix = "",
            const std::string &line_suffix = "\n",
            bool stringescape = false
        ) const {
            std::string out;
            for(auto it : items) {
                out.append(line_prefix);
                if(stringescape)
                    out.append(c_str_escape(it.to_str(prds)));
                else
                    out.append(it.to_str(prds));
                out.append(line_suffix);
            }
            return out;
        }
    };

    struct lr_transition {
        const production_rule::step *right_of_dot;
        int next_state_number;

        lr_transition(const production_rule::step *rod, int sn) :
            right_of_dot(rod), next_state_number(sn) { }
    };
    using lr_transitions = std::vector<lr_transition>;


    void lr_closure_add_rules(
        lr_set &set, const production_rule &rule, int pos
    ) const {
        const production_rule::step *right_of_dot = rule.nth_step(pos);

        if(right_of_dot && !right_of_dot->is_terminal()) {
            // The thing to the right of the dot is a product,
            // so we need to add items for each rule that can
            // produce that (per the aho/sethi/ullman pg 222,
            // rule #2, closure procedure).
            std::string pname = right_of_dot->production_name();
            auto strl  = rules_for_product.lower_bound(pname);
            auto endrl = rules_for_product.upper_bound(pname);

            if(strl == endrl) {
                error(rule.location(), stringformat(
                    "Nothing produces «{}»\n", pname
                ));
            }

            for(auto rit = strl; rit != endrl; ++rit) {
                set.add_expanded(
                    // (these are always position 0)
                    lr_item(rit->second, 0), rules.at(rit->second)
                );
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
    lr_set lr_closure(const lr_set &in) const {
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
                const production_rule &rule = rules[item.rule];
                int pos = item.position;
                const production_rule::step *right_of_dot;
                // while loop here handles positions at eof (= NULL)
                // as well helping handle optional expressions
                // (i.e. expressions with "*" and "?"):
                while(right_of_dot = rule.nth_step(pos)) {
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


    // "goto" operation from page 224 Aho, Sethi and Ullman,
    // augmented to allow repetition (eg from * and + operators).
    // given a current state and a lookahead item, returns
    // the next state (i.e. the set of items which might appear
    // next)
    lr_set lr_goto(const lr_set &in, const grammar_element &sym) const {
        lr_set set;
        for(auto item : in.iterable_items()) {
            const production_rule &rule = rules.at(item.rule);
            const production_rule::step *step = rule.nth_step(item.position);
            if(step && step->matches(sym)) {
                set.add_expanded(lr_item(item.rule, item.position + 1), rule);
                if(step->max_times > 1) {
                    // ...if it can be repeated:
                    set.add_expanded(lr_item(item.rule, item.position), rule);
                }
            }
        }
        return lr_closure(set);
    }

    // used for detecting conflicts
    std::string transition_id(const production_rule::step &pexp, const lr_set &to) {
        char buf[40]; // 40 is arbitrarily larger than the 5 we'll need
        snprintf(buf, 40, "%0x_", element_index[pexp.gexpr]);
        std::string out(buf);
        out += to.id();
        return out;
    }

public:

// XXX this is currently stupid in that you pass the fpl_reader
// but then you have to make a separate call to parse it.  fix that.
    productions(const options &op, fpl_reader_p src) :
        inp(src), opts(op), default_main(false)
    {
        // element 0 is a null element and can be used to
        // indicate missing/uninitialized elements or such.
        // we count it as a nonterminal so that it can be
        // accessed via the enum etc.
        record_element(
            grammar_element("_fpl_null", grammar_element::NONTERM_PRODUCTION)
        );

        // default the reduce type to string.
        // this will (mostly?) work with defaults if
        // you just want to sketch out a grammar and
        // not have to specify any particular code.
        reduce_type = "std::string";
    }

    // returns the name of the type expected as the result
    // of reducing to the product type indicated:
    std::string type_for(const std::string &product) const {
        auto tf = type_for_product.find(product);
        if(tf != type_for_product.end()) {
            return tf->second;
        }
        return reduce_type;
    }

    // expects/scans a +{ }+ code block for the named directive.
    // the named directive is essentially for error reporting.
    enum code_source{
        INLINE = 1,
        LIB    = 2,
        REGEX  = 4,

        INLINE_OR_LIB = 3,
    };
    inline code_block code_for_directive(
        const std::string &dir, code_source allowed_src = INLINE
    ) {

        code_block code;
        if(allowed_src | INLINE) {
            code = read_code(*inp);
        }

        if(!code && (allowed_src | LIB)) {
            // expect the name of a file with the code:
            std::string fn = inp->read_re("\\s*(.+)\\s*")[1];
            if(fn.length() > 0) {
                code = code_block::from_file(
                    fn + ".inc", opts.src_path,
                    inp->filename(), inp->line_number()
                );
            }
        }

        std::string errm;
        if(!code && (allowed_src | REGEX)) {
            errm = "XXX FIXME ALLOW REGEX LENGTH";
        }

        if(!code) {
            if(allowed_src) {
                errm = stringformat("expected code for directive {}", dir);
            } else {
                errm = stringformat(
                    "Internal error: "
                    "code for directive {} not allowed from anything",
                    dir
                );
            }
            error(errm);
        }

        return code;
    }

    void add_separator_code(const code_block &code) {
        separator_code.push_back(code);
    }

    void add_comment_style(const std::string &style,
        const std::string &context_fn, int context_ln
    ) {
        // comment styles are to be found in a "comment"
        // subdir.  this is because eventually I think I
        // want to fold the whole @comment_style thing into
        // other separators and not implement as c++ fragments.
        fs::path fn(style);
        fn.replace_filename("comment/" + style + ".inc");
        add_separator_code(code_block::from_file(
            opts.src_path.find(fn), Searchpath(), context_fn, context_ln
        ));
    }

    void set_reduce_type(const std::string &rt)    { reduce_type = rt; }
    void set_default_action(const std::string &rt) { default_action = rt; }
    void set_post_parse(const code_block &cb)      { post_parse = cb; }
    void set_post_reduce(const code_block &cb)     { post_reduce = cb; }
    void set_default_main(bool def)                { default_main = def; }
    void add_internal(const code_block &cb)        { parser_members.push_back(cb); }

    std::string arg_for_directive() {
        // reads an argument to the end of the line.
        // end of line is any ascii vertical space (for now).
        // leading and trailing spaces/tabs are stripped
        return inp->read_re("[ \\t]*([^@\\x0a-\\x0d]+)[ \\t]*\n")[1];
    }

    void parse_directive(const std::string &dir) {
        if(dir == "comment_style") {
            // this is a near synonym with @separator
            int line_num = inp->line_number();
            std::string style = arg_for_directive();
            if(!style.length()) {
                warn("no comment style specified");
            } else {
                add_comment_style(style, inp->filename(), line_num);
            }
        } else if(dir == "default_action") {
            default_action = code_for_directive(dir);
        } else if(dir == "default_main") {
            // XXX deprecated. kill this in favor of
            // having a default main in grammarlib and importing.
            default_main = true;
        } else if(dir == "grammar") {
            // import the grammar from another fpl (or a library)
            productions sub(opts, open_for_import(arg_for_directive()));
            sub.parse_fpl();
            import_grammar(sub);
        } else if(dir == "internal") {
            // a code block which goes in the "private" part
            // of the parser class itself.  This is either
            // convenience for the fpl author, or a hack around
            // c++, depending on how you want to look at it.
            if(code_block mem = code_for_directive(dir)) {
                parser_members.push_back(mem);
            }
        } else if(dir == "main") {
            main_guts = code_for_directive(dir, code_source::INLINE_OR_LIB);
        } else if(dir == "post_parse") {
            post_parse = code_for_directive(dir);
        } else if(dir == "post_reduce") {
            post_reduce = code_for_directive(dir);
        } else if(dir == "produces") {
            reduce_type = arg_for_directive();
        } else if(dir == "separator") {
            add_separator_code(
                code_for_directive(dir, code_source::INLINE_OR_LIB)
            );
        } else if(dir == "type_for") {
            inp->eat_separator();
            std::string prod = read_production_name();
            inp->eat_separator();
            std::string type = inp->read_re(".*")[0];
            if(prod.length() && type.length()) {
                type_for_product[prod] = type;
            } else {
                error("type_for expects <product name> = <type>");
            }
        } else {
            error(stringformat("Unknown directive: '{}'\n", dir));
        }
    }

    void push_rule(production_rule &rule) {
        int rule_num = rules.size();
        rule.set_rulenum(rule_num);

        // we set the default action for the rule here so
        // that (1) we don't have to resolve it later
        // and (2) authors can set a default action, define
        // several rules for which it's a good default, then
        // set another, etc..
        if(!rule.code()) {
            rule.code(default_action);
        }

        rules.push_back(rule);

        for(int stp = 0; stp < rule.num_steps(); stp++) {
            record_element(rule.nth_step(stp)->gexpr);
        }
        record_element(rule.product_element());

        rules_for_product.insert(std::make_pair(rule.product(), rule_num));

        // if this rule has exactly one (.. hmm or <= 1?)
        // step, it's a candidate for folding..
        //  OK foldable thought/typing experiments (all assume there's
        //  no abstracted reducer):
        //    a -> b ;  # fold: a can be substituted for b always
        //    "a" -> b ;  # fold: "a" can be substituted in for b (assuming
        //                # the "a" can be converted on passing)
        //    "("^ foo ")"^ -> bar ; # fold: match all; foo will be passed
        //    "true" -> true ;
        //    "false" -> false ;
        //    true -> boolean ;  # foldable, except that...
        //    false -> boolean ; #  ... since this is also boolean and we
        //                       # don't do "or" we'd have to duplicate
        //                       # rules or somehting to fold
        //    boolean '|' boolean => bexpr ; # ...
        // .. so anyway I guess we're justing findind fold candidates here.
        // OH ONE INTERESTING THING:  if a parameter is ejected,
        // it can be folded into a rule where the same param is ejected:
        //      foo^ => bar ;
        //      "x" "y" "z" => foo ;
        // The above can be converted to "x"^ "y"^ "z"^ => bar ; because nothing
        // is passed;  it's solely for matching.
        // i.e. if the step the rule is being folded into gets ejected, I think
        // you can always fold..?
        // XXX figure out if folding is useful.  at one point you thought it
        // was.  maybe more explicit aliasing of terminals is enough.
    }

    void add_preamble(const code_block &code) {
        preamble.push_back(code);
    }

    static void read_quantifier(fpl_reader &src, production_rule::step &expr) {
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

    static void read_suffix(fpl_reader &src, production_rule::step &expr) {
        if(src.read_byte_equalling('^')) {
            expr.eject = true;
        } else if(src.read_byte_equalling(':')) {
            // the name to give the argument corresponding
            // this this step follows:
            expr.varname = src.read_re("[A-Za-z][A-Za-z0-9_]*")[0];
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

    inline std::list<std::string> imported_files() {
        return imports;
    }

    // Attempts to open an fpl source file and associate it with a
    // reader for import.  Searches the directory this source file
    // is in, as well as any other directories in the --src-path.
    fpl_reader_p open_for_import(const std::string &fpl_name) {

        Searchpath searchp = opts.src_path;

        // search relative to the importing fpl first:
        searchp.prepend(inp->input_dir());
        std::string filename = opts.src_path.find(fpl_name + ".fpl");

        // this is for generating dependencies and such (--dump-dependencies)
        imports.push_back(filename);

        // report errors in the sub-fpl in the context of
        // the importing file:
        fpl_reader &src = *inp;
        auto sub_errcb = [&src](const std::string &msg)->void {
            error(src, "\n\t" + msg);
        };

        return make_shared<fpl_reader>(filename, sub_errcb);
    }

    // syntax: '`' filename '`' ~ /(.production_to_import)/?
    // imports relevant rules into this and returns the name of
    // the top level production created
    std::string parse_import(fpl_reader &src, production_rule &rule) {
        std::string filename(src.parse_string());
        if(!filename.length()) {
            error(src, "no filename specified");
            return "<failed import>";
        }

        // `grammarname`.production means import only the
        // specified production:
        std::string prod_name;
        if(src.read_byte_equalling('.')) {
            prod_name = read_production_name(src);
        }

        productions subs(opts, open_for_import(filename));
        subs.parse_fpl();
        if(!subs.reduce_type.length()) {
            // the imported fpl doesn't specify that it produces any
            // particular type(s), so we tell it to produce what we want:
            subs.reduce_type = reduce_type;
        }

        return import_grammar(subs, prod_name);
    }


    int read_expressions(fpl_reader &src, production_rule &rule) {
        int num_read = 0;
        bool done = false;
        do {
            src.eat_separator();

            std::string expr_str;
            grammar_element::Type type = grammar_element::Type::NONE;

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
                    type     = grammar_element::Type::TERM_EXACT;
                    break;
                case '/':
                    expr_str = src.parse_string();
                    type     = grammar_element::Type::TERM_REGEX;
                    break;
                case '~':
                    // lack-of-space pseudo-terminal (or assertion?)
                    src.read_byte();
                    expr_str = "~";
                    type     = grammar_element::Type::LACK_OF_SEPARATOR;
                    break;
                case '-':
                    src.read_byte();
                    if(src.read_byte_equalling('>')) {
                        // just scanned "->", so we're done:
                        done = true;
                    } else {
                        error(src, "unexpected '-'");
                    }
                    break;
                case '`':
                    // parse/import the sub-fpl, and use whatever it produces:
                    expr_str = parse_import(src, rule);
                    type     = grammar_element::Type::NONTERM_PRODUCTION;
                    break;
                case '}':
                    // this can happen, especially if there's a '}+'
                    // embedded in a code block.
                    if(src.read_byte_equalling('+'))
                        error(src,
                            "stray '}+'.  "
                            "perhaps there's }+ embedded in a code block"
                        );
                    else
                        error(src, "unmatched '}'");
                    break;
                default:
                    // should be the name of a production.
                    expr_str = read_production_name(src);
                    if(!expr_str.length()) {
                        error(src, stringformat(
                            "expected production name for rule '{}'\n"
                            " starting at {}",
                            rule.to_str(), rule.location()
                        ));
                    }
                    type = grammar_element::Type::NONTERM_PRODUCTION;
                    break;
            }

            if(type != grammar_element::Type::NONE) {
                if(expr_str.length() >= 1) {
                    production_rule::step expr(expr_str, type);
                    // XXX awkward
                    if(type == grammar_element::Type::LACK_OF_SEPARATOR)
                        expr.eject = true;
                    read_quantifier(src, expr);
                    read_suffix(src, expr);
                    rule.add_step(expr);
                    num_read++;
                } else {
                    // XXX show the type in some non-numeric way here
                    error(src, stringformat(
                        "expected type {} = {} but got .. nothing?\n",
                        grammar_element::Type_to_str(type), type
                    ));
                }
            }
        } while(!(done || src.eof()));

        return num_read;
    }

    static code_block read_code(fpl_reader &src) {
         // code is within "+{" "}+" brackets.
         // we don't know anything about the grammar of the code
         // within the brackets (presently), so you will derail
         // it if you put unmatched +{ or }+ in a comment or
         // string or whatever.  sorry.  try not to do that.
         // matched +{ }+ it will handle ok.
         src.eat_separator();

         size_t start = src.current_position();
         if(!src.read_exact_match("+{")) {
             // no code - return a false value
             return code_block();
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
             error(src, stringformat(
                 "Expected code block terminator ('}}+') but got byte 0x{}",
                 to_hex(byte_in)
             ));
         }

         return code_block(code_str, src.filename(), src.line_number(start));
    }

    // reads and returns the production name from the current
    // reader.
    // on error, returns a 0-length string
    std::string read_production_name() {
        std::cmatch nm = inp->read_re("[a-zA-Z][a-zA-Z_0-9]*");
        if(!nm.length())
            return "";
        return nm[0];
    }

    // argument declaration for a reduction code block:
    //
    //   '(' (argument ','?)* ')' -> argdecl ;
    //
    std::set<std::string> parse_argdecl() {
        std::set<std::string> args;

        if(!inp->read_byte_equalling('(')) {
            error("expected start of argument declaration '('");
        } else {
            while(!inp->read_byte_equalling(')')) {
                std::string name = read_production_name();
                if(!name.length()) {
                    error("invalid production name");
                    break;
                }

                args.insert(name);

                // allow (but do not require) commas between (or after)
                // production names (by skipping them)
                inp->eat_separator();
                inp->read_byte_equalling(',');
                inp->eat_separator();

                if(inp->peek() == '\0') // more reasons to fpl this..
                    break;
            }
        }

        return args;
    }

    //
    // +<production_name> <argdecl> <code_block>
    //
    void parse_reducer() {
        if(!inp->read_byte_equalling('+')) {
            error("expected +<production_name>");
            return;
        }

        std::string name = read_production_name();
        if(name.length() == 0) {
            error("expected production name after '+'");
            return;
        }

        auto args = parse_argdecl();
        auto code = read_code(*inp);

        if(!code) {
             error(stringformat(
                 "expected start of code (\"+{{\") but got «{}»",
                 inp->debug_peek()
             ));
        }

        reducers.push_back(reducer(name, args, code));
    }

    void parse_fpl() {
        do {
            inp->eat_separator();
            if(inp->peek() == '#') {
                inp->read_line();
            } else if(inp->peek() == '+') {
                if(inp->peek(1) == '{') {
                    // inlined/general code - goes at the top of the
                    // generated code.  Use to define types or whatever.
                    code_block code(read_code(*inp));
                    if(code) {
                        add_preamble(code);
                    }
                } else {
                    // expect code for reducing to the production given:
                    parse_reducer();
                }
            } else if(inp->read_byte_equalling('@')) {
                std::string directive = read_directive(*inp);
                parse_directive(directive);
            } else if(inp->read_byte_equalling('}')) {
                // likely what happened is someone put a }+ inside
                // a code block.  anyway a floating end brace is
                // wrong..
                error("unmatched '}'\n");
            } else {
                production_rule rule(inp->filename(), inp->line_number());
                if(read_expressions(*inp, rule)) {
                    // .. we've read the expressions/steps leading to
                    // the production (including the "->").
                    // read what the expressions above produce:
                    inp->eat_separator();
                    std::cmatch pname = inp->read_re("[A-Za-z][A-Za-z0-9_]+");
                    if(!pname.length()) {
                        error("invalid production name\n");
                    } else {
                        rule.product(pname[0]);
                    }

                    inp->eat_separator();

                    // next we expect either ';' or a code block.
                    // if it's ';' we read it and move on;  otherwise
                    // it's a code block for the rule.
                    if(!inp->read_byte_equalling(';')) {
                        rule.code(read_code(*inp));
                        if(!rule.code()) {
                            error(stringformat(
                                "expected ';' or code block for rule {}",
                                rule.to_str()
                            ));
                        }
                    }

                    push_rule(rule);
                }
            }
        } while(!inp->eof());
    }

    // returns a set of strings representing the set of products
    // needed to create the product passed (including the product
    // passed)
    std::set<std::string> dependent_products(const std::string &prod) const {
        std::list<std::string> all_wanted = { prod};
        std::set<std::string> out;

        // NOTE I'm assuming here that we can append to a list
        // while iterating it and have things dtrt.  I believe
        // this is a reasonable thing to ask from std::list,
        // but have no documentation/spec saying it's ok.
        for(auto wanted : all_wanted) {
            out.insert(wanted);

            auto strl  = rules_for_product.lower_bound(wanted);
            auto endrl = rules_for_product.upper_bound(wanted);
            
            if(strl == endrl) {
                error(stringformat(
                    "No rule for '{}' in {}\n",
                    wanted, inp->filename()
                ));
            }

            for(auto rit = strl; rit != endrl; ++rit) {
                production_rule rule = rules[rit->second];

                // any rules needed to generate the rule we just pushed are
                // also potentially relevant:
                for(int stepi = 0; stepi < rule.num_steps(); ++stepi) {
                     const production_rule::step *step = rule.nth_step(stepi);
                     if(step && !step->is_terminal()) {
                         if(!out.count(step->production_name())) {
                             all_wanted.push_back(step->production_name());
                         }
                     }
                }
            }
        }

        return out;
    }

    // Imports the grammar from the productions specified.
    // This will include rules and whatever's deemed necessary
    // to support those rules.
    // returns the name of the production which this import will produce,
    // or  ...
    std::string import_grammar(
        const productions &from, const std::string &pname = ""
    ) {
        std::string src_fn = from.inp->filename();

        // import any preamble as well, as it may be necessary for the rules.
        // hmm.. too bad we can't scope this and/or figure out if it's necessary
        // to import in the first place... (can we scope?)
        // (consider either getting rid of this or making it optional - possibly
        // we only want to be importing "pure" fpl anyway)
        //preamble += "\n" + from.preamble;
        for(auto primp : from.preamble)
            add_preamble(primp);

        // these are the names of the products whose rules (and elements)
        // we need to import:
        std::string import_name;
        if(pname.size()) {
            import_name = pname;
        } else if(from.rules.size()) {
            // no particular production specified.  import the
            // default (first) production.
            import_name = from.rules[0].product();
        }

        // get the list of all products which we'll need to
        // import (i.e. the one we wanted to import, plus
        // anything needed to generate that)
        std::set<std::string> wanted = from.dependent_products(import_name);

        // ... and now import the relevant rules.
        // NOTE that we import the rules IN ORDER because
        // changing the rule order changes precedence.
        int num_imported = 0;
        for(auto rule : from.rules) {
            if(wanted.count(rule.product()) > 0) {
                push_rule(rule);
                num_imported++;
            }
        }

        if(num_imported <= 0) {
            warn(
                stringformat("No rules imported for {} from {}",
                import_name, from.inp->filename())
            );
        }

        return import_name;
    }

    // returns the name of the function to use for the given state
    std::string state_fn(int state_num, bool fully_qualified = false) const {
        std::string fn("state_");

        fn += std::to_string(state_num);

        if(fully_qualified)
            fn = fq_member_name(fn);

        return fn;
    }

    std::string state_fn(
        const std::string &state_id,
        bool fully_qualified = false,
        src_location caller = CALLER()
    ) const {
        auto state_numi = state_index.find(state_id);

        if(state_numi != state_index.end())
            return state_fn(state_numi->second, fully_qualified);

        std::string error = stringformat(
            "{}: unindexed state for {}", caller, state_id
        );
        internal_error(error);
        return "\n#error CANT GET HERE " + error + "\n";
    }

    std::string state_fn(
        const lr_set &state,
        bool fully_qualified = false,
        src_location caller = CALLER()
    ) const {
        return state_fn(state.id(), fully_qualified, caller);
    }

    std::string rule_fn(const production_rule rule, bool fq = false) const {
        std::string fn = rule.rule_fn();

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
    std::string debug_single_step_code(const lr_set &st) const {
        std::string out;
        if(opts.debug) {
            out += "fprintf(stderr, \"%s\", base_parser.to_str().c_str());\n";
            if(opts.single_step) {
                out += "base_parser.debug_pause();\n";
            }
        }
        return out;
    }

    std::string rule_meta_argname(const production_rule &rule) const {
        std::string out =
            "static const char *argname(unsigned int argi) {\n"
            "    static const char *an[] = {\n"
        ;

        for(int sti = 0; sti < rule.num_steps(); ++sti) {
            if(const production_rule::step *st = rule.nth_step(sti)) {
                if(sti > 0) out += ", ";
                out += "    \"" + st->variable_name() + "\"\n";
            }
        }
        out += "    };\n";
        out += "    if(argi < " + std::to_string(rule.num_steps()) + ") {\n";
        out += "        return an[argi];\n";
        out += "    } else {\n";
        out += "        return \"arg index out of bounds\";\n";
        out += "    }\n";
        out += "}\n";
        return out;
    }

    #define rule_meta_str(mem) \
        std::string("static const char *" #mem "() {\n") +\
            "return \"" + c_str_escape(rule.mem()) + "\";\n" \
        "}\n"
    #define rule_meta_int(mem) \
        std::string("static int " #mem "() {\n") +\
            "return " + std::to_string(rule.mem()) + ";\n" \
        "}\n"
    std::string rule_metadata(const production_rule &rule) const {
        std::string out;
        out += "struct {\n";
        out += rule_meta_str(name);
        out += rule_meta_str(product);
        out += rule_meta_int(num_steps);
        out += rule_meta_int(line_number);
        out += rule_meta_str(filename);
        out += rule_meta_str(location);
        out += rule_meta_str(to_str);
        out += rule_meta_argname(rule);
        out += "} this_rule;\n";
        return out;
    }
    #undef rule_meta_int
    #undef rule_meta_str


    code_block reduce_action(const production_rule &rule) const {
        /*
          A given rule will be reduced according to (in priority order):
          1) abstracted implementations (+product). this is top
             priority so that you can use the grammer defined by
             non-"pure" fpl and just override anything you need to
             without having to change the grammar fpl
             (reducer_for(...))
          2) code defined in the rule
          3) @default_action
          4) folding rules with only one step (i.e. treating them
             as aliases)
          If none of these apply, there's no code for the rule,
          and the caller will handle it.
         */
        code_block rule_code = rule.reduce_code();

        // (this is the "none of these apply" case:)
        if(!rule_code) return rule_code;

        std::string out = reducer_decl(rule) + " {\n";
        out += "// " + rule.to_str() + "\n";
        out += rule_metadata(rule) + "\n";
        out += "SourcePosition start_pos = args[0].position();\n";
        out += "SourcePosition end_pos = base_parser.position();\n";
        out += rule_code.format(false);
        out += "\n}\n";
        return code_block(out);
    }

    // returns code for reduce within a given state
    std::string production_code(
        const lr_set &state,
        int rule_ind
    ) const {
        std::string out;
        const production_rule &rule = rules[rule_ind];

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
            const production_rule::step *expr = rule.nth_step(stind);
            if(!expr) {
                error(rule.location(), stringformat(
                    "Bug: no expression for step {} in {}",
                    stind, rule.to_str()
                ));
            } else {
                out += stringformat(
                    "FPLBP::StackSlice arg_{}(base_parser, {}, {}, pos);\n",
                    stind, element_index.at(expr->gexpr), expr->max_times
                );
            }
        }

        // now one slice for all the arguments (to rule them all)
        out += "FPLBP::StackSlice args(base_parser, pos + 1, frame_start - pos);\n";

        // generates the call to the reduction rule:
        out += "    " + type_for(rule.product()) + " result = " + rule_fn(rule) + "(";
        for(int stind = 0; stind < rule.num_steps(); stind++) {
            const production_rule::step *expr = rule.nth_step(stind);

            if(expr->skip_on_reduce())
                continue;  // Author has specified that this arg isn't passed

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

        if(opts.debug) {
            out += "fprintf(stderr, \"popping %i to %i:\\n%s\\n\", "
                   "frame_start, pos, args.to_str().c_str());\n";
        }

        out += "    base_parser.set_product(Product(result, "
             + rule.product_element().nonterm_id_str()
             + ", args[0].position()));\n";

        // this is what actually pops the stack. note we pop after
        // the reduce (mainly to minimize moves, but also so the
        // stack is more intact for error/bug analysis)
        out += "base_parser.lr_pop_to(pos);\n";

        return out;
    }

    std::string state_goto(const lr_set &in, const grammar_element &sym) {
        std::string out;
        return out;
    }

    std::string args_for_shift(
        int next_state, const production_rule::step &expr
    ) const {
        std::string el_id(std::to_string(element_index.at(expr.gexpr)));
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

    void reduce_reduce_conflict(int r1, int r2) const {
        warn(stringformat(
            "reduce/reduce conflict:\n    {} line {}\n vs {} at {}\n",
            rules[r1].to_str().c_str(), rules[r1].location().c_str(),
            rules[r2].to_str().c_str(), rules[r2].location().c_str()
        ));
    }

    // reports what is probably a shift/shift conflict,
    // which would probably only happen due to a bug in
    // this program...
    void other_conflict(lr_item item1, lr_item item2) const {
        warn(stringformat(
           "conflict:\n    {}\n vs {}\n",
           item1.to_str(this).c_str(), item2.to_str(this).c_str()
        ));
    }

    std::string code_for_shift(const lr_transition &trans) const {
        const production_rule::step *right_of_dot = trans.right_of_dot;
        if(!right_of_dot)  // if this happens, it's a bug
            internal_error("missing right_of_dot?");

        int next_state = trans.next_state_number;
        std::string out;

        const grammar_element::Type type = right_of_dot->type();
        switch(type) {
            case grammar_element::TERM_EXACT:
                out += "} else if(base_parser.shift_exact(";
                out += args_for_shift(next_state, *right_of_dot) + ")) {\n";
                break;
            case grammar_element::TERM_REGEX:
                out += "} else if(base_parser.shift_re(";
                out += args_for_shift(next_state, *right_of_dot) + ")) {\n";
                break;
            case grammar_element::NONTERM_PRODUCTION:
                out += "} else if(base_parser.shift_nonterm(";
                out += args_for_shift(next_state, *right_of_dot) + ")) {\n";
                break;
            case grammar_element::LACK_OF_SEPARATOR:
                // (b_eaten is number of separator bytes "eaten"
                // since last terminal)
                out += "} else if(!b_eaten) {\n";
                // OK the problem here is that we _do_ need to shift the state,
                // even though we do not need to shift the lack of separator per se.
                // this makes me wonder about going back to recursive ascent
                // (or another dual stack solution)
                out += "FPLBP::Terminal term(\"<special ~>\");\n"; // XXX this is terrible
                out += stringformat(
                    "base_parser.lr_push(&{}, FPLBP::Product(term, {}, base_parser.position()));\n",
                    state_fn(next_state, true), element_index.at(right_of_dot->gexpr)
                );
                break;
            default:
                // if we get here, it's due to a bug:
                internal_error(stringformat(
                    "Missing/unknown grammar element (id: {} {})",
                    type, right_of_dot->to_str()
                ));
                break;
        }

/*
        out += "    // transition ID: " + transition_id(
            *right_of_dot, lr_goto(state, right_of_dot->gexpr)
        ) + "\n";
 */

        if(opts.debug) {
            out += "fprintf(stderr, \"     shifted %s\\n\", \""
                   + c_str_escape(right_of_dot->to_str()) +
                   "\");\n";
        }

        return out;
    }

    // returns an iterable set of transitions out of the state
    // passed.  use this for code generation.
    lr_transitions transitions_for_state(const lr_set &state) const {
        lr_transitions out;

        //   NOTE:  if there's a shift/reduce conflict, we will resolve it:
        //      - first by longest match (i.e. shift instead of reducing).
        //        example:  if() ...  vs if() ... else ...;
        //        if() .. else is longer so we shift.
        //      - next by operator precedence.. XXX implement
        std::map<int, int> transition; // grammar element id -> state number
        std::map<int, lr_item> item_for_el_id;
        std::vector<lr_item> optionals;
        for(lr_item item : state.iterable_items()) {
            const production_rule &rule = rules[item.rule];
            const production_rule::step *right_of_dot = rule.nth_step(item.position);
            if(!right_of_dot) {
                lr_item reduce_item = state.reduction_item(this);
                if(item != reduce_item) {
                    reduce_reduce_conflict(item.rule, reduce_item.rule);
                }
            } else {
                lr_set next_state = lr_goto(state, right_of_dot->gexpr);
                int next_state_num = state_index.at(next_state.id());
                int el_id = element_index.at(right_of_dot->gexpr);

                auto existing = transition.find(el_id);
                if(existing != transition.end()) {
                    if(existing->second != next_state_num) {
                        // ... shift/shift conflict, which makes no sense,
                        // and afaict means there's a bug someplace:
                        other_conflict(item, item_for_el_id.at(existing->first));
                    }
                } else {
                    out.push_back(lr_transition(right_of_dot, next_state_num));
                }

                if(right_of_dot->is_optional())
                    optionals.push_back(item);

                transition[el_id] = next_state_num;
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
            warn(stringformat(
                "Ambiguity in {}:\n{}\n",
                state_fn(state), which
            ));
        }

        return out;
    }

    std::string code_for_state(const lr_set &state) const {
        std::string out;
        std::string sfn = state_fn(state);

        out += "//\n";
        out += state.to_str(this, "// ");
        out += "//\n";
        out += "void " + sfn + "() {\n";
        out += "size_t b_eaten = eat_separator();\n";
        if(opts.debug) {
            out += "fprintf(stderr, \"%li bytes eaten since last terminal\\n\", ";
            out += "b_eaten);\n";
        }

        out += debug_single_step_code(state);

        out += "    if(0) {\n"; // now everything past this can be "else if"

        for(auto trans : transitions_for_state(state)) {
            // } else if(...) { ... " shift/state transitions:
            out += code_for_shift(trans);
        }
        out += "} else {\n";

        lr_item reduce_item = state.reduction_item(this);
        if(reduce_item) {
            if(opts.debug) {
                out += "fprintf(stderr, \"    " + sfn +
                       " is going to reduce to a %s\\n\", \"" +
                       c_str_escape(reduce_item.to_str(this)) + "\");\n";
            }
            out += production_code(state, reduce_item.rule);
        } else {
            if(opts.debug)
                out += "fprintf(stderr, \"    terminating in " +
                       sfn + "\\n\");\n";

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

    // returns an enum string for nonterminals.
    // this makes the generated code easier to read - eg
    // instead of "if(prd.grammar_element_id == 62)" we can
    // do "if(prd.grammar_element_id == NontermID::_decimal_constant)"
public:
    std::string nonterm_enum() const {
        std::string out;
        std::string nonterm_str_guts;
        std::string is_terminal_guts;

        out += "typedef enum {\n";
        for(int el_id = 0; el_id < elements.size(); ++el_id) {
            is_terminal_guts += "case " + std::to_string(el_id);
            if(elements[el_id].type == grammar_element::NONTERM_PRODUCTION) {
                std::string name = elements[el_id].nonterm_id_str(false);
                out += name;
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

    std::string state_to_str_code() const {
        std::string out("static std::string state_to_str(State st) {\n");
        out += "if(!st) return \"NULL\";\n";
        // c++ won't let you compare pointers in a switch statement.. sigh
        for(auto st: states) {
            out += "if(&" + state_fn(st, true) + " == st) ";
            out += "return \"" + state_fn(st) + "\";\n";
        }
        out += "    return \"<not a state>\";\n";
        out += "}\n";

        return out;
    }

    std::string state_string_code() const {
        std::string out("static const char *state_string(State st) {\n");
        for(auto st: states) {
            out += stringformat(
                "    if(&{} == st) return \"{}:\\n\"\n{};\n",
                state_fn(st, true), state_fn(st, false),
                st.to_str(this, "\"    ", "\\n\"\n", true)
            );
        }
        out += "    return \"<invalid state>\";\n";
        out += "}\n";
        return out;
    }

    // returns the code for the target is_goal function
    std::string is_goal() const {
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
    code_block separator_method() const {
        code_block out(
            "static size_t separator_length(const utf8_byte *inp) {\n"
        );

        if(separator_code.size() == 0) {
            // default is space separation:
            out += code_block("return space_length(inp);\n");
        } else {
            for(auto sepc : separator_code) {
                out += sepc.format_scoped();
            }
        }

        // catchall: if nothing returned a length yet, no separator
        out += "    return 0;\n";
        out += "}\n";

        return out;
    }

    std::string eat_separator_code() const {
        return (
            "size_t eat_separator() {\n"
            "    return base_parser.eat_separator(separator_length);\n"
            "}\n"
        );
    }

    std::string parser_class_name() const {
        std::string base;
        for(auto chr : inp->base_name()) {
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
    std::string fq_member_name(const std::string &mem) const {
        return parser_class_name() + "::" + mem;
    }

    std::string default_main_code() const {
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
        out += "    fpl_reader_p inp = std::make_shared<fpl_reader>(argv[1]);\n";
        out +=      parser_class_name() + " parser(inp);\n";
        out += "    using namespace std;\n";
        out += "    auto result = parser.parse();\n";
        //out += "    printf(\" %s\\n\", to_string(result).c_str());\n";
        //out += "    fprintf(stderr, \"parser state:\\n%s\\n\", parser.to_str().c_str());\n";
        out += "    return parser.error_count()?-1:0;\n";
        out += "}\n\n";

        return out;
    }

    void clear_states() {
        states.clear();
        state_index.clear();
    }

    void generate_states(const std::list<std::string> &wanted) {

        if(rules.empty()) {
            error("No rules found\n");
        }

        clear_states();

        lr_set entry_set;
        for(auto entry_prod : wanted) {
            auto strl  = rules_for_product.lower_bound(entry_prod);
            auto endrl = rules_for_product.upper_bound(entry_prod);
            if(strl == endrl) {
                error(stringformat(
                    "Can't find rule for goal '{}'\n", entry_prod
                ));
            }
            for(auto rit = strl; rit != endrl; ++rit) {
                entry_set.add_expanded(
                    lr_item(rit->second, 0), rules.at(rit->second)
                );
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

    // returns the declaration for a reduce function
    // (possibly this can go in reducer)
    std::string reducer_decl(const production_rule &rule) const {
        std::string rfn = rule_fn(rule);
        std::string out;
        out += type_for(rule.product()) + " " + rfn + "(";

        // "simple" parameters:
        // If the expression for the step has a min/max times of
        // anything other than exactly 1 (is_single()), we pass
        // it as a stack slice.
        // Otherwise, if it's a terminal, pass it as a string,
        // or (if it's the result of a reduce) as whatever the reduce
        // type is.  This is complicated for the code generator,
        // but simplifies life for the fpl author, especially
        // for trivial cases.
        int argind = 0;
        const std::vector<production_rule::step> &steps = rule.steps();
        for(int stind = 0; stind < steps.size(); stind++) {
            const production_rule::step &expr = steps[stind];

            // this expression might be suffixed with '^', which
            // means it's only used for recognizing, and is not
            // passed to the reduce function:
            if(expr.skip_on_reduce())
                continue;

            // arg type depends on the expression:
            // XXX consider if we always should just pass stack element
            // for singles and slices for multiples.  stack element needs
            // to be able to behave as whatever, then.
            if(!expr.is_single()) {
                // the argument is either optional or can repeat or
                // both, so we can't pass it simply.  so pass it
                // as a slice:
                out += "const FPLBP::StackSlice &";
            } else if(expr.is_terminal()) {
                // TODO probably want to allow multiple regex captures..?
                // XXX _must_ do so.
                out += "std::string ";
            } else {
                out += reduce_type + " ";
            }

            // argument name:
            out += rule.varname(stind);
            out += ", ";

            argind++;
        }

        // last parameter is the slice of the stack with
        // everything we're popping.  this lets the fpl author
        // get things like the line number for a given argument
        // or whatever.  (actully, the "simple" positional
        // parameters below can be implemented via this,
        // and in the jest version might be).  It's last only
        // because that simplifies the generating code
        out += "const FPLBP::StackSlice &args)";

        return out;
    }

    std::string why_cant_use_reducer(const reducer &red, const production_rule &rule) {
        if(red.name() != rule.product()) {
            return stringformat(
                "different products ({} vs {})",
                red.name(), rule.product()
            );
        }

        // check if there are any variables in the reducer which
        // don't match steps in the rule:
        std::set<std::string> unknown_vars = red.required_arguments();
        for(int stepi = 0; stepi < rule.num_steps(); ++stepi) {
            const production_rule::step *step = rule.nth_step(stepi);
            if(step) {
                // this one matches, so remove it from the set:
                unknown_vars.erase(step->variable_name());
            }
        }
        int num_unk = unknown_vars.size();
        if(num_unk > 0) {
            std::string unks;
            bool did = false;
            for(auto unk : unknown_vars) {
                if(did) unks += ", ";
                unks += unk;
                did = true;
            }
            return stringformat("rule has no steps matching '{}'", unks);
        }

        // no reason this can't be a match!
        return "";
    }

    // Returns the number of matching parameters.
    // This is used to determine which rule the reducer best fits.
    // Note that this is separate from the check which sees if
    // the reducer can fit the rule at all - a given rule/reducer
    // might have a righ match count, but still be incompatible
    // for one reason or another.
    int matchcount(const reducer &red, const production_rule &rule) const {
        int cnt = 0;

        for(int stepi = 0; stepi < rule.num_steps(); ++stepi) {
            const production_rule::step *step = rule.nth_step(stepi);
            // what if there's an unnamed step?
            // this will "just work", but will it be
            // confusing?
            if(step && red.argument_matches(step->variable_name()))
                cnt++;
        }
        return cnt;
    }

    void apply_reducers() {
        for(auto red: reducers) {
            std::string why_no_match;

            const std::string &pname = red.name();
            auto rulei_0 = rules_for_product.lower_bound(pname);
            auto rulei_l = rules_for_product.upper_bound(pname);
            bool used = false;
            for(auto rit = rulei_0; rit != rulei_l; ++rit) {
                production_rule &rule = rules[rit->second];

                std::string why_not = why_cant_use_reducer(red, rule);
                if(why_not.length()) {
                    why_no_match += stringformat(
                        "\n        {}: {}", rule.to_str(), why_not
                    );
                    continue;
                }

                int existing_mc = -1;
                reducer existing = rule.abstracted_reducer();
                if(existing)
                    existing_mc = matchcount(existing, rule);

                int mc = matchcount(red, rule);
                if(mc >= existing_mc) {
                    // this reducer matches at least as well as
                    // whatever's there:
                    used = true;
                    rule.set_reducer(red);

                    // it would be nice to know if we accidentally
                    // overrode an existing reducer, ... hmm maybe...grrr
                    if(mc == existing_mc) {
                        warn(stringformat("{} overrides equally good {} on {}",
                            red.to_str(), existing.to_str(), rule.to_str()
                        ));
                    }
                }
            }

            // note this _won't_ warn if we just overwrote an existing
            // reducer to the point where it's no longer used... hmmm..
            if(!used) {
                if(rulei_0 == rulei_l) {
                    why_no_match = stringformat("nothing produces {}", pname);
                }
                warn(stringformat(
                    "reducer {} doesn't match any rules: {}\n",
                    red.to_str(), why_no_match
                ));
            }
        }
    }

    // returns a string containing one possible reducer
    // declaration for the rule passed.  used for giving
    // fpl authors a hint about what to declare.
    std::string hypothetical_reducer(const production_rule &rule) const {
        std::string out("+");
        out += rule.product() + "(";
        auto steps = rule.steps();
        int term_name = 0;
        int params = 0;
        for(int sti = 0; sti < steps.size(); sti++) {
            auto step = steps[sti];
            if(!step.skip_on_reduce()) {
                if(params > 0)
                    out += " ";

                if(step.varname.size())
                    out += step.varname;
                else if(step.is_terminal())
                    out += stringformat("term_{}", term_name++);
                else // XXX this can collide
                    out += stringformat(step.production_name());
                params++;
            }
        }
        out += ")";
        return out;
    }

    // determines goal(s), generates states, matches up reducers, etc.
    // call this before generating code.
    void resolve(src_location caller = CALLER()) {
        if(rules.size() <= 0) {
            internal_error("No rules found\n", caller);
        }

        goal = opts.entry_points;
        if(goal.empty()) {
            // no particular goal products specified, so we default
            // to whatever the first rule produces:
            goal.push_back(rules[0].product());
        }

        apply_reducers();
        generate_states(goal);
        report_unused_rules();

        std::list<std::string> missing_actions;
        for(int rnum = 0; rnum < rules.size(); rnum++) {
            const production_rule &rule = rules[rnum];
            if(!rule.reduce_code()) {
                missing_actions.push_back(stringformat(
                    "{}\t{}\n", rule.location(), hypothetical_reducer(rule)
                ));
            }
        }
        if(missing_actions.size() > 0) {
            std::string msg = stringformat(
                "missing reduce action for {} rules:\n"
                "    original rule location\tdesired reducer\n",
                missing_actions.size()
            );
            for(std::string rmsg : missing_actions) {
                msg += "    " + rmsg;
            }
            error(msg);
        }
    }

    // jemp-based output:
    friend std::string fpl_x_parser(const productions &, const options &);

    std::string generate_code(src_location caller = CALLER()) {
        resolve(caller);
        return reformat_code(fpl_x_parser(*this, opts), opts.output_fn);
    }

    // debugging:
    void dump_states() {
        for(int stind = 0; stind < states.size(); stind++) {
            printf("state %i:\n%s\n",
                stind, states[stind].to_str(this, "    ").c_str()
            );
        }
    }

    void report_unused_rules() const {
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
                const production_rule &rule = rules[rind];
                warn(stringformat(
                    "Rule {} producing {} at {} is unused\n",
                    rind, rule.product(), rule.location()
                ));
            }
        }
    }
};

#include "fpl_x_parser.h"

} // namespace fpl

#endif // PRODUCTIONS_H
