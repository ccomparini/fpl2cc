#ifndef PRODUCTIONS_H
#define PRODUCTIONS_H

#include "code_block.h"
#include "fpl_options.h"
#include "production_rule.h"
#include "reducer.h"

#include "util/c_str_escape.h"
#include "util/jerror.h"
#include "util/reformat_code.h"
#include "util/src_location.h"
#include "util/stringformat.h"
#include "util/to_hex.h"

#include <list>
#include <map>
#include <string>
#include <time.h> // for profiling

namespace fpl {

class productions;
std::string fpl_x_parser(const productions &, const fpl_options &);

class productions {

    fpl_reader_p  inp;
    const fpl_options opts;

    // for imports and such:
    using subgrammar_p = std::unique_ptr<productions>;
    std::map<std::string, subgrammar_p> sub_productions; // grammar name -> productions
    std::set<std::string> exported_products; // exported to that of which this is a sub

    std::string reduce_type; // default reduce type
    std::map<std::string, std::string> type_for_product; // (reduce type for particular product)
    std::set<std::string> all_types; // for deduplication

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
    std::map<std::string, code_block> scanners;

    std::vector<production_rule>    rules;
    std::multimap<std::string, int> rules_for_product; // product -> rule ind
    int anon_product_count; // used to generate unique ids for anonymous rules

    std::vector<grammar_element>    elements;
    std::map<grammar_element, int>  element_index;

    std::list<reducer> reducers;

    class lr_set;

    std::map<int32_t, lr_set> states;  // keyed by state number
    std::map<std::string, int32_t> state_index; // set id -> state number

    static void warn(const std::string &msg) { jerror::warning(msg); };

    static std::string format_error_message(
        const std::string &location,
        const std::string &msg
    ) {
        const char *nl = "";
        if(msg[msg.length() - 1] != '\n')
            nl = "\n";

        return stringformat("Error {}: {}{}", location, msg, nl);
    }

    // report an error relative to the file for the reader passed
    static void error(
        const fpl_reader &rdr, size_t pos, const std::string &msg
    ) {
        std::string location = stringformat(
            "{} near «{}»", rdr.location_str(pos), rdr.debug_peek(pos, 12)
        );
        jerror::error(format_error_message(location, msg));
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

    // report an error at a possibly-not-input-file location:
    static void error(const std::string &where, const std::string &msg) {
        jerror::error(format_error_message(where, msg));
    }

    // call this if there's a fatal bug detected at execution time:
    static void internal_error(
        const std::string &msg, src_location caller = CALLER()
    ) {
        jerror::error(stringformat("Internal error: {} at {}", msg, caller));
        exit(1);
    }

    // records the fact that the given grammar element exists
    void record_element(const grammar_element &nge) {
        if(element_index.find(nge) == element_index.end()) {
            element_index[nge] = elements.size();
            elements.push_back(nge);
        }
    }

    // this is the name of the element for purposes of (eg) enums
    std::string element_id_name(int el_ind) const {
        const grammar_element el = elements[el_ind];
        if(el.is_nonterminal()) {
            return el.nonterm_id_str();
        }
        return stringformat("_terminal_{}", el_ind);
    }

    std::string element_id_name(grammar_element el) const {
        return element_id_name(element_index.at(el));
    }

    struct lr_item {
        // Modified from classic lr - in our case,
        // there's no "or" in rules so we can just
        // refer to the rule itself.  So each item
        // can be encoded as a rule ID and the step
        // within that rule:
        uint16_t rule;     // offset into rules (i.e. rule number)
        uint16_t countdown; // offset from end of rule; 0 == reduce

        static const uint16_t no_rule = 0xffff;

        // this allows lr_items to be used as keys in things like
        // std::map, as well as helping determine precedence by
        // putting earlier rules earlier.
        friend bool operator<(const lr_item& left, const lr_item& right) {

            // same rule?  earlier position in the rule
            // comes first.
            if(left.rule == right.rule)
                return right.countdown < left.countdown;

            // otherwise items from earlier rules come earlier.
            return left.rule < right.rule;
        }

        // "false" indicates an invalid or non-item.
        operator bool() const {
            return rule != no_rule;
        }

        // constructs a false item:
        lr_item() : rule(no_rule), countdown(0) { }

        lr_item(int rl, int ctd) : rule(rl), countdown(ctd) { }

        // returns an lr_item representing the item following
        // this one, or a false lr_item if we're at the end of
        // the rule.
        lr_item next_in_rule() const {
            if(countdown > 0) {
                return lr_item(rule, countdown - 1);
            }

            return lr_item();
        }

        static lr_item from_id(uint32_t id) {
            return lr_item((id >> 16) & 0xffff, id & 0xffff);
        }

        uint32_t id() const {
            return rule << 16 | countdown;
        }

        // returns the rule step which this item refers to,
        // which will be false in the case that it's the end
        // of (or past the end of) the rule.
        production_rule::step step(const productions &prds) const {
            return prds.rules[rule].nth_from_end(countdown);
        }

        std::string to_str(
            const productions *prds = nullptr,
            const lr_set *state = nullptr
        ) const {
            // we can always show (rule number):(how many steps are left):
            std::string out = stringformat("({}:{})", rule, countdown);

            if(prds) {
                // if we have a suitable products pointer, we can
                // also show what this item is used to produce and
                // where it is in the rule:
                const production_rule &rl = prds->rules[rule];
                out = stringformat("{} {}:\t", rl.product(), out);

                for(int stepi = rl.num_steps(); stepi >= 0; --stepi) {
                    if(stepi == countdown) out += " •";
                    else                   out += " ";

                    production_rule::step step = rl.nth_from_end(stepi);
                    if(step)
                        out += step.to_str();
                }

                if(state) {
                    // .. and if we also know the state for the rule, we
                    // can show what to do after matching the item:
                    int32_t next_stn = state->next_state(step(*prds));
                    if(next_stn >= 0) {
                        out += stringformat("\t=> state {}", next_stn);
                    } else {
                        out += stringformat("\t=> (reduce)");
                    }
                }
            }

            return out;
        }
    };

    struct lr_transition {
        grammar_element right_of_dot;
        int32_t         next_state_number;
        lr_transition(const grammar_element &ge, int32_t ns)
            : right_of_dot(ge), next_state_number(ns) {
        }
    };

    // an lr_set is a set of lr items.
    // each state is represented by an lr_set.
    class lr_set {
        mutable std::string _id_cache;
        std::set<lr_item> items;
        std::map<grammar_element, int32_t> next_state_for_el;
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
                _id_cache = bs_to_hex(items, "-");
            }
            return _id_cache;
        }

        const std::set<lr_item> &iterable_items() const {
            return items;
        }

        int size() const {
            return items.size();
        }

        int32_t next_state(const grammar_element &ge) const {
            auto nsi = next_state_for_el.find(ge);
            if(nsi != next_state_for_el.end()) {
                return nsi->second;
            }
            return -1;
        }

        int32_t next_state(const production_rule::step &st) const {
            return next_state(st.gexpr);
        }

        // returns an iterable set of transitions out of the state
        // passed.  use this for code generation.
        using lr_transitions = std::vector<lr_transition>;
        lr_transitions transitions(const productions &prds) const {
            lr_transitions out;
            out.reserve(next_state_for_el.size());

            for(lr_item item : iterable_items()) {
                 if(item.countdown > 0) {
                     auto step = item.step(prds);
                     out.push_back(lr_transition(
                         step.gexpr, next_state(step)
                     ));
                 }
            }
            return out;
        }
 
        void add_item(const lr_item &it) {
            items.insert(it);
        }

        // Recorsively adds all items for whatever can come after the
        // item passed.  This includes handling repetition, optionalness,
        // and items needed to match the start of nonterminals.
        // Callers will need to deal with handling multiple-match items,
        // since this only adds to this state (and multiple-match requires
        // adding to the next state).
        void add_expanded(const lr_item &it, const productions &prds) {
            if(!it) return;


            const production_rule &rule = prds.rules[it.rule];

            for(int ctd = it.countdown; ctd >= 0; ctd--)  {
                production_rule::step expr = rule.nth_from_end(ctd);
                add_item(lr_item(it.rule, ctd));

                // if it's a nonterm, we need to add each thing which
                // can start that nonterm so that the generated state
                // can correctly recognize things which will make that
                // nonterm:
                if(expr.is_nonterminal()) {
                    const std::string &pname = expr.production_name();
                    auto strl  = prds.rules_for_product.lower_bound(pname);
                    auto endrl = prds.rules_for_product.upper_bound(pname);
                    if(strl == endrl) {
                        error(rule.location(), stringformat(
                            "Nothing produces '{}'\n", pname
                        ));
                    }
                    for(auto rit = strl; rit != endrl; ++rit) {
                        const production_rule &prule = prds.rules[rit->second];
                        lr_item start_item(rit->second, prule.num_steps());
                        if(items.count(start_item) == 0) {
                            // recursively add rule starts:
                            add_expanded(
                                lr_item(rit->second, prule.num_steps()), prds
                            );
                        }
                    }
                }

                // last, if the current step is optional, we'll look
                // at the next step.  otherwise, we're done:
                if(!expr.is_optional())
                    break;
            }
        }

        // returns an lr_item representing the reduction for
        // this set/state, or a false-valued lr_item if there's
        // no such reduction.
        lr_item reduction_item() const {
            for(auto it : items) {
                // It's possible that there's more than one item
                // with countdown == 0.  We're not worrying about
                // that here, though - the implied conflict should
                // have been reported when generating states.
                // We do return the first such item, though, to
                // make it so that the conflict is resolved by
                // rule order.
                if(it.countdown == 0) {
                    return it;
                }
            }
            // (no such item)
            return lr_item();
        }

        std::string to_str(
            const productions *prds = nullptr,
            const std::string &line_prefix = "    ",
            const std::string &line_suffix = "\n",
            bool stringescape = false
        ) const {
            std::string out;
            for(auto it : items) {
                out += line_prefix;
                if(stringescape)
                    out += c_str_escape(it.to_str(prds, this));
                else
                    out += it.to_str(prds, this);

                out += line_suffix;

            }
            // columnate the output:
            return stringformat("{::c}", out);
        }

        // returns a multimap of (grammar_element -> lr_item).
        // for use in generating states.
        // items for the ends of rules will be indexed under
        // grammar element type == NONE.
        std::multimap<grammar_element, lr_item> items_per_element(
            const productions &prds
        ) const {
            // inline function?  this is like.. index_items or something
            std::multimap<grammar_element, lr_item> items_for_el;
            lr_item default_item = lr_item();
            for(auto item : items) {
                if(item.countdown == 0) {
                    if(default_item) {
                        prds.conflict(default_item, item, this);
                    } else {
                        default_item = item;
                    }
                    items_for_el.insert(
                        std::make_pair(grammar_element(), item)
                    );
                } else {
                    const production_rule &rule = prds.rules.at(item.rule);
                    auto step = rule.nth_from_end(item.countdown);
                    if(!step) {
                        internal_error(rule.location(), stringformat(
                            "missing step for item {}?",
                            item.to_str(&prds, this)
                        ));
                    }
                    items_for_el.insert(std::make_pair(step.gexpr, item));
                }
            }
            return items_for_el;
        }

        // recursively generates any following states, adding them
        // to the productions passed.
        void generate_states(
            productions &prds,
            src_location caller = CALLER()
        ) {

            if(next_state_for_el.size() > 0)
                return; // (already generated)

            auto items_for_el = items_per_element(prds);
            if(items_for_el.size() == 0) {
                jerror::warning(stringformat(
                    "empty state for set? set: [{}] ({})\n", *this, caller
                ));
                return;
            }

            auto done = items_for_el.end();
            auto trit = items_for_el.begin();
            while(trit != done) {
                // make an lr_set for all the items matching the
                // input element:
                lr_set next_state;
                const grammar_element &cur_el = trit->first;
                for( ; trit != done && trit->first == cur_el; ++trit) {
                    if(cur_el.type != grammar_element::Type::NONE) {
                        const lr_item item = trit->second;
                        auto          step = item.step(prds);

                        // if the item is multiple-match ('*' or '+'
                        // after it), it can follow itself:
                        if(step.is_multiple()) {
                            next_state.add_expanded(item, prds);
                        }

                        next_state.add_expanded(item.next_in_rule(), prds);
                    }
                }

                if(next_state.items.size()) {
                    // (a state for that lr_set may or may not yet exist)
                    if(prds.state_index.count(next_state.id()) > 0) {
                        // the destination state already exists, so use it:
                        next_state_for_el[cur_el]
                            = prds.state_index[next_state.id()];
                    } else {
                        // .. doesn't exist yet.  make it:
                        int32_t new_state_num = prds.add_state(next_state);

                        next_state_for_el[cur_el] = new_state_num;
                        prds.states[new_state_num].generate_states(prds);
                    }
                }
            } // all elements handled
        }
    };

    // Adds lr_items representing the start(s) of each existing rule
    // for the product passed.  Rules may have multiple starts
    // due to optionals - eg 'a'? 'b' -> c could start with 'a'
    // or 'b'.
    // This is used for setting up the initial parsing goals.
    void add_starts_for_product(
        lr_set &set,
        const std::string &pname,
        const std::string &context
    ) const {
        auto strl  = rules_for_product.lower_bound(pname);
        auto endrl = rules_for_product.upper_bound(pname);

        if(strl == endrl) {
            error(context, stringformat(
                "Nothing produces '{}'\n", pname
            ));
        }
        
        for(auto rit = strl; rit != endrl; ++rit) {
            const production_rule &subrule = rules[rit->second];
            set.add_expanded(
                // (items here are always referring to the start
                // of the rule, and since items count down to the
                // end of the rule, start position is num_steps().
                lr_item(rit->second, subrule.num_steps()),
                *this
            );
        }
    }

public:

// XXX this is currently stupid in that you pass the fpl_reader
// but then you have to make a separate call to parse it.  fix that.
    productions(const fpl_options &op, fpl_reader_p src) :
        inp(src), opts(op), default_main(false), anon_product_count(0)
    {
        // element 0 is a null element and can be used to
        // indicate missing/uninitialized elements or such.
        // we count it as a nonterminal so that it can be
        // accessed via the enum etc.
        record_element(
            grammar_element("_fpl_null", grammar_element::NONTERM_PRODUCTION)
        );

    }

    // if there's only one way to make a given product, the
    // type for that product can be "inherited" from the way
    // to make it.
    // this is used for type inferrence in cases where there's
    // no explicit type for a given product.
    std::string inherited_type(const std::string &product) const {
        auto strl  = rules_for_product.lower_bound(product);
        auto endrl = rules_for_product.upper_bound(product);

        std::string in_type;

        for(auto rit = strl; rit != endrl; ++rit) {
            const production_rule &rule = rules[rit->second];
            const std::string alias = rule.potential_type_alias();
            if(in_type == "") {
                in_type = alias;
            } else if(in_type != alias) {
                // apparently there's more than one way to make
                // this product, so we can't simply alias
                return "";
            }
        }

        return in_type;
    }

    // returns the name of the type expected as the result
    // of reducing to the product type indicated:
    std::string type_for(const std::string &product) const {
        auto tf = type_for_product.find(product);
        if(tf != type_for_product.end()) {
            return tf->second;
        }

        // if we are produced from only one other production,
        // we can use that type.  (terminals too? terminal aliasing?)
        std::string inherited = inherited_type(product);
        if(inherited != "")
            return type_for(inherited);

        // last resort, use the @produces type. (possibly this part
        // could be implemented using the inheritance, above)
        return reduce_type;
    }

    // and this returns the type to use for a particular grammar
    // element, which covers terminals as well:
    std::string type_for(const grammar_element &ge) const {
        if(ge.is_terminal()) {
            return "Terminal";
        }
        // there's no type for assertions, presently.  bool
        // would make sense, but it's already covered by the
        // fact of a match so ... hmm optional assertions?
        // anyway at this point assume it's a production:
        return type_for(ge.expr);
    }

// XXX maybe fix the code and then this comment:
    // expects/scans a +{ }+ code block for the named directive.
    // the named directive is essentially for error reporting.
    // the intent is that at some point this can scan regexes
    // or whatever more portable thing.
    enum code_source{
        INLINE = 1,
        LIB    = 2,
        REGEX  = 4,

        INLINE_OR_LIB = INLINE | LIB,
        ANY           = INLINE | LIB | REGEX
    };
    inline code_block code_for_directive(
        const std::string &dir, code_source allowed_src = INLINE
    ) {

        code_block code;
        if(allowed_src | INLINE) {
            // try "regular" +{ }+ code blocks:
            code = read_code(*inp);
        }

// XXX oh yeah hey uhh... this used?  try it!
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
// XXX this error is never seen
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

    void set_reduce_type(const std::string &rt) {
        all_types.insert(rt);
        reduce_type = rt;
    }
    void set_default_action(const std::string &rt) { default_action = rt; }
    void set_post_parse(const code_block &cb)      { post_parse = cb; }
    void set_post_reduce(const code_block &cb)     { post_reduce = cb; }
    void set_default_main(bool def)                { default_main = def; }
    void add_internal(const code_block &cb)        { parser_members.push_back(cb); }

    void add_type_for(const std::string &prod, const std::string &type) {
        type_for_product[prod] = type;
        all_types.insert(type);
    }

    void parse_scanner() {
        std::string name = read_identifier(*inp);
        if(name == "") {
            error("expected name of scanner");
        }

        if(scanners.count(name)) {
            const code_block &existing = scanners[name];
            warn(stringformat(
                "scanner {} overwrites existing scanner at {}\n",
                name, existing.location()
            ));
        }

        scanners[name] = code_for_directive("scanner", code_source::ANY);
    }

    std::string arg_for_directive() {
        // reads an argument to the end of the line.
        // end of line is any ascii vertical space (for now).
        // leading and trailing spaces/tabs are stripped
        return inp->read_re("[ \\t]*([^@\\x0a-\\x0d]+)[ \\t]*\n")[1];
    }

    void add_goal(const std::string raw_goal) {
        // the goal is either simply a production name,
        // or grammar_name.production_name.  so see which:
        size_t grammar_end = raw_goal.find('.');
        if(grammar_end != std::string::npos) {
            // if the dot is at the start or end, we're missing
            // either the production or the grammar:
            if(grammar_end >= raw_goal.length() || grammar_end == 0) {
                error(stringformat("invalid goal '{}'\n", raw_goal));
            }

            std::string grammar_name = raw_goal.substr(0, grammar_end);
            std::string production   = raw_goal.substr(grammar_end + 1);
            import_grammar(subgrammar(grammar_name), production);
            goal.push_back(production);
        } else {
            // it's a simple goal name:
            goal.push_back(raw_goal);
        }
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
        } else if(dir == "goal") {
            std::string newgoal = arg_for_directive();
            if(!newgoal.length()) {
                error("can't parse the @goal");
            } else {
                add_goal(newgoal);
            }
        } else if(dir == "grammar") {
            // import the grammar from another fpl:
            import_grammar(subgrammar(arg_for_directive()));
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
            set_reduce_type(arg_for_directive());
        } else if(dir == "scanner") {
            parse_scanner();
        } else if(dir == "separator") {
            add_separator_code(
                code_for_directive(dir, code_source::INLINE_OR_LIB)
            );
        } else if(dir == "type_for") {
            inp->eat_separator();
            std::string prod = read_production_name(*inp);
            inp->eat_separator();
            std::string type = inp->read_re(".*")[0];
            if(prod.length() && type.length()) {
                add_type_for(prod, type);
            } else {
                error("type_for expects <product name> <type>");
            }
        } else {
            error(stringformat("Unknown directive: '{}'\n", dir));
        }
    }

    void add_rule(production_rule &rule) {
        int rule_num = rules.size();
        rule.set_rule_number(rule_num);

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
            record_element(rule.nth_step(stp).gexpr);
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
        // .. so anyway I guess we're just finding fold candidates here.
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
                expr.qty.optional = true;
                expr.qty.multiple = true;
                src.skip_bytes(1);
                break;
            case '+':
                expr.qty.optional = false;
                expr.qty.multiple = true;
                src.skip_bytes(1);
                break;
            case '?':
                expr.qty.optional = true;
                expr.qty.multiple = false;
                src.skip_bytes(1);
                break;
            default:
                expr.qty.optional = false;
                expr.qty.multiple = false;
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

    // returns true or false
    static inline bool is_production_name_char(const char ch) {
        return (ch == '_')              ||
               (ch >= 'A' && ch <= 'Z') ||
               (ch >= 'a' && ch <= 'z') ||
               (ch >= '0' && ch <= '9');
    }

    // identifiers must start with a letter, and thereafter may
    // contain letters, digits, or underscores.
    static inline std::string read_identifier(fpl_reader &src) {
        std::cmatch nm = src.read_re("[a-zA-Z][a-zA-Z_0-9]*");
        if(!nm.length())
            return "";
        return nm[0];
    }

    static inline std::string read_production_name(fpl_reader &src) {
        return read_identifier(src);
    }

    static inline std::string read_directive(fpl_reader &src) {
        return src.read_re("([A-Za-z][A-Za-z0-9_]+)\\s*")[1];
    }

    inline std::list<std::string> imported_files() const {
        // hopefully the compiler inlines this and doesn't copy all
        // these strings...
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

    // Loads (and parses) the subgrammar specifier, or fetches
    // it from cache. Does not import anything from the subgrammar - 
    // the idea here is basically to abstract the grammar name
    // from the source, and to implement caching so that if you
    // want to import multiple pieces of a given subgrammar, we
    // don't have to read and parse the source for that multiple
    // times.
    productions *subgrammar(const std::string &grammar_name) {
        productions *out = nullptr;

        auto exsp = sub_productions.find(grammar_name);
        if(exsp != sub_productions.end()) {
            out = exsp->second.get();
        } else {
            subgrammar_p sub = make_unique<productions>(
                opts, open_for_import(grammar_name)
            );
            sub->parse_fpl();
            out = sub.get();
            sub_productions[grammar_name] = std::move(sub);
        }

        return out;
    }

    // syntax: '`' grammar_name '`' ~ /(.production_to_import)/?
    // imports relevant rules into this and returns the name of
    // the top level production created
    std::string parse_import(fpl_reader &src, production_rule &rule) {
        std::string grammar_name(src.parse_string());
        if(!grammar_name.length()) {
            error(src, "no grammar name specified");
            return "<failed import>";
        }

        // `grammarname`.production means import only the
        // specified production:
        std::string prod_name;
        if(src.read_byte_equalling('.')) {
            prod_name = read_production_name(src);
        }

        return import_grammar(subgrammar(grammar_name), prod_name);
    }

    std::string make_sub_prod_name() {
        return stringformat("_subexpression_{]", anon_product_count++);
    }

    // returns the name of the production representing the subexpression,
    // or an empty string on failure.
    // (may also toss errors on failure)
    std::string parse_subexpression(fpl_reader &src) {
        std::string subname;
        if(!src.read_byte_equalling('(')) {
            error("expected subexpression");
        } else {
            // subexpressions are implemented as sub rules,
            // so we need to make a rule:
            production_rule subrule(src.filename(), src.line_number());
            subname = make_sub_prod_name();
            subrule.product(subname);
            parse_expressions(src, subrule);
            if(!src.read_byte_equalling(')')) {
                error(src, stringformat(
                    "expected ')' for subexpression starting at {}\n",
                    subrule.location()
                ));
            }

            // OK plan:
            //   - subexpression matches as a rule
            //   - subexpression contents are seen by the containing
            //     rule as flattened into 1 (or more if we can do skips)
            //     arguments to the reduce function.
            // eg:
            //   '('^ (el (',', el)*)? ')'^ -> arg_list ;
            //
            //   '{'^ (key '=>' el (', ' $)*)? '}'^ hmmm commas here? interestin.
            //   maybe it's not worth supporting more than one anyway.
            //   though it would be nice to be able to do the above...
            // For the moment, we'll just support one
            // 
            //add_type_for(

            add_rule(subrule);
        }

        return subname;
    }

    int parse_expressions(fpl_reader &src, production_rule &rule) {
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
                case '&':
                    src.read_byte(); // (read the '&')
                    expr_str = read_identifier(src);
                    type     = grammar_element::Type::TERM_CUSTOM;
                    break;
                case '~':
                    // lack-of-separator assertion:
                    src.read_byte();
                    expr_str = "~";
                    type     = grammar_element::Type::LACK_OF_SEPARATOR;
                    break;
                case '(':
                    expr_str = parse_subexpression(src);
                    if(expr_str.length()) {
                        type = grammar_element::Type::NONTERM_PRODUCTION;
                    } // else 
                    break;
                case ')':
                    // end of a subrule, or else a stray ')'
                    done = true;
                    break;
                case '-':
                    // end of rule steps (expressions), or else stray '-'
                    done = true;
                    break;
                case '`':
                    // parse/import the sub-fpl, and use whatever it produces:
                    expr_str = parse_import(src, rule);
                    type     = grammar_element::Type::NONTERM_PRODUCTION;
                    break;
                case /*{*/ '}':
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
                    read_quantifier(src, expr);
                    read_suffix(src, expr);
                    if(type == grammar_element::Type::LACK_OF_SEPARATOR)
                        expr.eject = true;
                    rule.add_step(expr);
                    num_read++;
                } else {
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
         // it if you put +{ or }+ in a comment or string or whatever.
         // sorry.  try not to do that.
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
                std::string name = read_production_name(*inp);
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

    inline bool maybe_name_start(const std::string &str, size_t pos) {
        // start of string definitely could be the start of a name
        if(pos == 0) return true;
        
        char ch_before = str[pos - 1];
        if(is_production_name_char(ch_before)) {
             // apparently we're within the name of another variable - 
             // eg at "bar" within  "foobar":
            return false;
        } else {
            // the character before the one we're looking at isn't
            // part of a normal variable name, but it might be something
            // indicating a member of something else (eg foo.bar or foo->bar)
            if(ch_before == '.') return false; // foo.bar

            if(ch_before == '>') {
                // check for ->bar vs possible 2>bar
                return pos < 2 || str[pos - 2] != '-';
            }
        }
        return true;
    }

    //
    // mangle arguments as follows:
    //  <argname>\[([0-9+])\] -> <argname>.val($1)
    //  <argname>[^@] -> <argname>.val()
    //  <argname>@ -> argname.
    // This means that the stack slice looks like an array of whatever
    // type is expected for that variable, but it's a magic array
    // where the name itself resolves to the first element (so that
    // simple things like wcalc can just deal in the arg names),
    // but, if you want metadata about the argument you can access it
    // via the "@" pseudo operator.
    //
    // MODIFIES THE code STRING PASSED
    void mangle_stack_slice_args(std::string &code, const std::set<std::string> args) {
        for(auto arg : args) {
            const size_t argl = arg.length();
            size_t pos = 0;
            while((pos = code.find(arg, pos)) != std::string::npos) {
                // we found something matching the name of the arg, but
                // make sure it matches the _start_ of the arg:
                size_t endp = pos + argl;
                if(maybe_name_start(code, pos)) {
                    // now check what comes right after:
                    if(code[endp] == '@') {
                        // author wants metadata: just change the '@' to '.',
                        // and we'll expect it to result in a call to
                        // whatever the method in the stack slice is:
                        code[endp] = '.';
                    } else if(code[endp] == '[') {
                        size_t end_brace = endp + 1;
                        while(code[end_brace] && code[end_brace] != ']')
                            end_brace++;
                        if(code[end_brace] != ']') {
                            // XXX better error message:
                            jerror::error("no end brace found..\n");
                        }
                        size_t subl = end_brace - endp - 1;
                        std::string subs = stringformat(
                            ".val({})", code.substr(endp + 1, subl)
                        );
                        code.replace(endp, end_brace - endp + 1, subs);
                        endp += subs.length();
                    } else if(!is_production_name_char(code[endp])) {
                        // plain production name - default to 0th element:
                        std::string subs = ".val()";
                        code.insert(endp, subs);
                        endp += subs.length();
                    } // else it's not something to expand
                }
                pos = endp;
            }
        }
    }

    //
    // "abstracted" reducers - these are functions which might match
    // one or more rules, filling in or overriding the reduce action(s)
    // for those rules.
    //
    // +<production_name> <argdecl> <code_block>
    //
    void parse_reducer() {
        if(!inp->read_byte_equalling('+')) {
            error("expected +<production_name>");
            return;
        }

        std::string name = read_production_name(*inp);
        if(name.length() == 0) {
            error("expected production name after '+'");
            return;
        }

        auto args = parse_argdecl();
        auto code = read_code(*inp);
        mangle_stack_slice_args(code.code, args); // XXX peeking inside code_block

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
                if(parse_expressions(*inp, rule)) {
                    // .. we've read the expressions/steps leading to
                    // read what the expressions above produce:
                    inp->eat_separator();
                    if(!inp->read_exact_match("->")) {
                        error("expected '->' after parsing rule expressions");
                    }
                    inp->eat_separator();

                    std::string pname = read_production_name(*inp);
                    if(!pname.length()) {
                        error("invalid production name\n");
                    } else {
                        rule.product(pname);

                        // if there's no explicit goal, default
                        // to the product for the first explicit
                        // rule parsed:
                        if(goal.empty())
                            goal = { pname };
                    }

                    inp->eat_separator();

                    // next we expect either ';' or a code block.
                    // if it's ';' we read it and move on;  otherwise
                    // it's a code block for the rule.
                    if(!inp->read_byte_equalling(';')) {
                        auto code = read_code(*inp);
                        if(!code) {
                            error(stringformat(
                                "expected ';' or code block for rule {}",
                                rule.to_str()
                            ));
                        }
                        // XXX peeking inside code_block
                        mangle_stack_slice_args(code.code, rule.reduce_params());
                        rule.code(code);
                    }

                    add_rule(rule);
                }
            }
        } while(!inp->eof());
    }

    // returns a set of strings representing the set of products
    // needed to create the product passed (including the product
    // passed)
    std::set<std::string> dependent_products(const std::string &prod) const {
        std::list<std::string> all_wanted = { prod };
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
                     production_rule::step step = rule.nth_step(stepi);
                     if(step && !step.is_terminal()) {
                         if(!out.count(step.production_name())) {
                             all_wanted.push_back(step.production_name());
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
    // returns the name of the production which this import will
    // produce, or an empty string if nothing was imported.
    // NOTE the grammar passed isn't const because we also use
    // it as a scratch pad to prevent redundant imports.
    std::string import_grammar(
        productions *fromp, const std::string &pname = ""
    ) {
        if(!fromp) {
            error(stringformat(
                "can't import {}: no source productions.\n", pname
            ));
        }
        productions &from = *fromp;

        std::string src_fn = from.inp->filename();

        // if we're importing everything, import separator and preamble
        // code as well:
        if(!pname.length()) {
            for(auto sepc : from.separator_code) {
                add_separator_code(sepc);
            }

            for(auto primp : from.preamble) {
                add_preamble(primp);
            }
        }

        // these are the names of the products whose rules (and elements)
        // we need to import:
        std::string import_name;
        if(pname.size()) {
            import_name = pname;
        } else if(from.rules.size()) {
            // no particular production specified.  import the
            // default (first) production.
            // (should this use goals, now?  probably, yes)
            import_name = from.rules[0].product();
        }

        // get the list of all products which we'll need to
        // import (i.e. the one we wanted to import, plus
        // anything needed to generate that)
        std::set<std::string> wanted = from.dependent_products(import_name);

        // ... and now import the relevant rules.
        // NOTE that we import the rules IN ORDER because
        // changing the rule order changes precedence.
        // (hence we don't do this via rules_for_product)
        int num_found = 0;
        for(auto rule : from.rules) {
            if(wanted.count(rule.product()) > 0) {
                num_found++;
                if(from.exported_products.count(rule.product()) == 0) {
                    add_rule(rule);
                } // else rules for this product are already imported
            }
        }

        for(auto got : wanted) {
            from.exported_products.insert(got);
        }

        if(num_found <= 0) {
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

    std::string state_goto(const lr_set &in, const grammar_element &sym) {
        std::string out;
        return out;
    }

    void conflict(lr_item item1, lr_item item2, const lr_set *state) const {
        std::string indent = "    ";
        const production_rule &rule1 = rules[item1.rule];
        const production_rule &rule2 = rules[item2.rule];

        std::string in_state;
        if(state) {
            in_state = stringformat(
                " in state {}", state_index.at(state->id())
            );
        }

        warn(stringformat(
            "conflict{}:\n{}    {} at {}\n{} vs {} at {}\n",
            in_state,
            indent, rule1, rule1.location(),
            indent, rule2, rule2.location()
        ));
    }

public:

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

    int num_states() const {
        return states.size();
    }

    void clear_states() {
        states.clear();
        state_index.clear();
    }

    // returns the state number for the lr_set passed
    int32_t add_state(const lr_set &st, src_location caller = CALLER()) {
        // that state's already been added, so just return
        // the existing state number:
        if(state_index.count(st.id()) > 0) {
            return state_index[st.id()];
        }

        int32_t state_num = states.size();


        state_index.insert(
            std::make_pair(st.id(), states.size())
        );
        states[state_num] = st;
        return state_num;
    }

    int32_t end_state_for_rule(int32_t rule) {
        lr_item end_item(rule, 0);

        auto rsi = state_index.find(lr_set(end_item).id());
        if(rsi != state_index.end())
            return rsi->second;

        // end state hasn't been created yet.  create it:
        return add_state(end_item);
    }

    // clears any existing states and generates the states (and transitions)
    // needed to parse the product(s) passed
    void generate_states(const std::list<std::string> &wanted) {
        if(rules.empty()) {
            error("No rules found\n");
        }

        clear_states();

        lr_set ns;
        for(auto prodname : wanted)
            add_starts_for_product(ns, prodname, "initial goal set");

        int32_t start_state = add_state(ns);
        if(start_state != 0) {
            warn(stringformat(
                "somehow starting state is {} instead of 0\n", start_state
            ));
        }

        states[start_state].generate_states(*this);
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
            production_rule::step step = rule.nth_step(stepi);
            if(step) {
                // this one matches, so remove it from the set:
                unknown_vars.erase(step.variable_name());
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
            production_rule::step step = rule.nth_step(stepi);
            if(step && red.argument_matches(step.variable_name()))
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
                        "\n        {} {}: {}", rule, rule.location(), why_not
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
                            red, existing, rule
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

    void check_missing_types() {
        std::set<std::string> missing_types;
        int num_rules = 0;
        for(auto prr : rules_for_product) {
            const std::string &prodn = prr.first;
            if(type_for(prodn) == "") {
                missing_types.insert(prodn);
                num_rules++;
            }
        }

        if(missing_types.size()) {
            std::string msg = stringformat(
                "missing type for {} product(s) ({} rules):\n",
                missing_types.size(), num_rules
            );
            for(auto prod : missing_types) {
                auto strl  = rules_for_product.lower_bound(prod);
                auto endrl = rules_for_product.upper_bound(prod);
                std::string rulestr;
                for(auto rit = strl; rit != endrl; ++rit) {
                    const production_rule &rule = rules[rit->second];
                    rulestr += stringformat(
                        "        {} {}\n", rule.location(), rule
                    );
                }
                msg += stringformat("    {}:\n{}", prod, rulestr);
            }
            error(msg);
        }
    }

    // determines goal(s), generates states, matches up reducers, 
    // reports errors, etc.
    // call this before generating code.
    void resolve(src_location caller = CALLER()) {
        if(rules.size() <= 0) {
            error("No rules found\n");
        }

        check_missing_types();

        if(opts.entry_points.size()) {
            goal = opts.entry_points;
        }

        if(goal.empty()) {
            // no particular goal products specified, so we default
            // to whatever the first rule produces:
            goal.push_back(rules[0].product());
        }

        apply_reducers();
        generate_states(goal);
        dump_states(opts);
        check_actions();
        check_rules();
    }

    // jemp-based output:
    //  this friends thing is horrible.  can we make them members?
    //  problem then is that the generated code format is then
    //  even more tightly bound to this class....
    friend std::string fpl_x_parser_state(
        const productions &,
        const productions::lr_set &,
        const fpl_options &opts
    );
    friend std::string fpl_x_parser(const productions &, const fpl_options &);
    friend std::string fpl_x_parser_nonterm_enum(const productions &);
    friend std::string fpl_x_parser_reduce_call(
        const productions &, const production_rule &, const fpl_options &
    );
    friend std::string fpl_x_parser_shift_term(
        const productions &prds, const grammar_element &el, const fpl_options &
    );
    friend std::string fpl_x_parser_shift_nonterm(
        const productions &prds, const grammar_element &el, const fpl_options &
    );

    std::string generate_code(src_location caller = CALLER()) {
        resolve(caller);
        return reformat_code(fpl_x_parser(*this, opts), opts.output_fn);
    }

    // debugging:
    std::string states_to_string() const {
        std::string out;
        for(int stind = 0; stind < states.size(); stind++) {
            out += stringformat(
                "state {}:\n{}\n",
                stind, states.at(stind).to_str(this, "    ")
            );
        }
        return out;
    }

    void dump_states(const fpl_options &opts) const {
        if(opts.statedump != "") {
            std::ofstream out(opts.statedump, std::ios::binary);
            if(!out.is_open()) {
                internal_error(stringformat(
                    // wow... errno.. why am I even using c++?
                    "can't open '{}' for dumping states: {}\n",
                    opts.statedump, strerror(errno)
                ));
            } else {
                out << states_to_string();
            }
        }
    }

    void check_rule(const production_rule &rule) const {
        for(int stepi = 0; stepi < rule.num_steps(); stepi++) {
            auto step = rule.nth_step(stepi);

            // if the step uses a special terminal scanner, that
            // scanner's going to need to exist:
            if(step.type() == grammar_element::TERM_CUSTOM) {
                std::string scanner_name = step.gexpr.expr;
                if(!scanners.count(scanner_name)) {
                    // it is possible that the rule isn't
                    // used, so the scanner won't be used, but
                    // we're going to complain about it anyway
                    // here because the code generator shouldn't
                    // have to be bothered checking if a given
                    // element is actually used, 
                    error(stringformat(
                        "use of undefined scanner &{} in rule {} {}",
                        scanner_name, rule, rule.location()
                    ));
                }
            }
        }
    }

    void check_rules() const {
        std::list<std::string> missing_actions;
        bool used[rules.size()];

        for(int rind = 0; rind < rules.size(); rind++) {
            used[rind] = false;
        }

        // iterate the states to find which rules we actually use:
        for(auto num_state : states) {
            auto state = num_state.second;
            for(lr_item item : state.iterable_items()) {
                if(!used[item.rule]) {
                    used[item.rule] = true;

                    // turns out we do use this rule.  it'll need reduce
                    // code from one place or another:
                    const production_rule &rule = rules[item.rule];
                    if(!rule.reduce_code()) {
                        missing_actions.push_back(stringformat(
                            "{}\t{}\n", rule.location(), hypothetical_reducer(rule)
                        ));
                    }
                }
            }
        }

        for(int rind = 0; rind < rules.size(); rind++) {
            const production_rule &rule = rules[rind];
            // warn about unused rules, since they might be cruft:
            if(!used[rind]) {
                warn(stringformat(
                    "Rule {} producing {} at {} is unused\n",
                    rind, rule.product(), rule.location()
                ));
            }

            check_rule(rule);
        }
    }

    void check_actions() const {
        std::list<std::string> missing_actions;

        for(int rnum = 0; rnum < rules.size(); rnum++) {
            const production_rule &rule = rules[rnum];
            if(rule.needs_reducer()) {
                missing_actions.push_back(stringformat(
                    "{}\t{}\n", rule.location(), hypothetical_reducer(rule)
                ));
            }
        }

        // .. but if any rules are missing actions, it's an error because
        // it means we won't know what to do if the rule matches:
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
};

#include "fpl_x_parser.h"

} // namespace fpl

#endif // PRODUCTIONS_H
