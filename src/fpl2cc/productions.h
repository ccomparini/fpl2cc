#ifndef PRODUCTIONS_H
#define PRODUCTIONS_H

#include "code_block.h"
#include "fpl_options.h"
#include "production_rule.h"
#include "reducer.h"

#include "util/c_str_escape.h"
#include "util/jerror.h"
#include "util/join.h"
#include "util/reformat_code.h"
#include "util/src_location.h"
#include "util/stringformat.h"
#include "util/to_hex.h"

#include <list>
#include <map>
#include <string>
#include <time.h> // for profiling

#include "fpl_regex_separator.h" // generated

namespace fpl {

class productions;
std::string fpl_x_parser(const productions &, const fpl_options &);

class productions {

    fpl_reader_p      inp;
    const fpl_options opts;

    // for imports and such:
    using subgrammar_p = std::unique_ptr<productions>;
    const productions *parent; // prods of which we are a sub, or nullptr
    std::map<std::string, subgrammar_p> sub_productions; // grammar name -> productions
    std::set<std::string> exported_products; // exported to that of which this is a sub

    std::string final_type; // this is what the parser returns. set by @produces or inferred
    std::map<std::string, std::string> type_for_product; // (c++ reduce type for particular product)
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
    std::list<std::string> goals; // goal is any of these
    std::map<std::string, code_block> scanners;

    // for precedence lists:
    std::vector<std::string>   precedence_group_names;
    std::map<std::string, int> product_precedence;

    std::vector<production_rule>    rules;
    std::multimap<std::string, int> rules_for_product; // product -> rule ind
    mutable int anon_product_count; // to generate ids for anonymous rules

    std::vector<grammar_element>    elements;
    std::map<grammar_element, int>  element_index; // element -> element ID
    std::multimap<grammar_element, grammar_element> masks_elements; // key el "masks" value els

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
    static void error(const SourcePosition &where, const std::string &msg) {
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

    void error(const fpl_reader_p &rdr, const std::string &msg) {
        error(*rdr, msg);
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
    void record_element(
        const grammar_element &nge,
        src_location caller = CALLER()
    ) {

        if(!nge) {
            //internal_error(stringformat(
            jerror::warning(stringformat(
                "attempt to record invalid element '{}'", nge
            ), caller);
        }

        // (recording placeholder elements is not useful,
        // and is in fact harmful because you get weird
        // duplicates)
        if(nge.is_placeholder())
            return;

        if(element_index.find(nge) == element_index.end()) {
            element_index[nge] = elements.size();
            elements.push_back(nge);
        }
    }

    // this is the name of the element for purposes of (eg) enums
    std::string element_id_name(
        int el_ind, src_location caller = CALLER()
    ) const {
        if(el_ind < 0 || el_ind >= elements.size()) {
            internal_error(
                stringformat("bad element id ({})\n", el_ind), caller
            );
        }

        const grammar_element el = elements[el_ind];
        if(el.is_nonterminal()) {
            return el.nonterm_id_str();
        }
        return stringformat("_terminal_{}", el_ind);
    }

    std::string element_id_name(
        grammar_element el, src_location caller = CALLER()
    ) const {
        auto eli = element_index.find(el);
        if(eli == element_index.end()) {
            // return the name for the "null" element:
            return element_id_name(0, caller);
        }
        return element_id_name(eli->second);
    }

    int element_id(
        grammar_element el, src_location caller = CALLER()
    ) const {
        auto eli = element_index.find(el);
        if(eli == element_index.end()) {
            internal_error(
                stringformat("element {} is unindexed", el), caller
            );
        }
        return eli->second;
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

        // Returns the (rule num:countdown) string used in to_str().
        // If this is a subrule, and the productions pointer passed
        // is not null, it also recursively prepends any parent
        // (rule num:countdown) strings, to show context.
        std::string rule_pos_str(const productions *prds) const {
            // we can always show (rule number):(how many steps are left):
            std::string out = stringformat("({}:{})", rule, countdown);
            if(prds) {
                // .. in this case, we can get the context too (if any)
                int prule = prds->rules[rule].parent_rule_number();
                if(prule >= 0) {
                    int ppos  = prds->rules[rule].parent_countdown_pos();
                    lr_item pitem(prule, ppos);
                    out = pitem.rule_pos_str(prds) + out;
                }
            }
            return out;
        }

        std::string to_str(
            const productions *prds = nullptr,
            const lr_set *state = nullptr
        ) const {
            // we can always show (rule number):(how many steps are left):
            std::string out = rule_pos_str(prds);

            if(prds) {
                const production_rule &prl = prds->rule(rule);

                // if we have a suitable products pointer, we can
                // also show what this item is used to produce and
                // where it is in the rule:
                if(rule >= 0 && rule < prds->rules.size()) {
                    const production_rule &prl = prds->rules[rule];
                    auto &context = prds->parentmost_rule(prl);
                    out = stringformat("{} {}:\t", context.product(), out);

                    for(int stepi = prl.num_steps(); stepi >= 0; --stepi) {
                        if(stepi == countdown) out += " •";
                        else                   out += " ";

                        production_rule::step step = prl.nth_from_end(stepi);
                        if(step)
                            out += step.to_str();
                    }
                } else {
                    out = stringformat("(invalid rule {})", rule);
                }

                if(state) {
                    // .. and if we also know the state for the rule, we
                    // can show what to do after matching the item:
                    int32_t next_stn = state->next_state(step(*prds));
                    if(next_stn >= 0) {
                        out += stringformat("\t=> state {}", next_stn);
                    } else if(!prl.product_element().is_end_of_parse()) {
                        out += stringformat("\t=> (reduce)");
                    } else {
                        out += stringformat("\t=> (done)");
                    }
                }

                if(rule >= 0 && rule < prds->rules.size()) {
                    out += "\t(" + prl.location() + ")";
                }
            }

            return out;
        }
    };

    struct lr_transition {
        using type = enum { STATE, REDUCTION, COMPLETION };
        grammar_element right_of_dot;
        type            what;  // what to do on matching right_of_dot
        int             which; // state num, or num of rule to reduce by
        bool            eject;

        lr_transition() : which(-1), eject(false) { }

        lr_transition(const grammar_element &ge, bool e, type t, int n)
            : right_of_dot(ge), what(t), which(n), eject(e) {
        }

        operator bool() const {
            return which >= 0;
        }

        static std::string type_to_str(type t) {
            switch(t) {
                case STATE:      return "STATE";
                case REDUCTION:  return "REDUCTION";
                case COMPLETION: return "COMPLETION";
            }
            return "(unknown transition type)";
        }

        std::string to_str() const {
            return stringformat("{}: ({} {})",
                right_of_dot, type_to_str(what), which
            );
        }
    };

    // An lr_set is a set of lr items.
    // Each state is represented by an lr_set (for now - 
    // this feels like it needs a refactor such that states
    // are implemented as lr_sets but have other features..)
    class lr_set {
        mutable std::string _id_cache;
        std::set<lr_item> items;

        // everything here and below is state only:
        std::map<grammar_element, int32_t> next_state_for_el;

        // in the current state, if the element matched is in
        // this set, it doesn't get pushed to the param stack:
        std::set<grammar_element>          ejected_el;

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

        const std::set<grammar_element> &ejected_elements() const {
            return ejected_el;
        }

        std::list<grammar_element> terminals() const {
            std::list<grammar_element> items;
            auto start = next_state_for_el.begin();
            auto end   = next_state_for_el.end();
            for(auto ge_nst = start; ge_nst != end; ++ge_nst) {
                if(!ge_nst->first.is_nonterminal()) {
                    items.push_back(ge_nst->first);
                }
            }
            return items;
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

        //
        // Returns an iterable set of transitions out of the state
        // passed.
        //
        // Each transition is a mapping of a (possibly false/"none")
        // grammar element to what to do if that element is next up.
        // "What to do" is either to push the matching element and
        // move to a new state, or reduce according to a particular
        // rule, followed by popping stack elements and moving to
        // the new state on the stack.
        //
        // Nonterminal transitions come first, followed by terminals,
        // possibly followed by a transition for element "NONE" (which
        // perhaps should be renamed "ANY"?), which should be executed
        // if none of the prior states match.
        //
        // Use this for code generation.
        using lr_transitions = std::list<lr_transition>;
        lr_transitions transitions(const productions &prds) const {
            lr_transitions nonterm_trans;
            lr_transitions term_trans;
            lr_transition  none_trans;

            for(lr_item item : iterable_items()) {
                production_rule::step step = item.step(prds);
                lr_transition trans;
                int32_t next_stn = next_state(step);
                bool ejected = ejected_el.count(step.gexpr) > 0;
                if(next_stn < 0) {
                    // This may be over articulated.   Possibly the
                    // reduce call (or action!) could handle the
                    // distinction between reducing to a normal
                    // product and being done.  Right now, I think
                    // it can't just be an action, because of how we
                    // clean the stack after the action.  But, if we
                    // ever allow rules to do the stack manipulation,
                    // the termination action could simply call
                    // terminate() and leave the stack intact (or in
                    // whatever state is appropriate)
                    lr_transition::type tp = lr_transition::REDUCTION;
                    grammar_element pe = prds.rule(item.rule).product_element();
                    if(pe.is_end_of_parse()) {
                        // matching this rule indicates end of parse:
                        tp = lr_transition::COMPLETION;
                    }
                    trans = lr_transition(step.gexpr, ejected, tp, item.rule);
                } else {
                    trans = lr_transition(
                        step.gexpr, ejected,
                        lr_transition::STATE, next_stn
                    );
                }

                if(step.gexpr.type == grammar_element::Type::NONE) {
                    none_trans = trans;
                } else if(step.gexpr.is_nonterminal()) {
                    if(!nonterm_trans.size() || (trans.right_of_dot != nonterm_trans.back().right_of_dot))
                        nonterm_trans.push_back(trans);
                } else {
                    if(!term_trans.size() || (trans.right_of_dot != term_trans.back().right_of_dot))
                        term_trans.push_back(trans);
                }
            }

            lr_transitions out;
            out.splice(out.end(), nonterm_trans);
            out.splice(out.end(), term_trans);
            if(none_trans) {
                out.push_back(none_trans);
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
                // can correctly recognize the start of the match:
                if(expr.is_nonterminal()) {
                    const std::string &pname = expr.production_name();
                    auto strl  = prds.rules_for_product.lower_bound(pname);
                    auto endrl = prds.rules_for_product.upper_bound(pname);
                    if(strl == endrl) {
                        error(rule.location(), stringformat(
                            "nothing produces '{}' ({})\n", pname, expr
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

        // Returns a multimap of (grammar_element -> lr_item)
        // for the items in this lr_set.
        // Items for the ends of rules will be indexed under
        // grammar element type == NONE.
        // Used in generating states.
        std::multimap<grammar_element, lr_item> items_per_element(
            const productions &prds
        ) const {
            std::multimap<grammar_element, lr_item> items_for_el;
            for(auto item : items) {
                if(item.countdown == 0) {
                    items_for_el.insert(
                        // index it as grammar_element of type NONE:
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

        void report_eject_conflicts( 
            const productions &prds,
            const grammar_element &element,
            const std::list <lr_item> &ejected,
            const std::list <lr_item> &nonejected
        ) const {
            std::string msg = stringformat(
                "eject/keep conflict on {}:\n", element
            );
            for(auto item : ejected) {
                msg += stringformat("        {}\n", item.to_str(&prds));
            }
            msg += "      vs\n";
            for(auto item : nonejected) {
                msg += stringformat("        {}\n", item.to_str(&prds));
            }

            // this is not at all fatal. the item will be kept if necessary.
            warn(stringformat("{::c}\n", msg));
        }

        void report_reduce_conflicts(
            productions &prds, const std::list<lr_item> &items
        ) {
            std::string cons;
            for(auto item: items) {
                cons += stringformat("        {}\n", item.to_str(&prds));
            }

            // these aren't fatal either, but typically the author
            // will want to know because it means that one rule or
            // the other won't ever reduce:
            warn(stringformat(
                "reduce/reduce conflict in state {} ({} items):\n{}",
                prds.state_index.at(this->id()), items.size(), cons
            ));
        }

        // recursively generates any following states, adding them
        // to the productions passed.
        void generate_following_states(
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

            auto trit = items_for_el.begin(); // transition iterator
            auto done = items_for_el.end();
            while(trit != done) {
                lr_set next_state;

                // these pertain to cur_el (below) and are use for
                // checking and reporting eject conflicts.  
                std::list<lr_item> items_with_eject;
                std::list<lr_item> items_without_eject;

                // for checking reduce/reduce conflicts:
                std::list<lr_item> reduce_items;

                // make an lr_set for all the items matching the
                // input element:
                const grammar_element &cur_el = trit->first;
                for( ; trit != done && trit->first == cur_el; ++trit) {
                    const lr_item item = trit->second;
                    auto          step = item.step(prds);

                    if(step.eject) items_with_eject.push_back(item);
                    else        items_without_eject.push_back(item);

                    if(cur_el.type != grammar_element::Type::NONE) {
                        // if the item is multiple-match ('*' or '+'
                        // after it), it can follow itself:
                        if(step.is_multiple()) {
                            next_state.add_expanded(item, prds);
                        }

                        next_state.add_expanded(item.next_in_rule(), prds);
                    } else {
                        // we're at the end of a rule or subexpression.
                        // if it's a subexpression, we don't reduce the
                        // subexpression.  Instead, we move on to the
                        // next step in the parent.  The parent will
                        // glean this rule's steps at reduce time.
                        auto rule = prds.rules.at(item.rule);
                        int prulenum = rule.parent_rule_number();
                        if(prulenum >= 0) {
                            lr_item us_in_parent = lr_item(
                                prulenum, rule.parent_countdown_pos()
                            );
                            auto pstep = us_in_parent.step(prds);
                            if(pstep.is_multiple()) {
                                next_state.add_expanded(us_in_parent, prds);
                            }
                            lr_item next_in_parent = us_in_parent.next_in_rule();
                            if(next_in_parent) {
                                next_state.add_expanded(
                                    us_in_parent.next_in_rule(), prds
                                );
                            } else {
                                // We are the final step in the parent
                                // rule.  Therefore, the next step is
                                // the 0 countdown of the parent as well,
                                // so add item (parent rule num, 0)
                                next_state.add_item(lr_item(prulenum, 0));
                            }
                        } else {
                            reduce_items.push_back(item);
                        }
                    }
                }

                if(reduce_items.size() > 1) {
                    report_reduce_conflicts(prds, reduce_items);
                }
 
                if(items_with_eject.size() > 0) {
                    if(items_without_eject.size() > 0) {
                        // some items/rules expect this to be ejected, and
                        // some don't.
                        report_eject_conflicts(
                            prds, cur_el,
                            items_with_eject, items_without_eject
                        );
                    } else {
                        // Only add it to the ejected set if there are
                        // no items expecting that it's not ejected.
                        // This is because if an element is marked ejected
                        // for a given rule but wasn't actually ejected,
                        // the reduce code can still work consistently,
                        // but reduce code which expects an argument might
                        // actually need that argument.
                        ejected_el.insert(cur_el);
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
                        prds.states[new_state_num]
                            .generate_following_states(prds);
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
            const production_rule &ruleref = rules[rit->second];
            set.add_expanded(
                // (items here are always referring to the start
                // of the rule, and since items count down to the
                // end of the rule, start position is num_steps().
                lr_item(rit->second, ruleref.num_steps()),
                *this
            );
        }
    }

public:

// XXX this is currently stupid in that you pass the fpl_reader
// but then you have to make a separate call to parse it.  fix that.
    productions(
        const fpl_options &op, fpl_reader_p src, productions *p = nullptr
    ) :
        inp(src), opts(op), parent(p),
        default_main(false), anon_product_count(0)
    {
        // element 0 is a null element and can be used to
        // indicate missing/uninitialized elements or such.
        // we count it as a nonterminal so that it can be
        // accessed via the enum etc.
        record_element(
            grammar_element("_fpl_null", grammar_element::NONTERM_PRODUCTION)
        );

    }

    std::string input_filename() const {
        if(inp) return inp->filename();
        return "";
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

    // returns the name of the target-language type expected as
    // the result of reducing to the product indicated:
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
        return final_type;
    }

    // and this returns the type to use for a particular grammar
    // element, which covers terminals as well:
    std::string type_for(const grammar_element &ge) const {
        if(ge.is_nonterminal()) {
            return type_for(ge.expr);
        }
        // assertions are terminals as well:
        return "Terminal";
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
            code = read_code();
        }

        std::string errm;
        if(!code && (allowed_src | REGEX)) {
            code = parse_regex_separator();
        }

        if(!code && (allowed_src | LIB)) {
            // expect a quoted (or otherwise string-delimited)
            // file basename:
            std::string fn = inp->parse_string();
            if(fn.length() > 0) {
                code = code_block::from_file(
                    fn + ".inc", opts.src_path,
                    inp->filename(), inp->line_number()
                );
            }
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

    void set_output_type(const std::string &rt) {
        all_types.insert(rt);
        final_type = rt;
    }
    std::string output_type() const {
        return final_type;
    }

    void set_post_parse(const code_block &cb)  { post_parse = cb; }
    void set_post_reduce(const code_block &cb) { post_reduce = cb; }
    void set_default_main(bool def)            { default_main = def; }
    void add_internal(const code_block &cb)    { parser_members.push_back(cb); }

    void add_type_for(const std::string &prod, const std::string &type) {
        type_for_product[prod] = type;
        all_types.insert(type);
    }

    void set_precedence(const std::string &product, int precedence) {
        product_precedence[product] = precedence;
    }

    void parse_scanner() {
        std::string name = read_identifier(inp);
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
        // Reads an argument to the end of the line.
        // End of line is any ascii vertical space (for now).
        // Leading and trailing spaces/tabs are stripped.
        return inp->read_re("[ \\t]*([^@\\x0a-\\x0d]+)[ \\t]*\n")[1];
    }

    void add_goal(const std::string raw_goal, const SourcePosition &whence) {
        // the goal is either simply a production name,
        // or grammar_name.production_name.  so, see which:
        size_t grammar_end = raw_goal.find('.');
        if(grammar_end != std::string::npos) {
            // if the dot is at the start or end, we're missing
            // either the production or the grammar:
            if(grammar_end >= raw_goal.length() || grammar_end == 0) {
                error(stringformat("invalid goal '{}'\n", raw_goal));
            }

            std::string grammar_name = raw_goal.substr(0, grammar_end);
            std::string production   = raw_goal.substr(grammar_end + 1);
            import_grammar(subgrammar(grammar_name, whence), production);
            goals.push_back(production);
        } else {
            // it's a simple goal name:
            goals.push_back(raw_goal);
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
            code_block new_code = code_for_directive(dir);
            if(default_action) {
                warn(stringformat(
                    "default_action at {} overwrites existing at {}\n",
                    new_code.location(), default_action.location()
                ));
            }
            default_action = new_code;
        } else if(dir == "default_main") {
            // TODO kill this in favor of having a default
            // main in grammarlib and importing.
            default_main = true;
        } else if(dir == "goal") {
            SourcePosition whence(inp);
            std::string newgoal = arg_for_directive();
            if(!newgoal.length()) {
                error("can't parse the @goal");
            } else {
                add_goal(newgoal, whence);
            }
        } else if(dir == "grammar") {
            // import the grammar from another fpl:
            SourcePosition whence(inp);
            import_grammar(subgrammar(arg_for_directive(), whence));
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
            set_output_type(arg_for_directive());
        } else if(dir == "scanner") {
            parse_scanner();
        } else if(dir == "separator") {
            add_separator_code(
                code_for_directive(dir, code_source::INLINE_OR_LIB)
            );
        } else if(dir == "type_for") {
            inp->eat_separator();
            std::string prod = read_production_name(inp);
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

    // If the element passed is a subexpression, returns the
    // index of the rule for matching the element.
    // Returns -1 if the element isn't a subexpression.
    int subrulenum_for_el(const grammar_element &el) const {
        int found = -1;

        if(el.type == grammar_element::NONTERM_SUBEXPRESSION) {
            auto rit = rules_for_product.lower_bound(el.expr);
            auto end = rules_for_product.upper_bound(el.expr);

            for( ; rit != end; ++rit) {
                if(found >= 0) {
                    warn(stringformat(
                        "more than one rule produces {} - bug?", el
                    ));
                    break;
                }
                found = rit->second;
            }
        }

        return found;
    }

    const production_rule &rule(int rnum, src_location cl = CALLER()) const {
        if(rnum < rules.size()) {
            return rules[rnum];
        }
        warn(stringformat("invalid rule number {} at {}\n", rnum, cl));
        static production_rule dummy;
        return dummy;
    }

    // Traverses the set of parent rules until it comes to a
    // rule with no parents, and returns a ref to that rule.
    // Used for determining the context of subrules.
    const production_rule &parentmost_rule(
        const production_rule &child
    ) const {
        int topmost = child.rule_number();
        int prulenum;
        while((prulenum = rules[topmost].parent_rule_number()) >= 0) {
            topmost = prulenum;
        }

        return rule(topmost);
    }


    // adds and indexes a copy of the rule passed.
    // returns a rule number.
    int add_rule(production_rule &rule, src_location caller = CALLER()) {
        int rule_num = rules.size();
        rule.set_rule_number(rule_num);

        rules.push_back(rule);

        for(int stp = 0; stp < rule.num_steps(); stp++) {
            grammar_element ge = rule.nth_step(stp).gexpr;

            record_element(ge);

            if(ge.type == grammar_element::Type::NONTERM_SUBEXPRESSION) {
                int srnum = subrulenum_for_el(ge);
                if(srnum < 0) {
                    warn(stringformat(
                        "no subrule for {} at {} - bug?", ge, caller
                    ));
                } else {
                    rules[srnum].set_parent(rule_num, rule.num_steps() - stp);
                }
            }
        }
        record_element(rule.product_element(), caller);

        std::string prd = rule.product();
        if(prd == "") {
            warn(stringformat("no product for rule {} - bug?", rule));
        } else {
            rules_for_product.insert(std::make_pair(prd, rule_num));
        }

        return rule_num;
    }

    void add_preamble(const code_block &code) {
        preamble.push_back(code);
    }

    static void read_quantifier(fpl_reader_p &src, production_rule::step &expr) {
        switch(src->peek()) {
            case '*':
                expr.qty.optional = true;
                expr.qty.multiple = true;
                src->skip_bytes(1);
                break;
            case '+':
                expr.qty.optional = false;
                expr.qty.multiple = true;
                src->skip_bytes(1);
                break;
            case '?':
                expr.qty.optional = true;
                expr.qty.multiple = false;
                src->skip_bytes(1);
                break;
            default:
                expr.qty.optional = false;
                expr.qty.multiple = false;
                break;
        }
    }

    static void read_suffix(fpl_reader_p &src, production_rule::step &expr) {
        if(src->read_byte_equalling('^')) {
            expr.eject = true;
        } else if(src->read_byte_equalling(':')) {
            // the name to give the argument corresponding
            // this this step follows:
            expr.varname = src->read_re("[A-Za-z][A-Za-z0-9_]*")[0];
        }
    }

    // identifiers must start with a letter, and thereafter may
    // contain letters, digits, or underscores.
    static inline std::string read_identifier(fpl_reader_p &src) {
        std::cmatch nm = src->read_re("[a-zA-Z][a-zA-Z_0-9]*");
        if(!nm.length())
            return "";
        return nm[0];
    }

    static inline std::string read_production_name(fpl_reader_p &src) {
        return read_identifier(src);
    }

    static inline std::string read_directive(fpl_reader_p &src) {
        return src->read_re("([A-Za-z][A-Za-z0-9_]+)\\s*")[1];
    }

    // Returns the set of names of all imported files.
    inline std::set<std::string> imported_files() const {
        // Hopefully the compiler does something smart and doesn't
        // copy all these strings over and over.. :/
        std::set<std::string> out;
        out.insert(imports.begin(), imports.end());
        // for(auto it = sub_productions.begin(); it != sub_productions.end(); it++) {
        for(auto const &sub : sub_productions) {
            auto subimps = sub.second->imported_files();
            out.insert(subimps.begin(), subimps.end());
        }
        return out;
    }

    // Searches ancestors to see if any import the file with the given
    // name.  Used for finding dependency loops.
    // Filename checks are simplistic (no checks for symlinks or other
    // file aliasing) but this is good enough for now.
    const productions *ancestor_which_imports(const std::string filename) {
        for(const productions *prds = this; prds; prds = prds->parent) {
            if(prds->input_filename() == filename) {
                return prds;
            }
        }
        return nullptr;
    }

    // Attempts to open an fpl source file and associate it with a
    // reader for import.  Searches the directory this source file
    // is in, as well as any other directories in the --src-path.
    fpl_reader_p open_for_import(
        const std::string &fpl_name,
        const SourcePosition whence
    ) {

        Searchpath searchp = opts.src_path;

        // search relative to the source fpl as well.
        searchp.append(inp->input_dir());
        std::string filename = searchp.find(fpl_name + ".fpl");

        // Check to see if importing the given filename would cause an
        // "import loop".  For example, if a given fpl imports itself,
        // it's nearly certain that's not what the user intended. Also,
        // at the moment we can't handle it if a.fpl imports b.fpl and
        // b.fpl imports a (and I question if it's ever a good idea to
        // do that), so we're calling that an error as well.
        if(auto other = ancestor_which_imports(filename))
            error(whence, stringformat("Import loop loading {}", filename));

        // this is for generating dependencies and such (--dump-dependencies)
        imports.push_back(filename);

        // report errors in the sub-fpl in the context of
        // the importing file:
        auto sub_errcb = [whence](const std::string &msg)->void {
            error(whence, "\n\t" + msg);
        };

        auto subreader = make_shared<fpl_reader>(filename, sub_errcb);
        if(!subreader) {
            internal_error(stringformat(
                "make_shared<fpl_reader> failed for '{}'\n", filename
            ));
        }
        return subreader;
    }

    // Loads (and parses) the subgrammar specifier, or fetches
    // it from cache. Does not import anything from the subgrammar - 
    // the idea here is basically to abstract the grammar name
    // from the source, and to implement caching so that if you
    // want to import multiple pieces of a given subgrammar, we
    // don't have to read and parse the source for that multiple
    // times.
    productions *subgrammar(
        const std::string &grammar_name, const SourcePosition &where
    ) {
        productions *out = nullptr;

        auto exsp = sub_productions.find(grammar_name);
        if(exsp != sub_productions.end()) {
            out = exsp->second.get();
        } else {
            subgrammar_p sub = make_unique<productions>(
                opts, open_for_import(grammar_name, where), this
            );
            // To avoid issues when/if the fpl tries to load itself
            // as a subgrammar, we're going to move the sub to the
            // sub_productions index before parsing (thus avoiding
            // infinite loops).  But save the "out" ptr so we can
            // parse off of it:
            out = sub.get();
            sub_productions[grammar_name] = std::move(sub);
            out->parse_fpl();
        }

        return out;
    }

    // syntax: '`' grammar_name '`' ~ /(.production_to_import)/?
    // imports relevant rules into this and returns the name of
    // the top level production created
    std::string parse_import() {
        SourcePosition whence(inp);
        std::string grammar_name(inp->parse_string());
        if(!grammar_name.length()) {
            error(inp, "no grammar name specified");
            return "<failed import>";
        }

        // `grammarname`.production means import only the
        // specified production:
        std::string prod_name;
        if(inp->read_byte_equalling('.')) {
            prod_name = read_production_name(inp);
        }

        return import_grammar(subgrammar(grammar_name, whence), prod_name);
    }

    std::string make_sub_prod_name() const {
        if(parent) {
            // to avoid name collision madness when there are multiple
            // subgrammars each with subexpressions, sub production names
            // need to be generated by the parentmost productions:
            return parent->make_sub_prod_name();
        }
        return stringformat("_subex_{}", anon_product_count++);
    }

    // returns the name of the production representing the subexpression,
    // or an empty string on failure.
    // (may also toss errors on failure)
    std::string parse_subexpression() {
        std::string subname;
        if(!inp->read_byte_equalling('(')) {
            error("expected subexpression");
        } else {
            // subexpressions are implemented as sub rules,
            // so we need to make a rule:
            production_rule subrule(
                inp->filename(), inp->line_number(),
                grammar_element::NONTERM_SUBEXPRESSION
            );
            subname = make_sub_prod_name();
            subrule.product(subname);
            parse_expressions(subrule);
            if(!inp->read_byte_equalling(')')) {
                error(inp, stringformat(
                    "expected ')' for subexpression starting at {}\n",
                    subrule.location()
                ));
            }

            add_rule(subrule);
        }

        return subname;
    }

    int parse_expressions(production_rule &rule) {
        int num_read = 0;
        bool done = false;
        do {
            inp->eat_separator();

            std::string expr_str;
            grammar_element::Type type = grammar_element::Type::NONE;

            const utf8_byte inch = inp->peek();
            switch(inch) {
                case '\0':
                    done = true;
                    break; // EOF
                case '#':
                    // line comment - just skip
                    inp->read_line();
                    break;
                case '"':
                case '\'':
                    expr_str = inp->parse_string();
                    type     = grammar_element::Type::TERM_EXACT;
                    break;
                case '/':
                    expr_str = inp->parse_string();
                    type     = grammar_element::Type::TERM_REGEX;
                    break;
                case '&':
                    inp->read_byte(); // (read the '&')
                    expr_str = read_identifier(inp);
                    type     = grammar_element::Type::TERM_CUSTOM;
                    break;
                case '~':
                    // lack-of-separator assertion:
                    inp->read_byte();
                    expr_str = "~";
                    type     = grammar_element::Type::LACK_OF_SEPARATOR;
                    break;
                case '(':
                    expr_str = parse_subexpression();
                    if(expr_str.length()) {
                        type = grammar_element::Type::NONTERM_SUBEXPRESSION;
                    } // else ... ? error? XXX
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
                    expr_str = parse_import();
                    type     = grammar_element::Type::NONTERM_PRODUCTION;
                    break;
                case /*{*/ '}':
                    // this can happen, especially if there's a '}+'
                    // embedded in a code block.
                    if(inp->read_byte_equalling('+'))
                        error(inp,
                            "stray '}+'.  "
                            "perhaps there's }+ embedded in a code block"
                        );
                    else
                        error(inp, "unmatched '}'");
                    break;
                case '<':
                case '.':
                case '>':
                    // special precedence-related production names:
                    //  '>>' = the highest precedence product
                    //  '>'  = the product with precedence one higher than us
                    //  '.'  = the product for our precedence group
                    //  '<'  = the product with precedence just lower than us
                    //  '<<' = the lowest precedence product
                    expr_str = inp->read_re("(:?<<?)|\\.|(:?>>?)")[0];
                    type = grammar_element::Type::NONTERM_PREC_PLACEHOLDER;
                    break;
                default:
                    // should be the name of a production.
                    expr_str = read_production_name(inp);
                    if(!expr_str.length()) {
                        error(inp, stringformat(
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
                    read_quantifier(inp, expr);
                    read_suffix(inp, expr);
                    if(type == grammar_element::Type::LACK_OF_SEPARATOR)
                        expr.eject = true;
                    rule.add_step(expr);
                    num_read++;
                } else {
                    error(inp, stringformat(
                        "expected type {} = {} but got .. nothing?\n",
                        grammar_element::Type_to_str(type), type
                    ));
                }
            }
        } while(!(done || inp->eof()));

        return num_read;
    }

    code_block read_code() {
         // code is within "+{" "}+" brackets.
         // we don't know anything about the grammar of the code
         // within the brackets (presently), so you will derail
         // it if you put +{ or }+ in a comment or string or whatever.
         // sorry.  try not to do that.
         inp->eat_separator();

         size_t start = inp->current_position();
         if(!inp->read_exact_match("+{")) {
             // no code - return a false value
             return code_block();
         }

         std::string code_str;
         bool found_terminator = false;
         char byte_in;
         while(byte_in = inp->read_byte()) {
             if(byte_in == '}') {
                 if(inp->peek() == '+') {
                     inp->read_byte();
                     found_terminator = true;
                     break;
                 }
             }
             code_str += byte_in;
         }

         if(!found_terminator) {
             error(inp, stringformat(
                 "Expected code block terminator ('}}+') but got byte 0x{}",
                 to_hex(byte_in)
             ));
         }

         return code_block(code_str, inp->filename(), inp->line_number(start));
    }

    code_block parse_regex_separator() {
        code_block code;
        std::string regex;

        if(inp->peek() == '/') {
            regex = inp->parse_string();
            if(regex.length()) {
                code = code_block(fpl_regex_separator(regex));
            }
        }

        return code;
    }

    // optional argument declaration for a reduction code block:
    //
    //   ('(' (argument ','?)* ')')? -> argdecl ;
    //
    std::set<std::string> parse_argdecl() {
        std::set<std::string> args;

        if(inp->read_byte_equalling('(')) {
            while(!inp->read_byte_equalling(')')) {
                std::string name = read_production_name(inp);
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

        std::string name = read_production_name(inp);
        if(name.length() == 0) {
            error("expected production name after '+'");
            return;
        }

        auto args = parse_argdecl();
        auto code = read_code();

        if(!code) {
             error(stringformat(
                 "expected start of code (\"+{{\") but got «{}»",
                 inp->debug_peek()
             ));
        }

        reducers.push_back(reducer(name, args, code));
    }

    // parses the '->' [production name] in a rule definition
    std::string read_rule_production() {
        inp->eat_separator();
        if(!inp->read_exact_match("->")) {
            error("expected '->' before production name");
        }

        inp->eat_separator();

        std::string pname = read_production_name(inp);
        if(!pname.length()) {
            error("invalid production name\n");
        }
        return pname;
    }

    void parse_rule_implementation(production_rule &rule) {
        std::string pname = read_rule_production();
        if(!pname.length()) {
            error("invalid production name\n");
        } else {
            rule.product(pname);

        }

        inp->eat_separator();

        // next we might have a code block:
        auto code = read_code();
        if(code) {
            rule.code(code);
        }

        inp->read_byte_equalling(';');
    }

    // Parses the next rule (if any) and adds it to this.
    // Returns the new rule number, or -1 if no rule was parsed.
    int parse_rule() {
        production_rule rule(inp->filename(), inp->line_number());
        if(parse_expressions(rule)) {
            parse_rule_implementation(rule);
            return add_rule(rule);
        }
        return -1;
    }

    void parse_precedence_group_prods(
        std::list<std::string> &prods_this_group
    ) {
        // we expect 0 or more rules or names. (0 probably
        // doesn't really make sense, but we'll accept it)
        bool done = false;
        while(!done) {
            inp->eat_separator();

            if(inp->read_byte_equalling(']')) {
                break;
            } else if(inp->read_byte_equalling('#')) {
                inp->read_line(); // comment
                continue;
            }
            
            size_t rew_pos = inp->current_position();
            std::string this_product;
            if(inp->peek() == '`') {
                this_product = parse_import();
            } else {
                this_product = read_production_name(inp);
            }
            inp->eat_separator();
            if(inp->read_byte_equalling(',')) {
                // this_product is the name of the product; proceed
            } else if(inp->peek() == ']') {
                // this_product is the last product name in the list;
                // proceed
            } else {
                // this_product is not a simple product name.  The
                // other valid possibility is it's a rule producing
                // the product to add to the group, so we need to
                // rewind and parse the rule:
                inp->go_to(rew_pos);
                int rn = parse_rule();
                if(rn >= 0) {
                    this_product = rules[rn].product();
                } else {
                    // failed parsing the rule as well. it probably
                    // already tossed an error, but clear the result
                    // anyway:
                    this_product = "";
                }
            }

            if(this_product.length()) {
                prods_this_group.push_back(this_product);
            } else {
                error(SourcePosition(inp, rew_pos),
                    "expected rule definition or product name"
                );
                done = true;
            }
        }
    }

    void parse_precedence_group() {
        // Precedence groups are:
        //     '[' rule_or_name+ ']' -> group_name rule_code ;
        //
        // If it's a rule, rule_or_name is parsed as a regular rule.
        // Names are just product names, comma terminated (with the
        // final comma optional).
        //
        // What they do is:
        //   - add group_name to the (global) precedence list; add the
        //     index to the relative_precedence_index
        //   - for each rule/product name inside the list,
        //     - set the relative precedence index for the product
        //       to the index of the group name
        //     - create another rule for product -> group_name
        //   - add a rule:
        //         > -> group_name ;
        // (note: the assumption in the above rule is that higher precedence
        // comes later in the file, and has a greater index)
        //
        // The relative precedence index is used to resolve the
        // special production tokens '<', '.', and '>' to the group name
        // at index relative precedence - 1, relative precedence, and
        // relative precedence + 1 respectively.
        //
        if(inp->read_byte_equalling('[')) {
            std::list<std::string> prods_this_group;
            parse_precedence_group_prods(prods_this_group);

            // "group rule" serves 2 purposes.
            // The first is precedence linking - the rule itself
            // is an alias of the next higher precedence product
            // to the thing the group rule produces.
            // Second is that it's product is the group name product,
            // which other things may use via the < . > product aliases
            // (or as any other product name).
            production_rule group_rule(inp->filename(), inp->line_number());
            parse_rule_implementation(group_rule); // (parses the name etc)

            group_rule.add_step(production_rule::step(
                ">", grammar_element::Type::NONTERM_PREC_PLACEHOLDER,
                group_rule.product()
            ));
            std::string group_name = group_rule.product();

            inp->eat_separator();


            // If a comma comes next, we're linking this precedence group
            // to the group after the comma.
            // With some effort, we could actually make the comma be a sort
            // of precedence-setting operator and have it not matter if
            // the things on the left or right of it are groups or individual
            // rules (or production names), but for now I'm going to just
            // parse it here.
            if(inp->read_byte_equalling(',')) {
                add_rule(group_rule);
            }

            int precedence = precedence_group_names.size();
            set_precedence(group_name, precedence);
            precedence_group_names.push_back(group_name);

            for(auto prod: prods_this_group) {
                set_precedence(prod, precedence);

                // and this rule effectively adds the product to the group
                // by creating a rule:   prod -> group_product
                production_rule rule(inp->filename(), inp->line_number());
                rule.add_step(production_rule::step(
                    prod, grammar_element::Type::NONTERM_PRODUCTION,
                    group_rule.product()
                ));
                rule.code(group_rule.code());
                rule.product(group_name);
                add_rule(rule);
            }
        }
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
                    code_block code(read_code());
                    if(code) {
                        add_preamble(code);
                    }
                } else {
                    // expect code for reducing to the production given:
                    parse_reducer();
                }
            } else if(inp->read_byte_equalling('@')) {
                std::string directive = read_directive(inp);
                parse_directive(directive);
            } else if(inp->peek() == '[') {
                parse_precedence_group();
            } else if(inp->read_byte_equalling(']')) {
                error("unmatched ']'\n");
            } else if(/* { */ inp->read_byte_equalling('}')) {
                // likely what happened is someone put a }+ inside
                // a code block.  anyway a floating end brace is
                // wrong..
                error("unmatched '}'\n");
            } else {
                parse_rule();
            }
        } while(!inp->eof());

        // we do this here (right after parsing) so that it
        // works right for imports
        resolve_precedence_expressions();
    }

    // Returns an arbitrary (but better than nothing) file:line
    // source location for the product passed.
    std::string prod_location(const std::string &prod) const {
        auto first_rule_i = rules_for_product.lower_bound(prod);
        if(first_rule_i != rules_for_product.end()) {
            return rules[first_rule_i->second].location();
        }
        return stringformat("(no location available for {})", prod);
    }

    // Returns true if nothing creates the product passed.
    // Used for error checking.
    bool nothing_creates(const std::string &prod) const {
        auto strl  = rules_for_product.lower_bound(prod);
        auto endrl = rules_for_product.upper_bound(prod);
        return strl == endrl;
    }

    // returns a set of strings representing the set of products
    // needed to create the product passed (including the product
    // passed)
    std::set<std::string> dependent_products(const std::string &prod) const {
        std::list<std::string> all_wanted = { prod };
        std::set<std::string> missing;
        std::set<std::string> out;

        // NOTE I'm assuming here that we can append to a list
        // while iterating it and have things dtrt.  I believe
        // this is a reasonable thing to ask from std::list,
        // but have no documentation/spec saying it's ok.
        for(auto wanted : all_wanted) {
            out.insert(wanted);

            auto strl  = rules_for_product.lower_bound(wanted);
            auto endrl = rules_for_product.upper_bound(wanted);

            for(auto rit = strl; rit != endrl; ++rit) {
                production_rule rule = rules[rit->second];

                // any rules needed to generate the thing we just pushed
                // are also potentially relevant:
                for(int stepi = 0; stepi < rule.num_steps(); ++stepi) {
                     production_rule::step step = rule.nth_step(stepi);
                     if(step && step.is_nonterminal()) {
                         if(!out.count(step.production_name())) {
                             const std::string next = step.production_name();
                             all_wanted.push_back(next);
                             if(nothing_creates(next)) {
                                 missing.insert(stringformat(
                                     "{} (needed by {}, {})",
                                     next, rule.product(), rule.location()
                                 ));
                             }
                         }
                     }
                }
            }
        }

        if(missing.size()) {
            error(prod_location(prod), stringformat(
                "missing the following elements, needed for '{}':\n    {}\n",
                prod, join(missing, "\n    ")
            ));
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

    // returns the state number for the lr_set passed, or -1
    // if there's no such state
    int state_num(const lr_set &state) const {
        auto state_numi = state_index.find(state.id());

        if(state_numi == state_index.end())
            return -1;

        return state_numi->second;
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

        states[start_state].generate_following_states(*this);
    }

    std::string why_cant_use_reducer(
        const reducer &red, const production_rule &rule
    ) {
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
                else if(step.is_nonterminal())
                    out += stringformat(step.production_name());
                else
                    out += stringformat("term_{}", term_name++);
                params++;
            }
        }
        out += ")";
        return out;
    }

    void resolve_precedence_expressions() {
        for(int rulei = 0; rulei < rules.size(); rulei++) {
            production_rule &rule = rules[rulei];
            // the product to which these expressions are relative
            // is the product of the parentmost rule so that you
            // can use relative expressions in subexpressions:
            std::string prod = parentmost_rule(rule).product();
            auto prp = product_precedence.find(prod);
            for(int stepi = 0; stepi < rule.num_steps(); stepi++) {
                grammar_element ge = rule.nth_step(stepi).gexpr;
                if(ge.is_placeholder()) {
                    int pg_ind = -1;
                    std::string errmsg;
                    if(prp == product_precedence.end()) {
                        errmsg = stringformat(
                            "{} is not in any precedence list", prod
                        );
                    } else if(ge.expr == "<<") {
                        pg_ind = 0;
                    } else if(ge.expr == "<") {
                        pg_ind = prp->second - 1;
                        if(pg_ind < 0) {
                            errmsg = stringformat(
                                "nothing has precedence less than {}", prod
                            );
                        }
                    } else if(ge.expr == ".") {
                        pg_ind = prp->second;
                    } else if(ge.expr == ">") {
                        pg_ind = prp->second + 1;
                        if(pg_ind >= precedence_group_names.size()) {
                            errmsg = stringformat(
                                "nothing has precedence greater than {}", prod
                            );
                        }
                    } else if(ge.expr == ">>") {
                        pg_ind = precedence_group_names.size() - 1;
                    }

                    if(pg_ind < 0 || pg_ind >= precedence_group_names.size()) {
                        if(!errmsg.size()) {
                            errmsg = stringformat(
                                "no precedence defined for {}", ge.expr
                            );
                        } // else there's already a message for the situation
                    }

                    if(errmsg.size()) {
                        error(rule.location(), stringformat(
                            "can't resolve '{}' relative to '{}': {}",
                            ge.expr, prod, errmsg
                        ));
                    } else {
                        rule.resolve_placeholder(
                            stepi, precedence_group_names[pg_ind]
                        );
                        record_element(rule.nth_step(stepi).gexpr);
                    }
                } // else it's not something we need to resolve
            }
        }
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

    // Mark the element passed as "masking" the other element passed.
    // In this context, if an element is "masked" by another element,
    // it means that the existence of the element would potentially
    // preclude the ability of the generated parser to match the other
    // element.
    void record_masking_element(
        const grammar_element &masker, const grammar_element &maskee
    ) {
        masks_elements.insert(std::make_pair(masker, maskee));
    }

    std::list<std::string> bad_suffixes_for_element(
        const grammar_element &el
    ) const {
        std::list<std::string> suffixes;

        auto start = masks_elements.lower_bound(el);
        auto end   = masks_elements.upper_bound(el);
        for(auto sit = start; sit != end; sit++) {
            size_t prefix_len = el.expr.length();
            if(prefix_len > sit->second.expr.length()) {
                // counterintuitively or not, the shorter one is the "mask"
                internal_error(stringformat(
                    "{} masks {}, which is backward",
                    el, sit->second
                ));
            }
            suffixes.push_back(sit->second.expr.substr(prefix_len));
        }

        return suffixes;
    }

    // 
    // It's possible (easy, even) for madness and confusion to arise
    // if you have terminals which are prefixes of other terminals
    // (for example, '+' vs '++' vs '+=').  If the generated parser
    // if naiive about this sort of thing, it can end up parsing
    // something like "foo++" as "foo" "+" "+", which leads to surprise
    // and annoyance all around.
    //
    // So, we prevent such madness and confusion in the generated parser
    // by explicitly not matching a shorter terminal if a longer one would
    // match.  This function figures out and records the elements for which
    // this is relevant.
    //
    // I believe other parser generators (yacc etc) resolve this with
    // the concept of word boundaries.  But, I want fpl to work 
    // Anyway this should be more efficient for the generated parser
    // since it only needs to look for a boundary if it actually matters
    // for the particular terminal, and then we'd only have the check
    // the particular suffixes which matter.
    // 
    // Note that (at present) we only detect/prevent masking of TERM_EXACT
    // vs TERM_EXACT.  Checking vs TERM_REGEX is too complicated for me
    // to deal with right now, and vs TERM_CUSTOM I don't really know
    // what to do because custom could depend on any kind of runtime
    // situation in the parser.  So (for now, anyway) caveat author when
    // using regexes or custom terminals.
    //
    void resolve_terminal_masking() {
        // for each terminal
        //    for each other terminal
        //        if other terminal starts with this terminal,
        //            add other to this terminal's shadow list
        for(auto el : elements) {
            if(el.type == grammar_element::Type::TERM_EXACT) {
                for(auto other_el : elements) {
                    if(other_el == el)
                        continue;
                    if(other_el.type == grammar_element::Type::TERM_EXACT) {
                        size_t len = el.expr.length();
                        if(!other_el.expr.compare(0, len, el.expr)) {
                            // el is a substring of other_el, so it could
                            // potentially "mask" it.  record that fact:
                            record_masking_element(el, other_el);
                        }
                    }
                }
            }
        }
    }

    const std::string default_goal() const {
        // default goal is either:
        // 1) whatever the first precedence group produces
        //    (i.e. the lowest precedence expression in the
        //    grammar))
        if(precedence_group_names.size())
            return precedence_group_names[0];

        // 2) whatever the first top-level rule produces.
        //    "top level" means not a subrule.  Ideally, this
        //    is the first rule specified by the fpl author.
        for(auto rule : rules) {
            if(!rule.is_subexpression()) {
                return rule.product();
            }
        }

        // 3) a production named "goal"
        return "goal";
    }

    void infer_output_type() {
        if(output_type().length() == 0) {
            // We don't know our output type, so infer it from goals.
            // In theory we could return a union or some functional
            // equivalent, but for the moment we require that all
            // goals have the same type.
            std::map<std::string, std::string> goal_for_type;
            for(auto goal: goals) {
                goal_for_type[type_for(goal)] = goal;
            }
            if(goal_for_type.size() == 1) {
                set_output_type(goal_for_type.begin()->first);
            } else {
                // more than one type:  report a comflict
                std::string conflicts;
                for(auto gft : goal_for_type) {
                    conflicts += stringformat(
                        "    goal '{}' produces {}\n", gft.second, gft.first
                    );
                }
                error(stringformat("can't infer output type.\n{}", conflicts));
            }
        } // else we already know our output type
    }

    void resolve_goals() {
        // Goals can be overridden by options.  This lets
        // users test subsets of their grammars (or extract
        // subsets for other purposes).
        if(opts.entry_points.size()) {
            goals = opts.entry_points;
        }

        if(goals.empty()) {
            goals.push_back(default_goal());
        }

        add_goal_rules();
    }

    // goal rules are wrappers used in detecting termination:
    void add_goal_rules() {
        if(rules_for_product.count("_fpl_goal") == 0) {
            // for each goal the author/user wants,
            // make a "completion" rule:
            for(auto goal: goals) {
                production_rule goal_rule(
                    __FILE__, __LINE__,
                    grammar_element::END_OF_PARSE, "_fpl_goal"
                );
                // the rule is always a single step (product -> _fpl_goal)
                goal_rule.add_step(production_rule::step(
                    goal, grammar_element::Type::NONTERM_PRODUCTION
                ));
                add_rule(goal_rule);
            }
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
        resolve_terminal_masking();
        resolve_goals();

        infer_output_type();
        apply_reducers();
        //generate_states(goals);
        generate_states({"_fpl_goal"});
        dump_states(opts);
        resolve_actions();
        check_rules();
    }

    // jemp-based output:
    //  this friends thing is horrible.  can we make them members?
    //  problem then is that the generated code format is then
    //  even more tightly bound to this class.
    //  also, that's not how headers in c++ work.
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
    friend std::string fpl_x_parser_shift_none(
        const productions &prds, const grammar_element &el, const fpl_options &
    );

    std::string generate_code(
        const fpl_options &opts, src_location caller = CALLER()
    ) {
        if(opts.check_only) {
            // This will (I think correctly) splat any @produces
            // type set, but (perhaps not correctly?) not splat
            // specific types set for specific productions.
            // It should cover the "check abstract fpl grammar"
            // case correctly, so I'm going with it for the moment.
            set_output_type("check_only");
        }
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

    void resolve_actions() {
        // A given rule will be reduced according to the first of the
        // following possibilities:
        //   1) abstracted implementations (+product). this is top
        //      priority so that you can use the grammar defined by
        //      non-"pure" fpl and just override anything you need to
        //      without having to change the grammar fpl.
        //      (handled in the production rule/reducer)
        //   2) code from the +{ ... }+ block in the rule definition
        //      (handled in the production rule)
        //   3) the @default_action (handled here)
        //   4) to construct an object of the type for the
        //      production from the result of the steps for the rule
        //      (handled in the .jemp for the reduce action)
        for(int rnum = 0; rnum < rules.size(); rnum++) {
            production_rule &rule = rules[rnum];
            if(!rule.code() && default_action) {
                rule.code(default_action);
            }
        }
    }
};

#include "fpl_x_parser.h"

} // namespace fpl

#endif // PRODUCTIONS_H
