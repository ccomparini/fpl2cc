#ifndef PRODUCTIONS_H
#define PRODUCTIONS_H

#include "code_block.h"
#include "fpl_options.h"
#include "fpl_reader.h"
#include "production_rule.h"
#include "reducer.h"
#include "stack_distance.h"

#include "util/c_str_escape.h"
#include "util/from_hex.h"
#include "util/jerror.h"
#include "util/join.h"
#include "util/reformat_code.h"
#include "util/src_location.h"
#include "util/stringformat.h"
#include "util/to_hex.h"
#include "util/to_utf8ish.h"
#include "util/utf8_buffer.h"

#include <list>
#include <map>
#include <string>
#include <time.h> // for profiling

namespace fpl {

// Predeclarations for jemp-generated functions.
// This is fer da boids.  can we make jemp generate
// member-ready methods or something?  Or otherwise
// not have to do this by hand?
class productions;
std::string fpl_x_parser(const productions &, const fpl_options &);
std::string scan_group_terminal(const productions &, const std::list<grammar_element> &);
std::string scan_inv_group_terminal(const productions &, const std::list<grammar_element> &);
std::string regex_custom_scanner(const productions &, const grammar_element &);

class productions {

    fpl_reader_p      inp;
    const fpl_options opts;

    // for imports and such:
    //   - @grammar is used when you want to implement a parser based
    //     on an exising grammar
    //   - @import imports sets of rules from another fpl into the
    //     current rulespace
    //   - backticks import a nonterminal (including any rules to produce
    //     that nonterminal) for use in an expression context.  eg:
    //       `ansi-c`.declaration -> declaration
    using subgrammar_p = std::unique_ptr<productions>;
    const productions *parent; // prods of which we are a sub, or nullptr
    std::map<std::string, subgrammar_p> sub_productions; // grammar name -> productions
    std::set<std::string> exported_products; // exported to that of which this is a sub

    std::string final_type; // this is what the parser returns. set by @produces or inferred
    std::string def_type; // if not "", fill in unknown types with this
    std::map<std::string, std::string> type_for_product; // (c++ reduce type for particular product)
    std::set<std::string> all_types; // for deduplication

    std::list<std::string> imports; // filenames
    std::list<utf8_buffer> embeds;
    code_block default_action;
    code_block post_parse;
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

    // grammar elements:
    std::vector<grammar_element>    elements;
    std::map<grammar_element, int>  element_index; // element -> element ID

    // key el "masks" value els:
    std::multimap<grammar_element, grammar_element> masks_elements;

    // element -> source location:
    std::map<grammar_element, std::string> source_of_element;

    // rules/reduce actions/products:
    std::vector<production_rule>    rules;
    std::multimap<std::string, int> rules_for_product; // product -> rule ind
    mutable int subex_count; // to generate product names for subexpressions
    std::list<reducer> reducers;

    struct generated_type;
    std::set<generated_type> generated_types;
    bool generate_types;

    class lr_set;
    std::map<int, lr_set> states;  // keyed by state number
    std::map<std::string, int32_t> state_index; // set id -> state number

    // A "rulestep" is basically a way to get at a "real" step
    // from the point of view of a parentmost rule.  (The "real"
    // step may be part of a subrule)
    struct rulestep {
        const productions *owner;
        unsigned rulenum;   // index of the rule containing the step
        unsigned stepnum;   // index of the step within the rule

        rulestep() :
            owner(nullptr), rulenum(-1), stepnum(-1) {
        }

        rulestep(const productions *o, int rn, int sn) :
            owner(o),
            rulenum(rn),
            stepnum(sn) {

        }

        // If this step is in a subexpression, returns the step in
        // the parent rule which represents that subexpression.
        // otherwise, returns a false step.
        rulestep parent_rulestep() const {
            int prln = rule().parent_rule_number();
            if(prln >= 0) {
                int cdp = rule().parent_countdown_pos();
                auto parent_rs = rulestep(owner, prln, -1);
                parent_rs.stepnum = parent_rs.rule().num_steps() - cdp;
                return parent_rs;
            }
            return rulestep(owner, -1, -1);
        }

        // If this step is a subexpression, this returns the step
        // in the non-subexpression rule which contains represents
        // the tree of subexpressions in which this step exists.
        // Otherwise, returns a false step.
        // The result differs from parent_rulestep() only if there
        // are multiple levels of nested subexpressions.
        rulestep parentmost_rulestep() const {
            rulestep parentmost = *this;
            while(auto up = parentmost.parent_rulestep()) {
                parentmost = up;
            }
            return parentmost;
        }

        // .... to my surpsise, I seem to have to explicitly
        // write this?  what am I missing?
        bool operator==(const rulestep &other) const {
            return   (owner == other.owner)
                && (rulenum == other.rulenum)
                && (stepnum == other.stepnum)
            ;
        }

        // convenience:
        bool is_ejected()       const { return step().is_ejected(); }
        bool is_subexpression() const { return step().is_subexpression(); }
        bool is_multiple()      const { return step().is_multiple();      }
        bool is_optional()      const { return step().is_optional();      }

        bool is_in_subexpression() const {
            return rule().parent_rule_number() >= 0;
        }

        // eg:
        //   (x y)* -> foo;
        // x and y are in a subexpression whose step is "multiple",
        // so this would return true.
        bool is_in_multiple_subexpression() const {
            auto prs = parent_rulestep();
            if(prs) {
                return prs.is_multiple();
            }

            return false;
        }


        // Returns the "flat" next rulestep, which is the rulestep
        // after the current rulestep, ignoring multiples/optionals
        // etc. (but traversing parent/child relationships)
        rulestep flat_next(bool no_descend = false) const {
            rulestep result = *this;
            if(result) {
                auto st = result.step();
                if(!st.is_subexpression() || no_descend) {
                    result.stepnum++;
                } else {
                    // descend into the subexpression:
                    result.stepnum = 0;
                    result.rulenum = owner->subrulenum_for_step(st);
                }

                while(!result.step()) {
                    // we hit the end of the current rule.
                    // if it has a parent, go to the step after
                    // the step in that rule.
                    int parent_rulenum = result.rule().parent_rule_number();
                    if(parent_rulenum < 0) {
                        // we're at the end of a top level rule
                        break;
                    } else {
                        int cdp = result.rule().parent_countdown_pos();
                        result.rulenum = parent_rulenum;
                        result.stepnum = result.rule().num_steps() - cdp + 1;
                    }
                }
            }
            return result;
        }

        // returns the set of rulesteps which could legitimately
        // encountered as the start of the rule passed.
        // (there can be more than one due to optionals)
        static std::list<rulestep> rule_starts(
            const productions *owner, unsigned rulenum
        ) {
            std::list<rulestep> result;
            if(owner && (rulenum < owner->rules.size())) {
                rulestep st(owner, rulenum, 0);
                while(st) {
                    result.push_back(st);
                    if(!st.is_optional())
                        break;
                    st = st.flat_next();
                }
            }
            return result;
        }

        // There are 0 or more possible rulesteps which can follow any
        // given rulestep.
        // The first is always the "flat" next (unless we're at the end
        // of a rule).
        // If the current step is multiple, or at the end of a multiple
        // substep, another other possible next step is either itself or
        // the start of the substep.
        // If the "flat" next step is optional, the real next step may
        // also be the step after that step.
        void nexts(std::list<rulestep> &result, bool no_repeat = false) const {
            if(!bool(*this))
                return;

            auto normal_next = flat_next();

            if(normal_next) result.push_back(normal_next);

            auto opt_next = normal_next;
            while(opt_next && opt_next.step().is_optional()) {
                opt_next = opt_next.flat_next(true);
                if(opt_next) {
                    result.push_back(opt_next);
                } else if(!no_repeat && is_in_multiple_subexpression()) {
                    // we "wrapped" past the end of the subrule,
                    // and it's a repeated subrule. so, the next
                    // step is any of the starts of that subrule:
                    result.splice(
                        result.end(), rule_starts(owner, rulenum)
                    );
                }
            }

            if(!no_repeat) {
                auto this_step = step();
                if(this_step.is_multiple() && !this_step.is_subexpression()) {
                    // simple multiple, so possible nexts include ourself:
                    result.push_back(*this);
                }

                int prln = rule().parent_rule_number();
                if(prln >= 0) {
                    if(stepnum + 1 >= rule().num_steps()) {
                        // next is at (or past) the end of a substep.
                        // let's see what that substep looks like:
                        int cdp = rule().parent_countdown_pos();
                        auto parent_rs = rulestep(owner, prln, -1);
                        parent_rs.stepnum = parent_rs.rule().num_steps() - cdp;
                        if(parent_rs.step().is_multiple()) {
                            // .. it's multiple, so we'll need to wrap
                            // back to the start:
                            parent_rs.nexts(result, true);
                        }
                    }
                }
            }
        }

        std::list<std::list<rulestep>> paths(
            std::list<rulestep> in = {},
            bool no_repeat = false
        ) const {
            std::list<rulestep> path_so_far = in;

            path_so_far.push_back(*this);

            std::list<std::list<rulestep>> result;

            std::list<rulestep> next_steps;
            nexts(next_steps, no_repeat);
            if(next_steps.size() == 0) {
                result.push_back(path_so_far);
                return result;
            }
            for(auto next : next_steps) {
                if(next.rulenum == rulenum) {
                    if(next.stepnum <= stepnum) {
                        // we're looking at an earlier step in the
                        // same rule, so we've recursed once on it.
                        // don't do it again:
                        no_repeat = true;
                    }
                }
                result.splice(
                    result.end(), next.paths(path_so_far, no_repeat)
                );
            }

            return result;
        }

        // returns a string containing all the "paths" forward 
        // through various rules from this rulestep.
        std::string paths_string(const std::string &prefix = "") {
            std::string result;
            for(auto path : paths()) {
                result += prefix;
                for(auto rs : path) {
                    result += rs.to_str() + " ";
                }
                result += "\n";
            }
            return result;
        }

        const production_rule &rule() const {
            if(owner && (rulenum < owner->rules.size()))
                return owner->rules[rulenum];

            return no_rule();
        }

        // const production_rule::step &step() const {
        const production_rule::step step() const {
            auto rl = rule();
            if(rl) {
                return rl.nth_step(stepnum);
            }
            return production_rule::step::false_step();
        }

        operator bool() const {
            return rule() && step();
        }

        // If this rulestep is the canonical step for a
        // parameter, this returns the distance in param
        // stack entries to the next value in that parameter.
        // If it's not the canonical step, all bets are off.
        // Throws an error if the meld is invalid.
        // 
        // This is what code generators should use to find
        // the meld skip.
        int final_meld() const {
            stack_distance meld = step().meld_distance;

            if(meld.is_indeterminate()) {
                jerror::error(stringformat(
                    "{} bad meld ({}) for step {} in {}!", 
                    rule().location(), meld, step(), rule()
                ));
            }

            return meld.to_int();
        }

        // Returns the index of the final step within the top-level
        // rule which would be involved in the meld for this step,
        // if it's the canonical step for the variable.
        // If it's not the canonical step, then the result is probably
        // meaningless (but will to be the last step with the same parameter
        // name as this step, so maybe fine).
        int last_meld_step() const {
// SOME TESTS TO ADD:
// say we have (foo bar)* foo -> bat;
// foo's last meld step is 1, but bar's is 0.
// say we have (foo bar foo bat)* foo -> moo;
// foo has a different meld from bar and bat.. what's the canonical step?
// test the above.
            auto pname = step().variable_name();
            auto lastest = *this;
            for(auto nx = flat_next(); nx; nx = nx.flat_next()) {
                if(nx.step().variable_name() == pname) {
                    lastest = nx;
                }
            }
            return lastest.parentmost_rulestep().stepnum;
        }

        // If this is the canonical step for a parameter,
        // returns the number of param stack entries from
        // the base step in the top level rule.
        //
        // In all, the nth param stack entry for a given
        // reduce parameter is at offset:
        //     final_offset + n*final_meld
        // .. from the entry for the top level step.
        int final_offset() const {
            auto pname = step().variable_name();
            int dist = 0;
            auto rstep = this->parentmost_rulestep();

            for( ; rstep ; rstep = rstep.flat_next()) {

                if(rstep == *this) {
                    return dist;
                }

                if(rstep.is_ejected()) {
                    continue; // skip because no param stack entry
                }

                if(rstep.is_subexpression()) {
                    if(rstep.is_multiple() || rstep.is_optional()) {
                        auto subr = owner->sub_rule(rstep.step());
                        // if this step contains the parameter we're
                        // looking for, it's ok for it to be optional/
                        // multiple, but if not, the optional/mutiple-ness
                        // makes the distance indeterminate, so:
                        if(!subr.parameter_step(pname)) {
                            break;
                        }
                    }

                    continue; // .. i.e. iterate into the sub
                }

                if(rstep.is_multiple() || rstep.is_optional()) {
                    break;
                }
                ++dist;

            }

            if(rstep.is_multiple() || rstep.is_optional()) {
                jerror::error(stringformat(
                    "{} indeterminate offset for step {} due to {}", 
                    rule().location(),
                    owner->stepstring(step()),
                    owner->stepstring(rstep.step())
                ));
            }

            return dist; // I guess?  it's an error if we got here
        }

        // returns the type for this argument in the target language
        std::string type_in_target() const {
            return owner->type_for(step().gexpr);
        }

        std::string to_str(bool plain = false) const {
            return stringformat(
                "<{}.{} = {}>", rulenum, stepnum, owner->stepstring(step())
            );
        }
    };

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
        src_location definition_location = CALLER()
    ) {

        if(!nge) {
            jerror::warning(stringformat(
                "attempt to record invalid element '{}'", nge
            ), definition_location);
        }

        // (recording placeholder elements is not useful,
        // and is in fact harmful because you get weird
        // duplicates)
        if(nge.is_placeholder())
            return;

        if(element_index.find(nge) == element_index.end()) {
            element_index[nge] = elements.size();
            elements.push_back(nge);

            source_of_element.insert(std::make_pair(nge, definition_location));
        }
    }

    // returns the file:line (or other indication) of where we think
    // this element was defined.
    std::string element_source(const grammar_element &el) const {
        auto found = source_of_element.find(el);
        if(found != source_of_element.end())
            return found->second;

        return "<no source>";
    }

    struct lr_item {
        // Modified from classic lr - in our case,
        // there's no "or" in rules so we can just
        // refer to the rule itself.  So each item
        // can be encoded as a rule ID and the step
        // within that rule:
        uint16_t rule;      // offset into rules (i.e. rule number)
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
        // of (or past the end of) the rule. (i.e. the reduce
        // step is a "false" one)
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

                    // stepi >= 0 so we get the dots past the end on
                    // final steps
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
            if(right_of_dot) {
                return stringformat("{}: ({} {})",
                    right_of_dot, type_to_str(what), which
                );
            } else {
                return stringformat("({} {})", type_to_str(what), which);
            }
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
        mutable lr_transitions transition_cache;
        lr_transitions transitions(const productions &prds) const {
            if(transition_cache.size())
                return transition_cache;

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
            transition_cache = out;
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

        // recursively generates any following states (i.e., states to
        // which we might transition from this one), adding them to the
        // productions passed.
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

    // Represents the need to generate a type for a particular product.
    // Provides the information needed (by the code generator) to generate
    // that type.
    struct generated_type {
        class attribute {
            std::string attr_name;
            std::set<grammar_element> elements; // set of elements possibly generating this
            std::string type;
            bool multiple;    // if true, store in a way allowing 0 or more

        public:

            attribute(const production_rule::step &step, src_location caller = CALLER()) :
                attr_name(step.variable_name()),
                elements({step.gexpr}),
                multiple(!step.is_single()) {
            }

            void merge(const attribute &other) {
                for(auto el : other.elements) {
                    elements.insert(el);
                }

                // if any of the possible things with this name are
                // not limited to being singular, the whole thing is
                // multiple:
                if(other.multiple) {
                    multiple = true;
                }
            }

            std::string name() const {
                return attr_name;
            } 

            std::string to_str() const {
                return stringformat(
                    // if there's more than one element, show them
                    // joined with '|' (i.e. "or")
                    "    {}:{}{}\n", join(elements, "|"), name(), multiple?"*":""
                );
            }

            // so we can make a std::set
            friend bool operator<(const attribute &lhs, const attribute &rhs) {
                return lhs.name() < rhs.name();
            }

            std::string type_in_target(
                const productions &prds, src_location caller = CALLER()
            ) const {
                bool type_conflict = false;
                std::string type;
                for(auto el : elements) {
                    auto candidate = prds.type_for(el);
                    if(!candidate.length()) {
                        jerror::warning(stringformat(
                            "No type known for {} defined at {}\n",
                            el, prds.element_source(el)
                        ));
                    } else {
                        if(type.length())
                            type_conflict = true;
                        else
                            type = candidate;
                    }
                }

                if(type_conflict) {
                    jerror::warning(stringformat(
                        "Conflicting types for '{}' in generated type:\n{}\n",
                        name(),
                        join(elements, "\n", [&prds] (auto el) -> std::string {
                            auto type_name = prds.type_for(*el);
                            if(!type_name.length()) type_name = "<no type>";
                            return stringformat(
                                "        {}\t{} at {}",
                                type_name, *el, prds.element_source(*el)
                            );
                        })
                    ));
                }

                return type;
            }

            void resolve_type(const productions &prds) {
                type = type_in_target(prds);
            }
        };

        using attribute_set = std::set<attribute>;
        attribute_set attributes;
        std::string type_name;

        generated_type(const std::string &pr, const productions &prds) :
            type_name(stringformat("_generated_type_for_{}", pr)) {
            init_attributes(pr, prds);
        }

        // this is so they can be looked up by name:
        generated_type(const std::string &tn) : type_name(tn) { }

        std::string to_str() const {
            return stringformat("{}: {}", type_name, attributes);
        }

        std::string name() const {
            return type_name;
        }

        // generated types can be looked up by name in a std::set or similar:
        friend bool operator<(
            const generated_type &left, const generated_type &right
        ) {
            return left.type_name < right.type_name;
        }

        void init_attributes(
            const std::string &for_product, const productions &prds
        ) {
            auto strl  = prds.rules_for_product.lower_bound(for_product);
            auto endrl = prds.rules_for_product.upper_bound(for_product);
            for(auto rit = strl; rit != endrl; ++rit) {
                const production_rule &rule = prds.rules[rit->second];
                const int num_params = rule.parameter_count();
                for(int param = 0; param < num_params; param++) {
                    const production_rule::step st = rule.parameter_step(param);

                    if(st.skip_on_reduce())
                        continue; // reducers never see this one anyway - skip

                    attribute attr(st);
                    auto existing_attr = attributes.find(attr);
                    if(existing_attr != attributes.end()) {
                        attr.merge(*existing_attr);
                    }
                    attributes.insert(attr);
                }
            }
        }

        void resolve_attribute_types(const productions &prds) {
            attribute_set new_attrs;
            for(auto attr : attributes) {
                attr.resolve_type(prds);
                new_attrs.insert(attr);
            }
            attributes = new_attrs;
        }
    };

    static size_t separator_length(const utf8_byte *inp) {
        // sh-style "#" comments:
        size_t len = 0;
        if(inp[0] == '#') {
            len++;
            while(inp[len] && inp[len] != '\n') {
                len++;
            }
        }

        // regular whitespace:
        while(inp[len] && (
            inp[len] == ' '   ||
            inp[len] == '\t'  ||
            inp[len] == '\n'  ||
            inp[len] == '\v'  ||
            inp[len] == '\f'  ||
            inp[len] == '\r'
        )) {
            len++;
        }

        // huh... how does this work if we don't loop here?

        return len;
    }

    size_t eat_separator() {
        return inp->eat_separator(&separator_length);
    }

public:

    productions(
        const fpl_options &op, fpl_reader_p src, productions *p = nullptr
    ) :
        inp(src), opts(op), parent(p),
        default_main(false), subex_count(0), generate_types(false)
    {
        // element 0 is a null element and can be used to
        // indicate missing/uninitialized elements or such.
        // we count it as a nonterminal so that it can be
        // accessed via the enum etc.
        record_element(
            grammar_element("_fpl_null", grammar_element::NONTERM_PRODUCTION)
        );

        // terminals are always a thing:
        all_types.insert("Terminal");
    }

    std::string input_filename() const {
        if(inp) return inp->filename();
        return "";
    }

    // returns the name of the target-language type expected as
    // the result of reducing to the product indicated:
    std::string type_for(
        const std::string &product,
        src_location caller = CALLER()
    ) const {
        // if there's a specific type already designated for this
        // product, use that:
        auto tf = type_for_product.find(product);
        if(tf != type_for_product.end()) {
            if(opts.debug_types) {
                std::cerr << stringformat(
                    "telling {} that {} has specific type {}\n",
                    caller, product, tf->second
                );
            }
            return tf->second;
        }

        // if it's a goal, use the output type
        if(is_goal(product)) {
            if(opts.debug_types) {
                std::cerr << stringformat(
                    "telling {} that {} has goal type {}\n",
                    caller, product, output_type()
                );
            }
            return output_type();
        }

        // can't infer type:
        if(opts.debug_types) {
            std::cerr << stringformat(
                "telling {} that we don't know the type for {}\n",
                caller, product
            );
        }
        return "";
    }

    // and this returns the type to use for a particular grammar
    // element, which covers terminals as well:
    std::string type_for(
        const grammar_element &ge,
        src_location caller = CALLER()
    ) const {
        if(ge.is_nonterminal()) {
            return type_for(ge.expr, caller);
        }
        // assertions are terminals as well:
        return "Terminal";
    }

    // .. and one more convenience, because this is many-layered
    // in places:
    std::string type_for(
        const production_rule::step &st,
        src_location caller = CALLER()
    ) const {
        return type_for(st.gexpr, caller);
    }

    enum code_source{
        // these are bit fields so that parsers or users of the code
        // block can specify what types might be valid in a particular
        // context:
        INLINE = 1,
        LIB    = 2,
        REGEX  = 4,

        INLINE_OR_REGEX = INLINE | REGEX,
        INLINE_OR_LIB   = INLINE | LIB,
        ANY             = INLINE | LIB | REGEX
    };
    inline code_block code_for_directive(
        const std::string &dir,
        code_source allowed_src = INLINE,
        bool allow_empty = false
    ) {
        eat_separator();

        auto filename = inp->filename();
        auto line     = inp->line_number();

        code_block code;

        if(allowed_src & INLINE) {
            // try "regular" +{ }+ code blocks:
            code = read_code();
        }

        std::string errm;
        if(!code && (allowed_src & REGEX)) {
            code = parse_regex_code_block();
        }

        if(!code && (allowed_src & LIB)) {
            // expect a quoted (or otherwise string-delimited)
            // file basename:
            std::string fn = inp->parse_string();
            if(fn.length() > 0) {
                code = code_block::from_file(
                    fn + ".inc", opts.src_path,
                    filename, line
                );
            }
        }

        if(!code) {
            if(allow_empty) {
                // set the location of the empty code block anyway,
                // so callers can use it for reporting
                code.source_file = filename;
                code.line        = line;
            } else if(allowed_src) {
                errm = stringformat("expected code for directive {}", dir);
            } else {
                errm = stringformat(
                    "Internal error: "
                    "code for directive {} not allowed from anything",
                    dir
                );
            }
        }

        if(errm != "")
            error(errm);

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

    void set_default_type(const std::string &tp) {
        all_types.insert(tp);
        def_type = tp;
    }

    bool has_default_type() const {
        return def_type.length() > 0;
    }

    std::string default_type() const {
        return def_type;
    }

    void set_post_parse(const code_block &cb)  { post_parse = cb; }
    void set_default_main(bool def)            { default_main = def; }
    void add_internal(const code_block &cb)    { parser_members.push_back(cb); }

    void add_type_for(
        const std::string &prod, const std::string &type,
        const std::string &why = "just cuz",
        src_location caller = CALLER()
    ) {
        if(type == "") {
            jerror::warning("Bug in fpl2cc {}: adding empty type\n", caller);
        } else {
            if(opts.debug_types) {
                std::cerr << stringformat(
                    "{} gets type {} ({})\n", prod, type, why
                );
            }
            type_for_product[prod] = type;
            all_types.insert(type);
        }
    }

    void set_precedence(const std::string &product, int precedence) {
        product_precedence[product] = precedence;
    }

    // Tries to parse a [ ]-bracketed set of terminals (a "group terminal"),
    // adding each to the std::sets passed.
    // Returns true if it parsed one, or false if it didn't recognize
    // a compound terminal.
    // Throws parse errors on malformed input.
    bool parse_terminal_list(
        std::list<grammar_element> &terms
    ) {
        if(!inp->read_exact_match("[")) {
            return false;
        }

        eat_separator();
        bool invert_next = false;
        while(!inp->read_exact_match("]")) {
            if(inp->read_exact_match("!")) {
                invert_next = !invert_next;
                eat_separator();
            }

            auto element = parse_terminal();
            if(!element) {
                error("expected some kind of terminal in terminal list");
                break;
            } else {
                if(invert_next) {
                    if(!element.invert_type())
                        error(stringformat("can't invert {}"));
                    invert_next = false;
                }
                terms.push_back(element);
            }
            eat_separator();
        }

        return true;
    }

    std::string parse_scanner_name() {
        std::string name = read_identifier(inp);
        if(name != "") {
            if(scanners.count(name)) {
                const code_block &existing = scanners[name];
                if(!existing.is_stub) {
                    warn(stringformat(
                        "scanner {} overwrites existing scanner at {}\n",
                        name, existing.location()
                    ));
                }
            }
        }

        return name;
    }

    void parse_scanner() {
        std::string name = parse_scanner_name();

        if(name == "") {
            error("expected name of scanner");
        } else {
            code_block scanner = code_for_directive(
                "scanner", code_source::INLINE_OR_REGEX, true
            );
            code_block inverse;

            if(!scanner) {
                // No scanner was specified, so stub one.
                // Stub scanners never match.  If the stub scanner
                // never gets overridden, we'll warn so that the
                // author knows they need to implement something,
                // but the stub will still "work".
                // (This allows authors to stub scanners in an abtract
                // grammar without having to deal with implementation)
                scanner.stub("return Terminal();");
            }

            // if the scanner is regex-implemented, convert
            // that scanner to target code and make an inverse
            // so that we have fewer special cases down the line
            if(scanner.language == code_block::REGEX) {
                auto regex = scanner.code;
                inverse = code_block(
                    regex_custom_scanner(*this, grammar_element(
                        regex, grammar_element::TERM_REGEX_INV
                    )),
                    code_block::DEFAULT,
                    scanner.source_filename(), scanner.line_number()
                );
                scanner = code_block(
                    regex_custom_scanner(*this, grammar_element(
                        regex, grammar_element::TERM_REGEX
                    )),
                    code_block::DEFAULT,
                    scanner.source_filename(), scanner.line_number()
                );
            }
          
            scanners[name] = scanner;
            if(inverse) {
                scanners[inverse_scanner_name(name)] = inverse;
            }
        }
    }

    // given the name of a custom scanner, returns the name to use
    // for the corresponding inverse scanner (which may or may not
    // actually exist)
    std::string inverse_scanner_name(const std::string &name) const {
        return stringformat("_inv_{}", name);
    }

    // As parse_scanner, this creates a custom terminal.
    // This version, however, allows syntax for terminal
    // groups and inverting matches.
    void parse_custom_terminal() {
        std::string name = parse_scanner_name();
        if(name == "") {
            error("expected name for terminal definition");
        } else {
            eat_separator();
            if(inp->peek() == '[') {
                // this terminal matches anything in the set
                // of the terminals within the [ ]
                std::list<grammar_element> sub_terms;
                auto start = inp->current_position();
                if(parse_terminal_list(sub_terms)) {
                    scanners[name] = code_block(
                        scan_group_terminal(*this, sub_terms),
                        code_block::DEFAULT,
                        inp->filename(), inp->line_number(start)
                    );
                    scanners[inverse_scanner_name(name)] = code_block(
                        scan_inv_group_terminal(*this, sub_terms),
                        code_block::DEFAULT,
                        inp->filename(), inp->line_number(start)
                    );
                }
            } else {
                scanners[name] = code_for_directive(
                    stringformat(
                        "custom terminal {} at {}",
                        name, inp->location_str()
                    ),
                    code_source::INLINE_OR_REGEX
                );
                eat_separator();
                if(inp->read_exact_match("inverse")) {
                    if(scanners[name].language == code_block::REGEX) {
                        // regex scanner inversion is already covered,
                        // and I'm thinking it'll lead to inconsistencies
                        // and confusion if someone tries to hand code an
                        // inverse of a regex match (which, in the present
                        // implementation, wouldn't even get called), so
                        // for now I'm just going to disallow it.
                        error(stringformat(
                            "can't override inverse of regex scanner '{}'",
                            name
                        ));
                    }
                    eat_separator();
                    scanners[inverse_scanner_name(name)] = code_for_directive(
                        stringformat(
                            "inverse of custom terminal {} at {}",
                            name, inp->location_str()
                        ),
                        code_source::INLINE
                    );
                }
            }
        }
    }

    std::string arg_for_directive() {
        // Reads an argument to the end of the line.
        // End of line is any ascii vertical space (for now).
        // Leading and trailing spaces/tabs are stripped.
        // (note the non-greedy +? match to keep the trailing
        // space out)
        return inp->read_re("[ \\t]*([^\\x0a\\x0d]+?)[ \\t]*[\\x0a\\x0d]")[1];
    }

    // this is some kind of utility thing which probably doesn't
    // belong here, but it here because we need it and I'm already
    // layers deep in yak shave..
    // splits the given string on spaces and returns a set of the things
    // in it.
    // actually it's a reverse join (on space).  maybe nioj()?
    static std::set<std::string> space_set(const std::string &in) {
        std::set<std::string> out;
        const std::string ws("\t "); // just tab or space actually
        size_t next = in.npos;
        for(auto start = in.find_first_of(ws); start != in.npos; start = next) {
            next = in.find_first_of(ws, start);
            if(next != in.npos) {
                out.insert(in.substr(start, next - start));
            } else {
                out.insert(in.substr(start));
            }
        }
        return out;
    }

    void parse_goal() {
        std::string goal;
        if(inp->peek() == '`') {
            // you can specify that you're importing a goal with
            // backtick syntax; eg: `grammar name`.goal_product
            // ... maybe we need some more generic parse_product...
            goal = parse_import();
        } else {
            goal = read_identifier(inp);
            if(inp->read_re("[\t ]*\n").empty()) {
                error("unexpected input after goal identifier");
            }
        }

        if(!goal.length()) {
            error("expected goal name");
        } else {
            goals.push_back(goal);
        }
    }

    void parse_directive(const std::string &dir) {
        if(dir == "comment") {
            // specifies a comment as a set of 2 terminals - start and end.
        } else if(dir == "comment_style") {
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
            default_main = true;
        } else if(dir == "default_type") {
            set_default_type(read_cpp_type(inp));
        } else if(dir == "embed") {
            // embed the contents of a .h (or similar) file in the output:
            auto emfile = opts.embed_include_path.find(arg_for_directive());
            embeds.emplace(embeds.end(), utf8_buffer(emfile));
        } else if(dir == "generate_types") {
            // tell this to generate a type for anything whose
            // type it otherwise can't infer:
            generate_types = true;
        } else if(dir == "goal") {
            parse_goal();
        } else if(dir == "grammar") {
            // @grammar <grammar name> means this fpl is an implementation
            // or augmentation of the specified (possibly abstract) grammar,
            // so we'll want to import it:
            import_grammar(arg_for_directive());
        } else if(dir == "import") {
            // import the rules from another fpl:
            eat_separator();
            parse_import();
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
        } else if(dir == "produces") {
            set_output_type(arg_for_directive());
        } else if(dir == "scanner") {
            // Note: this is superceded by @terminal; perhaps
            // it shall be deprecated.
            parse_scanner();
        } else if(dir == "separator") {
            add_separator_code(code_for_directive(dir, code_source::ANY));
        } else if(dir == "terminal") {
            // @terminal is like @scanner.  both generate custom scanners.
            // @terminal supports inverting matches and multiple terminal
            // syntax.
            parse_custom_terminal();
        } else if(dir == "type_for") {
            eat_separator();
            std::string prod = read_production_name(inp);
            eat_separator();
            std::string type = read_cpp_type(inp);
            if(prod.length() && type.length()) {
                add_type_for(prod, type, "@type_for");
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

    // If the step passed is a subexpression,
    // returns the index of the rule for the subexpression.
    // Otherwise, returns -1.
    int subrulenum_for_step(const production_rule::step &st) const {
        return subrulenum_for_el(st.gexpr);
    }

    const production_rule &rule(int rnum, src_location cl = CALLER()) const {
        if(rnum < rules.size()) {
            return rules[rnum];
        }
        warn(stringformat("invalid rule number {} at {}\n", rnum, cl));
        static production_rule dummy;
        return dummy;
    }

    // If there's exactly one rule for the product passed, this
    // returns a reference to it.
    // Otherwise, returns a reference to a false rule.
    const production_rule &single_rule_for_product(
        const std::string &prodname
    ) const {
        static production_rule no_rule;
        if(rules_for_product.count(prodname) == 1) {
            return rule(rules_for_product.lower_bound(prodname)->second);
        }
        return no_rule;
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

            record_element(ge, rule.location());

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
        record_element(rule.product_element(), rule.location());

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
        }
    }

    static void read_suffix(fpl_reader_p &src, production_rule::step &expr) {
        if(src->read_byte_equalling('^')) {
            expr.eject = true;
        } else if(src->read_byte_equalling(':')) {
            // the name to give the argument corresponding
            // this this step follows:
            expr.varname = src->read_re("[A-Za-z][A-Za-z0-9_]*")[0];
            expr.explicitly_named = true;
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

    static inline std::string read_cpp_type(fpl_reader_p &src) {
        return src->read_re(".*")[0]; // good enough for now. shipit.
    }

    static inline std::string read_directive(fpl_reader_p &src) {
        return src->read_re("([A-Za-z][A-Za-z0-9_]+)\\s*")[1];
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


    // Returns the set of names of all imported files.
    inline std::set<std::string> imported_files() const {
        // Hopefully the compiler does something smart and doesn't
        // copy all these strings over and over.. :/
        std::set<std::string> out;
        out.insert(imports.begin(), imports.end());
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
        std::ifstream in(filename);
        if(!in.is_open()) {
            error(whence, stringformat(
                "can't open '{}' ({}) for import: {}\n",
                filename, fpl_name, std::string(strerror(errno))
            ));
        }

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

    // Imports relevant rules into this and returns the name of
    // the top level production created
    // syntax: quote grammar_name quote ('.' product_name)?
    // So the grammar name is quoted with whatever kind of quotes,
    // and is optiinally followed by '.' and the name of
    // the product to import from the grammar.
    // Returns the name of the top level product imported
    // (which should be the specified product if specified,
    // and otherwise is the goal....)
    // except what if there are multiple goals?
    std::string parse_import() {
        SourcePosition whence(inp);
        std::string grammar_name(inp->parse_string());
        if(!grammar_name.length()) {
            error(inp, "no grammar name specified");
            return "<failed import>";
        }

        auto subg = subgrammar(grammar_name, whence);

        // `grammarname`.production means import only the
        // specified production (rules and whatever):
        std::string prod_name;
        if(inp->read_byte_equalling('.')) {
            prod_name = read_production_name(inp);
            if(!prod_name.length()) {
                error("couldn't read production name after '.'");
            } else {
                auto dependencies = subg->dependent_products(prod_name);
                import_rules_for_products(subg, dependencies);
            }
        } else {
            // (empty products set means import everything)
            import_rules_for_products(subg, {});

            int num_goals = subg->goals.size();
            if(num_goals == 0) {
                prod_name = subg->default_goal();
            } else {
                // errf.. need better structure. check this higher.
                // Alternately, if there's more than one goal,
                // we _could_ create an intermediate product
                // and jam some rules in like:
                //   for each goal -> that intermediate product
                // (and return the name of the intermediate..)
                if(num_goals > 1) 
                    std::cerr << "TOO MANY GOALS\n";
                prod_name = *(subg->goals.begin());
            }
        }

        return prod_name;
    }

    production_rule::step parse_import_expression() {
        // parse/import the sub-fpl, and use whatever it produces:
        std::string name = parse_import();
        return production_rule::step(
            name,
            grammar_element::Type::NONTERM_PRODUCTION
        );
    }

    std::string make_sub_prod_name() const {
        if(parent) {
            // to avoid name collision madness when there are multiple
            // subgrammars each with subexpressions, sub production names
            // need to be generated by the parentmost productions:
            return parent->make_sub_prod_name();
        }
        return stringformat("_subex_{}", subex_count++);
    }

    // returns a step representing the subexpression, or a
    // false step if whatever's at the current input
    // position doesn't appear to be a subexpression.
    // (may also toss errors on failure)
    production_rule::step parse_subexpression() {
        if(inp->read_byte_equalling('(')) {
            // subexpressions are implemented as sub rules,
            // so we need to make a rule:
            production_rule subrule(
                inp->filename(), inp->line_number(),
                grammar_element::NONTERM_SUBEXPRESSION
            );
            std::string subname = make_sub_prod_name();
            subrule.product(subname);
            parse_expressions(subrule);
            if(!inp->read_byte_equalling(')')) {
                error(inp, stringformat(
                    "expected ')' for subexpression starting at {}\n",
                    subrule.location()
                ));
            }

            add_rule(subrule);

            return production_rule::step(
                subname, grammar_element::NONTERM_SUBEXPRESSION
            );
        }

        return production_rule::step();
    }

    //
    // converts c-like escape sequences:
    //   \a          0x07 ascii BEL ("alert")
    //   \b          0x08 ascii BS (backspace/ctrl-h)
    //   \t          0x09 ascii HT (horizontal tab)
    //   \n          0x0A ascii NL (newline)
    //   \v          0x0B ascii VT (vertical tab)
    //   \f          0x0C ascii FF (formfeed/page break)
    //   \r          0x0D ascii CR (carriage return)
    //   \e          0x1B ascii ESC (escape) (differs from c)
    //   \xhh?       One or 2 hex digits -> 1 byte 
    //   \[uU]h{1,8} "Unicode" -> utf8-ish (differs from c,
    //               and not strictly unicode - see below)
    //   \.          whatever byte follows the backslash
    //               (differs from c - see below)
    //
    // Differences from c:
    //  - no octal support
    //  - \e is apparently nonstandard in c, but we have it
    //  - \[anything else] silently resolves to [anything else],
    //    which implicitly supports '\\', '\'', '"' and '\?'
    //    NOTE that this applies to things like invalid hex or
    //    \[uU] sequences: if the input is "\xyubba", the 
    //    output is "xyubba", and this is not treated as an error.
    //  - \xh is a valid 1 digit hex sequence. As with most
    //    numerals, leading 0 is implied, so \xf = 0x0f etc.
    //  - For "Unicode", U and u are interchangable, and you can
    //    specify 1 to 8 hex digits.  Whatever you give it will
    //    0-padded on the left and encoded into a utf-8 style
    //    series of bytes (i.e. high order bits signifying how
    //    many additional bytes follow in the current code point).
    //    No checks for valid unicode are performed.
    //    As such, anything <= 0x7f is the same in \Uhh or \xhh
    //    notation, and anything > 0x10ffff is guaranteed not to
    //    be actual unicode (though it may still be utf-8 encodable).
    //    Also (because the encoding "runs out" of bits in the
    //    first byte), the maximum encodable value is \u7fffffff.
    //    If the sequence specified can't be utf-8-style encoded,
    //    it will be treated as an escaped U or u followed by
    //    arbitrary text.
    //
    std::string convert_escapes(const std::string &src) const {
        std::string out;
        std::string tmp;
        char u_or_U = 'U';
        out.reserve(src.length());
        int ind = 0;
        while(ind < src.length()) {
            // utf-8 note:  backslash is 0x5f = 0b01011100.
            // multi-byte utf-8 chars all have their high bits
            // set, so (on valid utf-8) the backslash will never
            // abridge a utf-8 character (thus this should dtrt)
            if(src[ind] == '\\') {
                ++ind; // skip the backslash
                switch(src[ind]) {
                    case 'a': out += '\x07'; ind++; break;
                    case 'b': out += '\x08'; ind++; break;
                    case 't': out += '\x09'; ind++; break;
                    case 'n': out += '\x0A'; ind++; break;
                    case 'v': out += '\x0B'; ind++; break;
                    case 'f': out += '\x0C'; ind++; break;
                    case 'r': out += '\x0D'; ind++; break;
                    case 'e': out += '\x1B'; ind++; break;
                    case 'x':  // 2 hex chars max
                        ind++; // skip 'x'
                        tmp = from_hex<char>(src, ind, 2); // modifies ind
                        if(tmp.length()) {
                            out += tmp;
                        } else {
                            // no/invalid hex digits, so it's not a proper
                            // hex escape sequence: copy the x "normally":
                            out += "x";
                        }
                        break;
                    case 'u':
                        u_or_U = 'u';
                    case 'U': {
                        ind++; // skip the 'u'/'U'
                        uint32_t codepoint = from_hex<uint32_t>(src, ind);
                        tmp = to_utf8ish<std::string>(codepoint);
                        if(tmp.length()) {
                            out += tmp;
                        } else {
                            // unencodable pseudo-codepoint, so again
                            // we're just copying the 'u' as if it had
                            // been any other escaped character
                            out += u_or_U;
                        }
                        u_or_U = 'U';
                        break;
                    }
                    default:
                        out += src[ind++]; // just copy the char
                        break;
                }
            } else {
                out += src[ind++];
            }
        }
        return out;
    }

    // Remove backslashes which were used for escapes in single
    // quoted strings.  The 2 cases are '\'' and '\\'.  Everything
    // else is left intact.
    static void remove_single_quote_escapes(std::string &str) {
        for(size_t pos = 0; pos < str.length(); ++pos) {
            if(str[pos] == '\\') {
                if(pos + 1 < str.length()) {
                    if((str[pos+1] == '\\') || (str[pos+1] == '\'')) {
                        str.replace(pos, 1, "");
                    }
                }
            }
        }
    }

    // If there's a single, obvious name for the variable to use
    // for the subexpression passed, this returns it.  Otherwise,
    // returns an empty string.
    std::string subex_varname(const std::string &subex) const {
        if(auto rule = single_rule_for_product(subex)) {
            if(auto step = rule.single_param()) {
                return step.variable_name();
            }
        }

        return "";
    }

private:
    production_rule &sub_rule(const production_rule::step &step) {
        static production_rule no_rule;
        if(step.is_subexpression()) {
            auto sname = step.gexpr.expr;
            if(rules_for_product.count(sname) == 1) {
                return rules[rules_for_product.lower_bound(sname)->second];
            }
        }
        if(no_rule) {
            // no_rule somehow became a true value.
            std::cerr << stringformat(
                "dummy rule {} returned by subrule() has been modified",
                no_rule
            );
            // reset it:
            no_rule = production_rule();
        }
        return no_rule;
    }
public:

    const production_rule &sub_rule(const production_rule::step &step) const {
        // sigh on the const_cast, but I don't think there's a better
        // way to do this in c++...
        return const_cast<productions *>(this)->sub_rule(step);
    }

    void set_subex_names(
        const production_rule::step &for_step,
        const std::string &name
    ) {
        if(!name.length()) return;

        production_rule &subrule = sub_rule(for_step);
        if(!subrule) {
            internal_error(stringformat(
                "{} is not a subrule", for_step
            ));
            return;
        }

        for(int stepi = 0; stepi < subrule.num_steps(); ++stepi) {
            //auto substep = subrule.nth_step(stepi);
            production_rule::step &substep = subrule.nth_step_ref(stepi);
            if(!substep) {
                internal_error(stringformat(
                    "no substep for {}?\n", for_step
                ));
                return;
            }

            if(!substep.is_explicitly_named()) {
                if(!substep.is_ejected()) {
                    subrule.rename_parameter(stepi, substep.variable_name(), name);
                    substep.varname = name; // should this count as explicit?
                }

                if(substep.is_subexpression()) {
                    set_subex_names(substep, name);
                }
            }
        }
    }

    // subexpressions also add stuff to the containing rule...
    void add_subexpression(production_rule &rule, production_rule::step &sub) {
        if(sub.is_explicitly_named()) {
            // Propegate the explicit name down to the subs.
            // This is for stuff like (classic case):
            //   '('^ (item (','^ item)*)?:items ')'^ -> list;
            // In this case, +list will expect the "item"s in
            // the subexpression to be passed as one parameter
            // called "items".
            set_subex_names(sub, sub.varname);
        }
        
        if(sub.varname == "") {
            // this makes it so that if you have:
            //   (foo:bar)* -> bat;
            // ... the top level rule knows to look in
            // the subrule for "bar"
            // (this might be redundant with the subrules thing below, now)
            sub.varname = subex_varname(sub.gexpr.expr);
        }

        auto stepi = rule.add_step(sub, false);

        // if the subex hasn't been explicitly named,
        // we want to "bubble up" it's parameters:
        if(!sub.is_explicitly_named()) {
            auto subrule = single_rule_for_product(sub.gexpr.expr);

            if(!subrule) {
                // at present, subrules are always added before the parent
                // rule, so this shouldn't be able to happen.  but, future:
                internal_error(stringformat(
                    "No subrule found for {} in {}\n",
                    sub, rule
                ));
                return;
            }

            for(auto pname : subrule.parameter_names()) {
                rule.add_parameter(stepi, pname);
            }
        }
    }

    grammar_element parse_terminal() {
        std::string expr_str;
        grammar_element::Type type;
        switch(inp->peek()) {
            case '"':
                // within double quotes, c-like escape sequences
                // are supported:
                expr_str = convert_escapes(inp->parse_string());
                type     = grammar_element::Type::TERM_EXACT;
                break;
            case '\'':
                // Within single quotes, a backslash simply removes any
                // special meaning of the next char.  The only 2 chars
                // with special meaning are the end quote and backslashes.
                // So, '\\' means one backslash, '\'' means one single
                // quote, but '\n' means 2 chars: backslash and 'n'.
                expr_str = inp->parse_string();
                remove_single_quote_escapes(expr_str);
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
            default:
                // not a terminal:
                return grammar_element();
        }

        return grammar_element(expr_str, type);
    }

    production_rule::step parse_lack_of_separator_assertion() {
        if(inp->read_byte() == '~') {
            production_rule::step expr(
                "~", grammar_element::Type::LACK_OF_SEPARATOR
            );

            // lack-of-separator never gets passed to reduce actions - 
            // it's always a 0-length nothing
            expr.eject = true;

            return expr;
        }
        return production_rule::step();
    }

    int parse_expressions(production_rule &rule) {
        int num_read = 0;
        bool done = false;
        bool invert_next = false;
        do {
            eat_separator();
            production_rule::step expr;

            const utf8_byte inch = inp->peek();
            switch(inch) {
                case '\0':
                    done = true;
                    break; // EOF
                case '"':
                case '\'':
                case '/':
                case '&':
                    expr = production_rule::step(parse_terminal());
                    break;
                case '~':
                    expr = parse_lack_of_separator_assertion();
                    break;
                case '(':
                    expr = parse_subexpression();
                    break;
                case ')':
                    // end of a subexpression, or else a stray ')'
                    done = true;
                    break;
                case '!':
                    invert_next = !invert_next;
                    inp->read_byte();
                    break;
                case '-':
                    // end of rule steps (expressions), or else stray '-'
                    done = true;
                    break;
                case '`':
                    expr = parse_import_expression();
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
                    expr = production_rule::step(
                        inp->read_re("(:?<<?)|\\.|(:?>>?)")[0],
                        grammar_element::Type::NONTERM_PREC_PLACEHOLDER
                    );
                    break;
                default:
                    // should be the name of a production.
                    auto pname = read_production_name(inp);
                    if(!pname.length()) {
                        error(inp, stringformat(
                            "expected production name for rule '{}'\n"
                            " starting at {}",
                            rule.to_str(), rule.location()
                        ));
                    }
                    expr = production_rule::step(
                        pname, grammar_element::Type::NONTERM_PRODUCTION
                    );
                    break;
            }

            if(expr) {
                read_quantifier(inp, expr);
                read_suffix(inp, expr); // optional :name or ^
                if(expr.is_single())
                    read_quantifier(inp, expr);

                if(expr.type() == grammar_element::NONTERM_SUBEXPRESSION)
                    add_subexpression(rule, expr);
                else
                    rule.add_step(expr, invert_next);

                invert_next = false;
                num_read++;
            }
        } while(!(done || inp->eof()));

        return num_read;
    }

    code_block read_code() {
        // code is within "+{" "}+" brackets.
        // we don't know anything about the grammar of the code
        // within the brackets (presently), so you will derail
        // it if you put "}+" in a comment or string or whatever.
        // sorry.  try not to do that.
        eat_separator();

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

        return code_block(
            code_str, code_block::DEFAULT,
            inp->filename(), inp->line_number(start)
        );
    }

    code_block parse_regex_code_block() {
        code_block code;
        std::string regex;

        if(inp->peek() == '/') {
            int line = inp->line_number();
            regex = inp->parse_string();
            if(regex.length()) {
                code = code_block(
                    regex, code_block::REGEX, inp->filename(), line
                );
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
                eat_separator();
                inp->read_byte_equalling(',');
                eat_separator();

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
        eat_separator();
        if(!inp->read_exact_match("->")) {
            error("expected '->' before production name");
        }

        eat_separator();

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

        eat_separator();

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
            eat_separator();

            if(inp->read_byte_equalling(']')) {
                break;
            }
            
            size_t rew_pos = inp->current_position();
            std::string this_product;
            if(inp->peek() == '`') {
                this_product = parse_import();
            } else {
                this_product = read_production_name(inp);
            }
            eat_separator();
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
            ), false);
            std::string group_name = group_rule.product();

            eat_separator();


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
                ), false);
                rule.code(group_rule.code());
                rule.product(group_name);
                add_rule(rule);
            }
        }
    }

    void parse_fpl() {
        do {
            eat_separator();
            if(inp->peek() == '+') {
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
            if(out.count(wanted)) {
                continue;
            }

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
                         const std::string next = step.production_name();
                         if(!out.count(next)) {
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

    // Imports any missing custom scanners needed by the rule passed.
    // Custom scanners which already exist (either through having been
    // imported already, or from being defined previously in the importing
    // productions) are not imported.
    void import_scanners(productions *from, const production_rule &rule) {
        const int num_steps = rule.num_steps();
        for(int sti = 0; sti < num_steps; sti++) {
            auto scanner = rule.nth_step(sti).custom_scanner_name();
            if(scanner != "" && from->scanners[scanner]) {
                if(scanners.count(scanner) == 0) {
                    scanners[scanner] = from->scanners[scanner];
                }
            }
        }
    }

    // OK all cases:
    //   - `foo`.prod : importing specificlly one production
    //   - `foo`: SHOULD BE importing all goal productions.  If there's more than
    //      one, I don't think we can handle it here right now and should throw
    //      an error, OR make an intermediate rule 
    //   - @import, which always just imports productions as themselves.  2 cases:
    //     - @import 'grammar':  import each and every thing in the grammar file,
    //       regardless of if it's a goal or needed for a goal  or what
    //     - @import 'grammar'.prod:  import prod (and everything needed to produce it)
    //   - @grammar <full grammar> is basically saying we're implementing or
    //     specializing one particular grammar.  In this case, we should import
    //     the goals and make them our goals.
    //
    // How it (kinda) is:
    //   CASE                  WHICH RULES (+ depends)  ALSO
    //   @import 'foo'         all foo's rules
    //   @grammar 'foo'        foo's goals             separators, all supporting stuff
    //  `foo`                  foo's goals
    //   @import 'foo'.bar     rules for bar
    //  `foo`.bar              rules for bar
    //
    // But maybe we want:
    //   CASE                  WHICH RULES (+ depends)  ALSO
    //   @import 'foo'         all foo's rules
    //   @grammar 'foo'        all foo's rules          GOALS, separators, all supporting stuff
    //  `foo`                  all foo's rules          evaluate to goal or set of goals
    //   @import 'foo'.bar     rules for bar
    //  `foo`.bar              rules for bar
    //
    // which normalizses the cases..
    // @grammar could, in theory, support a grammar based on a subgrammar
    // too, if we normalize the syntax.
    // (alternately, always refer to subproducts with backticks?  it's ugly
    // syntax though).
    // maybe the key is to normalize the filenames so we don't have to
    // worry about '-' in them, and then they can just be symbols.  But
    // then, filenames are case-insensitive also (or might be).
    // Also it would be better if subgrammar elements were in the namespace
    // of that subgrammar.  But sometimes we want to import them into
    // our namespace (eg abnf-CORE)
    int import_rules_for_products(
        productions *from, const std::set<std::string> &wanted = {}
    ) {
        if(!from) return 0;

        // NOTE that we import the rules IN ORDER because
        // changing the rule order changes precedence.
        // (hence we don't do this via rules_for_product)
        std::set<std::string> found;
        for(auto rule : from->rules) {
            const std::string &prd = rule.product();
            // if the "wanted" set is empty, it means import
            // everything:
            if(!wanted.size() || (wanted.count(prd) > 0)) {
                found.insert(prd);
                if(from->exported_products.count(prd) == 0) {
                    import_scanners(from, rule);
                    add_rule(rule);
                } // else rules for this product are already imported
            }
        }

        // In order to prevent duplicate imports and import loops,
        // we mark the exported products.
        // This is a separate pass from the rules import because it's
        // by product and there's frequently more than one rule per
        // product.
        // Also, we mark the product exported regardless of if the rules
        // were successfully imported - if it failed once, there's
        // (presently) no reason it wouldn't fail again and we don't
        // want to keep trying.
        for(auto got : found) {
            from->exported_products.insert(got);
        }

        return found.size();
    }

    // Imports the grammar from the productions specified.
    // This will include rules and whatever's deemed necessary
    // to support those rules.
    // Returns the name of the production which this import will
    // produce, or an empty string if nothing was imported.
    // NOTE the grammar passed isn't const because we also use
    // it as a scratch pad to prevent redundant imports.
    void import_grammar(const std::string &grammar_name) {
        productions *fromp = subgrammar(grammar_name, inp);
        if(!fromp) {
            error(stringformat(
                "can't load subgrammar '{}'\n", grammar_name
            ));
            return;
        }
        productions &from = *fromp;

        for(auto sepc : from.separator_code) {
            add_separator_code(sepc);
        }

        for(auto primp : from.preamble) {
            add_preamble(primp);
        }

        int num_found = import_rules_for_products(&from);

        for(auto goal : from.goals) {
            goals.push_back(goal);
        }

        if(num_found <= 0) {
            warn(stringformat(
                "No rules imported from {}", from.inp->filename()
            ));
        }
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
                    why_no_match = stringformat(
                        "nothing produces {} for reducer {}",
                        pname, red
                    );
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
                        record_element(
                            rule.nth_step(stepi).gexpr,
                            rule.location()
                        );
                    }
                } // else it's not something we need to resolve
            }
        }
    }

    // returns true if the product passed has an auto-generated type
    bool has_generated_type(const std::string &for_prod) const {
        std::string ptype = type_for(for_prod);
        return generated_types.count(ptype) > 0;
    }

private:

    // 
    // Recursively attempt to fill in types for products
    // based on the types for the rules which produce those
    // products.
    // i.e., if we have:
    //    foo -> bat;
    //    bar -> bat;
    // and foo and bar both have the same type, bat can also
    // have that type.
    // 
    std::string inherit_type(const std::string &prod) {
        auto existing_type = type_for(prod);
        if(existing_type != "")
            return existing_type;

        // Use an empty type in the "type for" list as a sort
        // of flag to prevent infinite recursion in cases where
        // you have something like:
        //       a -> b; b -> a;
        // ('a' would explicitly have the "unknown" type "",
        // which we then return when this function gets called
        // again).
        // We clear these empty types in resolve_inherited_types()
        auto tf = type_for_product.find(prod);
        if(tf != type_for_product.end()) {
            return "";
        }
        type_for_product[prod] = "";

        // and here's where we recurse:
        //  if all rules creating this product produce the same
        //  type, we can use that type for this product:
        auto strl  = rules_for_product.lower_bound(prod);
        auto endrl = rules_for_product.upper_bound(prod);
        std::string inherited;
        std::set<std::string> aliases; // debug info
        for(auto rit = strl; rit != endrl; ++rit) {
            auto rule = rules[rit->second];
            const std::string alias = rule.potential_type_alias();
            if(alias == "") {
                if(opts.debug_types) {
                    std::cerr << stringformat(
                        "  {} has no potential type aliases\n", prod
                    );
                }
            } else {
                auto itype = inherit_type(alias);
                if((inherited != "") && (itype != inherited)) {
                    // differing inherited types, so can't inherit
                    if(opts.debug_types) {
                        std::cerr << stringformat(
                            "  {} can't inherit type from {} ({} != {})\n",
                            prod, alias, inherited, itype
                        );
                    }
                    return "";
                }
                aliases.insert(alias);
                inherited = itype;
            }
        }
        if(inherited != "") {
            add_type_for(
                prod, inherited,
                stringformat("inheritance from {}", aliases)
            );
        }

        return type_for(prod);
    }

    // This inheritance goes in the opposite direction -
    // Following the prior example,
    //    foo -> bat;
    //    bar -> bat;
    // if bat has a known type -and- neither foo nor bar have
    // known types, foo and bar are given bat's type.
    // I'm not sure if this is the "reverse" or the normal,
    // and I actually wonder a bit if the "normal" could be
    // replaced by always generating types.
    // (Or is this "pass type down"?)
    void reverse_inherit_type(const std::string &prod) {
        auto known_type = type_for(prod);
        if(known_type != "") {
            auto strl  = rules_for_product.lower_bound(prod);
            auto endrl = rules_for_product.upper_bound(prod);
            for(auto rit = strl; rit != endrl; ++rit) {
                auto rule = rules[rit->second];
                if(rule.is_potential_type_alias()) {
                    auto subp = rule.parameter_step(0).gexpr;
                    if(type_for(subp) == "") {
                        if(!subp.is_nonterminal()) {
                            // so afaik we can't get here.
                            // I'm not sure what happens if we do, though,
                            // so I want to at least flag it:
                            warn(stringformat(
                                "{} in {} is a nonterminal but "
                                "we're passing down a type for it",
                                subp, rule
                            ));
                        }
                        add_type_for(
                            subp.expr, known_type,
                            stringformat("passed down from {}", prod)
                        );
                        reverse_inherit_type(subp.expr);
                    }
                }
            }
        }
    }

    void reverse_inherit_types() {
        auto rit = rules_for_product.begin();
        auto end = rules_for_product.end();
        while(rit != end) {
            const std::string &prodn = rit->first;
            reverse_inherit_type(prodn);
            // ("upper bound" is actually "lower bound of next")
            rit = rules_for_product.upper_bound(prodn);
        }
    }

public:

    void resolve_inherited_types() {

        reverse_inherit_types();

        const auto end = rules_for_product.end();
        auto rit = rules_for_product.begin();
        while(rit != end) {
            const std::string &prodn = rit->first;
            inherit_type(prodn);
            // ("upper bound" is actually "lower bound of next")
            rit = rules_for_product.upper_bound(prodn);
        }

        // clear all the temporary empty-string types which we might
        // have used for markers:
        auto tfp = type_for_product.begin();
        while(tfp != type_for_product.end()) {
            if(tfp->second != "") {
                tfp++;
            } else {
                auto cur = tfp++;
                type_for_product.erase(cur);
            }
        }

        return;
    }

    // Warns if any reduce parameters would or could have more than
    // one type.  This can happen due to parameter melding - say you
    // have:
    //    foo:a bar:a -> bat;
    // +bat() will then get one parameter (a) for both the foo and the
    // bar, but if foo and bar have different types that's likely to lead
    // to surprises.
    void check_reduce_parameter_types() {
        for(auto rule : rules) {
            std::map<std::string, std::string> t_for_v;
            for(auto step : rule.steps()) {
                if(!step.skip_on_reduce()) {
                    auto vname = step.variable_name();
                    auto tname = type_for(step.gexpr);
                    if(t_for_v.count(vname)) {
                        if(t_for_v[vname] != tname) {
                            warn(stringformat( 
                                "type conflict at {} on {} in {}rule {}: "
                                "{} vs {}",
                                 rule.location(), vname,
                                 rule.is_subexpression()?"sub":"", rule,
                                 t_for_v[vname], tname
                            ));
                        }
                    } else {
                        t_for_v[vname] = tname;
                    }
                }
            }
        }
    }

    // generates/fills in any missing types
    void resolve_types() {
        if(generate_types) {
            std::set<generated_type> gt_candidates;
            for(auto prr : rules_for_product) {
                const std::string &prodn = prr.first;
                if(type_for(prodn) == "") {
                    generated_type gt(prodn, *this);
                    gt_candidates.insert(gt);
                    add_type_for(prodn, gt.name(), "generated");
                }
            }
            for(auto candidate : gt_candidates) {
                candidate.resolve_attribute_types(*this);
                generated_types.insert(candidate);
            }
        } else if(has_default_type()) {
            for(auto prr : rules_for_product) {
                const std::string &prodn = prr.first;
                if(type_for(prodn) == "") {
                    add_type_for(prodn, default_type(), "default type");
                }
            }
        } else {
            resolve_inherited_types();
        }

        resolve_output_and_goal_types();

        // anything still without a type defaults to the output type:
        for(auto prr : rules_for_product) {
            const std::string &prodn = prr.first;
            if(type_for(prodn) == "") {
                add_type_for(prodn, output_type(), "default to output type");
            }
        }

        check_reduce_parameter_types();
    }

    // Mark the element passed as "masking" the other element passed.
    // In this context, if an element is "masked" by another element,
    // it means that the existence of the element would potentially
    // preclude the ability of the generated parser to match the other
    // element.
    // Masking is not inherently bad.  An example of where it's used:
    //   x '+'  y -> addition;
    //   x '+=' y -> add_assign; # or whatever
    // In this case, if the input has 'foo += 23', we don't want the
    // addition expression eating the '+'.  We want it to match the
    // longer '+=' so we record the fact that '+' might mask '+='
    // and use that fact to disambiguate later.
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

    // Formerly, this matched custom scanner steps to their
    // implementations.  We no longer do that here, so at
    // the moment, all it does is report if a custom scanner
    // is undefined.
    void resolve_custom_step(production_rule &rule, int stepi) {
        auto step = rule.nth_step(stepi);
        std::string scanner_name = step.gexpr.expr;
        auto found = scanners.find(scanner_name);

        if(found == scanners.end()) {
            // it is possible that the rule isn't
            // used, so the scanner won't be used, but
            // we're going to complain about it here
            // anyway because the code generator shouldn't
            // have to be bothered checking if a given
            // element is actually used.
            warn(stringformat(
                "{} use of undefined scanner &{} in rule {}",
                rule.location(), scanner_name, rule
            ));
        } else if(found->second.is_stub) {
            warn(stringformat(
                "{}: &{} ({}) is unimplemented",
                rule.location(), scanner_name, found->second.location()
            ));
        }
    }

    // figures out what to do with weird cases like custom or inverted steps.
    // this may change steps within rules or even create or delete rules.
    void resolve_steps() {
        for(int rulei = 0; rulei < rules.size(); ++rulei) {
            production_rule &rule = rules[rulei];
            for(int stepi = 0; stepi < rule.num_steps(); stepi++) {
                auto step = rule.nth_step(stepi);
                if(step.type() == grammar_element::TERM_CUSTOM) {
                    resolve_custom_step(rule, stepi);
                }
            }

            // .. and now, brute-force ensure that all elements
            // are recorded, because elements may have changed
            // due to TERM_CUSTOM or match-inversion or whatever
            // else:
            for(int stepi = 0; stepi < rule.num_steps(); stepi++) {
                record_element(rule.nth_step(stepi).gexpr, rule.location());
            }
        }
    }

    /*
        Returns the number of parameter stack entries to the
        next parameter stack entry melded into the named parameter.
        Note:  parameter stack entries, -not- lr_stack entries.
     */
    stack_distance meld_distance(
        const rulestep &from_step,
        const std::string &pname
    ) {
        stack_distance dist(0);
      
        // false = don't include the starting step; that start step is
        // the one from which we're setting distance.
        //auto all_paths = from_step.paths(false);
        auto all_paths = from_step.paths();

        dprint_melds(stringformat(
            "getting meld distance for '{}' from {}."
            "There are {} paths:\n    {}\n",
            pname, from_step, all_paths.size(), join(all_paths, "\n    ")
        ));

        for(auto path : all_paths) {
            dprint_melds(stringformat("   walking path {}\n", path));
            stack_distance this_dist(0);
            for(auto rstep : path) {
                if(rstep.step().is_ejected())
                    continue; // because ejected = not on param stack

                if(rstep.step().is_subexpression())
                    continue;  // because we'll count the steps -within- it

                dprint_melds(stringformat("     looking at {}?\n", rstep));

                if(rstep.step().variable_name() == pname) {
                    dist = this_dist;
                    dprint_melds(stringformat(
                        "       bing! on {} distance {} gives dist {}\n",
                        rstep, this_dist, dist
                    ));
                    this_dist.reset();
                }

                if(dist.is_indeterminate())
                    break;

                // (everything here is one ls stack entry, since we
                // iterate into subexes)
                ++this_dist;
            }
            if(dist.is_indeterminate())
                break;
        }
        dprint_melds(stringformat(
            "   final distance for {}: {}\n", pname, dist
        ));
        return dist;
    }

    static production_rule &no_rule() {
        static production_rule nr;
        if(nr) {
            warn("something modified the rule from no_rule()");
            nr = production_rule();
        }
        return nr;
    }

    rulestep canonical_step_for_param(
        int rulenum, const std::string &pname
    ) const {
        if((unsigned)rulenum >= rules.size()) {
            warn(stringformat(
                "rule {} is out of bounds in canonical_step_for_param",
                rulenum
            ));
            return rulestep();
        }
        auto rule = rules[rulenum];
        int canonical_stepi = rule.parameter_step_number(pname);
        auto step = rule.nth_step(canonical_stepi);
        if(!step && rule.is_subexpression()) {
            // There's no explicitly named step for pname in this
            // rule, but we're a subexpression, so our parent rule
            // must think we have this parameter - otherwise we
            // wouldn't be here.
            // Some examples of where this would happen:
            //   ('x'):foo bar -> bat;
            //   '{'^ (member (','^ member)*)?:members '}'^ -> object ;
            // So how do we know which step is the canonical?
            // It must be:
            //    - not ejected
            //    - the only not-ejected item
            // What happens if the second case is written:
            //   '{'^ string:key ':'^ value (','^ string:key ':'^ value)*)?:members '}'^ -> object ;
            // .. then it's ok.  the subs have parameter count > 1.
            // Do we care about the names or types of subs?
            // Say it's:
            //   '{'^ (member (','^ other)*)?:members '}'^ -> object ;
            // ... I think we don't care _in this context_. In this context,
            // we'll act as if that was written:
            //   '{'^ (member:members (','^ other:members)*)? '}'^ -> object ;
            // .. and let other stuff decide if member and other have
            // different types or whatever.
            if(rule.parameter_count() == 1) {
                canonical_stepi = rule.parameter_step(0);
                step = rule.nth_step(canonical_stepi);
            }
        }
 
        if(step.is_subexpression()) {
            return canonical_step_for_param(subrulenum_for_step(step), pname);
        }

        return rulestep(this, rulenum, canonical_stepi);
    }

    // Sets the meld value for the (assumed canonical) rulestep
    // passed.  Throws errors (or warnings) if melds are inconsistent,
    // or if there appear to be other problems.
    void set_meld(rulestep rstep, const std::string &pname, stack_distance meld, src_location ca = CALLER()) {
        dprint_melds(stringformat(
            "{} SETTING MELD TO {} for {} in {}\n",
            ca, meld, pname, rstep.rule()
        ));

        production_rule::step &step = rules[rstep.rulenum].nth_step_ref(
            rstep.stepnum
        );

        if(step.variable_name() != pname) {
            internal_error(stringformat(
                "{} {} setting '{}' meld {} for wrong step {}",
                ca, rstep.rule(), pname, meld, step
            ));
            return;
        }

        if(step.is_subexpression()) {
            // this is probably an internal error as well, but it's
            // not a total disaster, so warn and try anyway:
            warn(stringformat(
                "setting meld {} on a subexression? {} in {}\n",
                meld, pname, rstep.rule()
            ));
        }

        auto old_meld = step.meld_distance; // (error messages/debug)
        step.meld_distance = meld;
        dprint_melds(stringformat(
            "  .. now that it's set that step is {}\n", step
        ));

        if(step.meld_distance.is_indeterminate()) {
            // if we can't meld, we do have to toss an actual error - 
            // generated code isn't going to work right.
            error(rstep.rule().location(), stringformat(
                "inconsistent meld for {} ({} vs {}) in {}\n",
                pname, old_meld, meld, rstep.rule()
            ));
        }
    }

    void resolve_melds_for_rule(
        unsigned rulenum,
        bool multiple_subex = false,
        int rec_depth = 0
    ) {
        if(rulenum >= rules.size()) {
            internal_error(stringformat(
                "rule {} is out of bounds in resolve_melds_for_rule",
                rulenum
            ));
            return;
        }

        production_rule &rule = rules[rulenum];
        for(auto name : rule.parameter_names()) {
            auto canonical_step = canonical_step_for_param(rulenum, name);
           
            if(!canonical_step) {
                internal_error(stringformat(
                    "No canonical step for {} in {} when trying to set meld",
                    name, rule
                ));
            } else {
                set_meld(
                    canonical_step, name,
                    meld_distance(canonical_step, name)
                );
            }
        }
    }

    // Rules can have more than one step corresponding to a
    // particular parameter.  For example, the reducer for:
    //    arg (','^ arg)* -> arg_list;
    // .. should get one argument "arg", which should contain
    // all the (comma separated) arguments.  But, this requires
    // some precalculation, and not all combinations are valid,
    // so we need to detect invalid ones.
    void resolve_melds() {
        for(int rulei = 0; rulei < rules.size(); ++rulei) {
            production_rule &rule = rules[rulei];
            // only do this for top level rules - top level rules
            // will recursively resolve subexpressions.
            // this is (in part) because the final meld distances
            // are all in the "frame of reference" of the top
            // level rule, and may span subexpressions.
            if(!rule.is_subexpression()) {
                dprint_melds(stringformat(
                    "MELDS FOR TOP RULE {} «{}» {}\n",
                    rulei, rule, rule.location()
                ));
                resolve_melds_for_rule(rulei);
            }
        }
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
    // I believe other parser generators (yacc etc) resolve this with the
    // concept of word boundaries.  But, I want fpl to be able to work
    // without that concept.  Anyway, in principle this should be more
    // efficient for the generated parser since it only needs to look
    // for a boundary if it actually matters for the particular terminal,
    // and then we'd only have the check the particular suffixes which matter.
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
        // 1) if there are any precedence groups, the goal is
        //    whatever the first (= lowest precedence) group
        //    produces:
        if(precedence_group_names.size())
            return precedence_group_names[0];

        // 2) whatever the first top-level rule produces.
        //    "top level" means not a subrule.  If there are
        //    no precedence groups, this is the first rule
        //    specified by the fpl author.
        for(auto rule : rules) {
            if(!rule.is_subexpression()) {
                return rule.product();
            }
        }

        // 3) a production named "goal"
        return "goal";
    }

    bool is_goal(const std::string &prod) const {
        if(goals.size() == 0)
            return prod == default_goal();

        // Linear search of goals here.  I'm thinking this is
        // ok because normally there's only one goal, and
        // one might expect (say) 3 at most.  I think you'd
        // have to have a fairly complex grammar for this
        // to be toooo terrible (though it could end up NxN
        // if the callers are iterating all products, and
        // all products are goals or something.. anyway,
        // moving on)
        for(auto goal : goals) {
            if(goal == prod)
                return true;
        }

        return false;
    }


    // This checks for and tries to resolve inconsistencies in goal types.
    // It only really matters if there's more than one goal.
    void resolve_output_and_goal_types() {
        // types_goals keys are the types specified in @produces 
        // (or similar) and the known types of our goal productions.
        // The values are a string specifying something which produces
        // the key type and which can be used to report a reasonable
        // error if there's a conflict.
        // The idea is that if we end up with exactly one entry
        // in types_goals, that entry's key will be the type the
        // parser returns, but if not, we can report the conflict
        // using the values.
        std::map<std::string, std::string> types_goals;
        if(output_type().length())
            types_goals[output_type()] = "(output type/@produces)";
        for(auto goal: goals) {
            if(type_for(goal).length())
                types_goals[type_for(goal)] = goal;
        }

        if(types_goals.size() == 1) {
            // There's exactly one specified/inferred type among
            // all possible goals, so they can all agree.  Make
            // them agree:
            set_output_type(types_goals.begin()->first);
            for(auto goal: goals) {
                // (the type is the ->first element of the only item)
                add_type_for(goal, types_goals.begin()->first, "goal type");
            }
        } else if(types_goals.size() == 0 ) {
            if(goals.size() == 0) {
                internal_error("no goals found!");
            } else {
                error(stringformat(
                    "can't infer type for goal. "
                    "try setting @produces or @type_for {}", 
                    *goals.begin()
                ));
            }
        } else {
            std::string conflicts;
            for(auto gft : types_goals) {
                conflicts += stringformat(
                    "\n    goal '{}' produces {}", gft.second, gft.first
                );
            }
            error(stringformat("conflicting output types: {}\n", conflicts));
        }
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
                ), false);
                add_rule(goal_rule);
                if(output_type() != "") {
                    add_type_for("_fpl_goal", output_type());
                }
            }
        }
    }

    // determines goal(s), generates states, matches up reducers, 
    // checks for/reports structural errors, etc.
    // call this before generating code.
    void resolve(src_location caller = CALLER()) {
        if(rules.size() <= 0) {
            error("No rules found\n");
        }

        resolve_terminal_masking();
        resolve_steps();
        resolve_melds();
        resolve_goals();

        resolve_types();
        apply_reducers();
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
    friend std::string fpl_x_parser_generated_types(const productions &);
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


    // debugging/messaging:

    std::string stepstring(const production_rule::step &st) const {
        if(auto subr = sub_rule(st)) {
            return "(" + rulestring(subr) + ")" + st.qty.to_str();
        }
        return st.to_str();
    }

    std::string rulestring(const production_rule &rl) const {
        std::string out;
        for(int stepi = 0; stepi < rl.num_steps(); ++stepi) {
            out += stepstring(rl.nth_step(stepi));
            if(stepi < rl.num_steps() - 1) {
                out += " ";
            }
        }

        if(!rl.is_subexpression()) {
           out += " -> ";
           out += rl.product();
        }

        return out;
    }

    void dprint_melds(const std::string &msg) {
        if(opts.debug_melds) {
            std::cerr << msg;
        }
    }

    void dpprint_melds(const std::string &msg) {
        if(opts.debug_melds) {
            dprint_melds(msg);
            getchar();
        }
    }

    std::string states_to_string() const {
        std::string out;

        // print the actual state transitions:
        for(int stind = 0; stind < states.size(); stind++) {
            out += stringformat("state {}:\n", stind);
            auto st_trans = states.at(stind).transitions(*this);
            for(auto trans : st_trans) {
                out += stringformat("    {}\n", trans);
            }
            out += "\n";
        }
        out += "\n\n\n";

        // now print the old way as well:
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
                    // errno... sigh
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
                    // element is actually used.
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
                // ... should we remove the rule so we don't
                // generate code for it?  or somehow prevent
                // generating code?
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
#include "regex_custom_scanner.h"
#include "scan_group_terminal.h"
#include "scan_inv_group_terminal.h"

} // namespace fpl

#endif // PRODUCTIONS_H
