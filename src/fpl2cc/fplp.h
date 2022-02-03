#ifndef FPLP_H
#define FPLP_H

#include <list>
#include <string>
#include <variant>

// what are the possible elements?
//  - directives (w/ arguments, which I guess can always be strings)
//  - code blocks (which are strings)
//  - imports (which are interesting and might want revisiting anyway)
//  - rules, which contain:
//    - the thing being generated
//    - steps, which are at least a string representing what to match,
//      the type of the thing to match, and a min/match count
//    - optionally the code for the rule

// @directive
struct fplp_directive {
    std::string name;
    std::string arg;
};

// +{ }+ code block
struct fplp_code_block {
    std::string code;
    fplp_code_block(const std::string &in) : code(in) { }
};

// `xxx.fpl` import
struct fplp_imp {
    // XXX there are actually other features supported in fpl 1.0
    // revisit.
    std::string filename;
};

// *, +, ?
struct fplp_quantifier {
    int min;
    int max;
};

// any regex, exact, or production match (with quantifiers)
struct fplp_rule_step {
    char            type; // ', ", /, or space for a production name
    std::string     match;
    fplp_quantifier quant;
};

// production rule
struct fplp_rule {
    std::string               product;
    std::list<fplp_rule_step> steps;
    fplp_code_block           reduce_code;
};

typedef std::variant<
    std::string,
    fplp_directive,
    fplp_code_block,
    fplp_imp,
    fplp_quantifier,
    fplp_rule_step,
    fplp_rule
> fplp_element;

std::string to_string(const fplp_element &el) {
    return "fplp_element...";
}

#endif // FPLP_H

