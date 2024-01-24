#ifndef FPLP_H
#define FPLP_H

#include <list>
#include <string>
#include <variant>

// XXX this header is deprecated

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

    std::string to_str() const {
        return "@" + name + " " + arg;
    }
};

// +{ }+ code block
struct fplp_code_block {
    std::string code;
    fplp_code_block(const std::string &in) : code(in) { }

    std::string to_str() const {
        return "(code block)";
    }
};

// `xxx.fpl` import
struct fplp_imp {
    // XXX there are actually other features supported in fpl 1.0.
    // revisit.
    std::string filename;
    std::string to_str() const { return "`" + filename + "`"; }
};

// *, +, ?
struct fplp_quantifier {
    int min;
    int max;
    std::string to_str() const {
        return "{" + std::to_string(min) + "," + std::to_string(max) + "}";
    }
};

// any regex, exact, or production match (with quantifiers)
struct fplp_rule_step {
    char            type; // ', ", /, or space for a production name
    std::string     match;
    fplp_quantifier quant;

    std::string to_str() const {
        if(type != ' ')
            return type + match + type + quant.to_str();
        else
            return match + quant.to_str();
    }
};

// production rule
struct fplp_rule {
    std::string               product;
    std::list<fplp_rule_step> steps;
    fplp_code_block           reduce_code;

    std::string to_str() const {
        std::string out;
        for(auto st : steps) {
            out += st.to_str() + " ";
        }
        out += "-> " + product;
        return out;
    }
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
    if(std::holds_alternative<std::string>(el))
        return std::get<std::string>(el);

    if(std::holds_alternative<fplp_directive>(el))
        return std::get<fplp_directive>(el).to_str();
    if(std::holds_alternative<fplp_code_block>(el))
        return std::get<fplp_code_block>(el).to_str();
    if(std::holds_alternative<fplp_imp>(el))
        return std::get<fplp_imp>(el).to_str();
    if(std::holds_alternative<fplp_quantifier>(el))
        return std::get<fplp_quantifier>(el).to_str();
    if(std::holds_alternative<fplp_rule_step>(el))
        return std::get<fplp_rule_step>(el).to_str();
    if(std::holds_alternative<fplp_rule>(el))
        return std::get<fplp_rule>(el).to_str();

    return "invalid fplp_element";
}

#endif // FPLP_H


