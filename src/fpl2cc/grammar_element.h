#ifndef GRAMMAR_ELEMENT_H
#define GRAMMAR_ELEMENT_H

#include "util/c_str_escape.h"
#include "util/jerror.h"
#include "util/src_location.h"

namespace fpl {

struct grammar_element {
    std::string expr; // either a string, regex, or name of product
    typedef enum {
        // order here matters:  earlier types are matched first
        // in parsing
        NONE = 0,
        NONTERM_PRODUCTION,       // the result of reducing a rule
        NONTERM_SUBEXPRESSION,    // parenthesized expression
        NONTERM_PREC_PLACEHOLDER, // precedence placeholder
        TERM_CUSTOM,       // custom code for matching terminal
        TERM_CUSTOM_INV,   // match anything -but- this custom
        LACK_OF_SEPARATOR, // pseudoterminal indicating no separator
        TERM_ASSERTION,    // match if condition pertains
        TERM_ASSERTION_INV,// match if condition does not pertain
        TERM_EXACT,        // exact string match
        TERM_EXACT_INV,    // match anything -but- this exact string
        TERM_REGEX,        // regular expression match
        TERM_REGEX_INV,    // match anything -but- this regex
        // at the moment, these are custom terminals only:
        TERM_GROUP,        // match anything in the group of terminals
        TERM_GROUP_INV,    // match up to anything in group of terminals
        TERM_COMPOUND,     // match only if each terminal within matches
        TERM_COMPOUND_INV, // match anything -but- this compound
        // end custom terminals only
        END_OF_PARSE,      // indicates we're done parsing (good or bad)
        _TYPE_CAP
    } Type;
    Type type;

    static const char *Type_to_str(Type t) {
        static const char *strs[] = {
            "NONE",
            "NONTERM_PRODUCTION",
            "NONTERM_SUBEXPRESSION",
            "NONTERM_PREC_PLACEHOLDER",
            "TERM_CUSTOM",
            "TERM_CUSTOM_INV",
            "LACK_OF_SEPARATOR", // (special case of TERM_ASSERTION now)
            "TERM_ASSERTION",
            "TERM_ASSERTION_INV",
            "TERM_EXACT",
            "TERM_EXACT_INV",
            "TERM_REGEX",
            "TERM_REGEX_INV",
            "TERM_GROUP",
            "TERM_GROUP_INV",
            "TERM_COMPOUND",
            "TERM_COMPOUND_INV",
            "END_OF_PARSE"
        };
        if(t >= NONE && t < _TYPE_CAP) {
            return strs[t];
        }
        return "invalid grammar_element::Type";
    }

    // like the above, but suitable for error messages:
    static const char *Type_to_human_str(Type t) {
        static const char *strs[] = {
            "<none!>",
            "nonterminal",
            "subexpression",
            "<precedence placeholder>",
            "custom terminal",
            "inverted custom terminal",
            "lack-of-separator assertion",
            "custom assertion",
            "inverse of custom assertion",
            "exact match",
            "inverted exact match",
            "regular expression match",
            "inverted regular expression match",
            "scan any element in group",
            "scan anything not matching the group",
            "scan if all elements match in order",
            "scan until all elements match in order",
            "end of parse"
        };
        if(t >= NONE && t < _TYPE_CAP) {
            return strs[t];
        }
        return "<invalid grammar element type!>";
    }

    grammar_element() : expr(""), type(Type::NONE) {
    }

    grammar_element(const std::string &str, Type tp, src_location ca = CALLER())
        : expr(str), type(tp) {
    }

    // returns a negative, 0, or positive value depending on
    // if this element can be considered <, ==, or > than the
    // other element:
    inline int compare(const grammar_element &other) const {
        int cmp = type - other.type;
        if(cmp == 0) {
            // to avoid termianl masking, we reverse-lexical compare
            // so that if there's a term that's a prefix of another,
            // the prefix one comes later and thereby doesn't match
            // first and prevent the the longer one from matching.
            cmp = -expr.compare(other.expr);
        }
        return cmp;
    }

    friend bool operator<(const grammar_element& left, const grammar_element& right) {
        return left.compare(right) < 0;
    }

    friend bool operator==(const grammar_element& left, const grammar_element& right) {
        return left.compare(right) == 0;
    }

    operator bool() const {
        // Type "NONE" counts as false.
        // Any invalid type (>= _TYPE_CAP) is also false.
        return (type > NONE && type < _TYPE_CAP);
    }

    inline bool is_nonterminal() const {
        return (type > NONE) && (type < TERM_CUSTOM);
    }

    inline bool is_placeholder() const {
        return (type == NONTERM_PREC_PLACEHOLDER);
    }

    inline bool is_end_of_parse() const {
        return (type == END_OF_PARSE);
    }

    std::string nonterm_id_str() const {
        if(is_nonterminal()) {
            // always prefix with underscore as a hack to avoid
            // colliding with target language keywords:
            std::string out = "_" + expr;
            return out;
        }

        // the ID passed isn't a nonterminal.
        // returning a string like this should
        // at least give something to grep for:
        return "error: " + expr + " is not a nonterminal";
    }

    inline std::string type_str() const {
        return Type_to_str(type);
    }

    // Returns the element type to use when the match-sense of the
    // given type is inverted (eg !"foo" to mean match anything but
    // "foo"), or grammar_element::NONE if there's no appropriate
    // inverse.
    static grammar_element::Type inverse_type(grammar_element::Type type) {
        switch(type) {
            // the 2 basic terminal types have built in inverses:
            case TERM_EXACT:     return TERM_EXACT_INV;
            case TERM_REGEX:     return TERM_REGEX_INV;

            // assuming a given custom terminal has implemented
            // an inverse implemented someplace, it too can
            // have an inverse:
            case TERM_CUSTOM:    return TERM_CUSTOM_INV;

            // (and, of course, you can invert back)
            case TERM_EXACT_INV:  return TERM_EXACT;
            case TERM_REGEX_INV:  return TERM_REGEX;
            case TERM_CUSTOM_INV: return TERM_CUSTOM;

            // assertions (being boolean functions) are trivially
            // invertible:
            case TERM_ASSERTION:     return TERM_ASSERTION_INV;
            case TERM_ASSERTION_INV: return TERM_ASSERTION;

            // I'm not sure how to support nonterms, or if it's
            // even possible, because there may be any number
            // of other things matched within the nonterm.
            // Inverting the lack-of-separator assertion is the
            // same as just leaving it out of the rule, so I
            // we don't need the ability to invert it.
            // So, at present, nothing else has an inverse
            // type.
            default: return NONE; // (no inverse)
        }
    }

    // .. And this tries to change the element type to whatever
    // the appropriate inverse is.
    // Returns true on success, false otherwise.
    bool invert_type() {
        auto new_type = inverse_type(type);
        if(new_type) {
            type = new_type;
            return true;
        }
        return false;
    }

    // ... And this returns a grammar element of the inverse type
    grammar_element inverse() const {
        return grammar_element(expr, inverse_type(type));
    }

    void resolve_placeholder(
        const std::string prod_name, src_location caller = CALLER()
    ) {
        if(!is_placeholder()) {
            jerror::warning(stringformat(
                "resolving non-placeholder expression ({}) at {}",
                *this, caller
            ));
        }
        expr = prod_name;
        type = NONTERM_PRODUCTION;
    }

    inline std::string to_str() const {
        const char *lb = "";
        const char *rb = "";
        switch(type) {
            case TERM_EXACT:
                lb = "'";
                rb = "'";
                break;
            case TERM_EXACT_INV:
                lb = "!'";
                rb = "'";
                break;
            case TERM_REGEX:
                lb = "/";
                rb = "/";
                break;
            case TERM_REGEX_INV:
                lb = "!/";
                rb = "/";
                break;
            case TERM_CUSTOM:
                lb = "&";
                rb = "";
                break;
            case TERM_CUSTOM_INV:
                lb = "!&";
                rb = "";
                break;
            case LACK_OF_SEPARATOR:
                // phase out - this is an assertion
                lb = "⸢";
                rb = "⸣";
                break;
            case TERM_ASSERTION:
                lb = "⸢";
                rb = "⸣";
                break;
            case TERM_ASSERTION_INV:
                lb = "!⸢";
                rb = "⸣";
                break;
            case NONTERM_PRODUCTION:
            case NONTERM_PREC_PLACEHOLDER:
                lb = "";
                rb = "";
                break;
            case TERM_GROUP:
                lb = "[";
                rb = "]";
                break;
            case TERM_GROUP_INV:
                lb = "![";
                rb = "]";
                break;
            case TERM_COMPOUND:
                lb = "(";
                rb = ")";
                break;
            case TERM_COMPOUND_INV:
                lb = "!(";
                rb = ")";
                break;
            case NONTERM_SUBEXPRESSION:
                lb = "(";
                rb = ")";
                break;
            case END_OF_PARSE:
                lb = "␄";
                rb = "";
                break;
            default:
                lb = "??????";
                rb = "??????";
                break;
        };

        std::string out;
        out += lb;
        out += c_str_escape(expr);
        out += rb;

        return out;
    }
};

inline std::string to_string(grammar_element::Type t) {
    return grammar_element::Type_to_str(t);
}

} // namespace fpl

#endif // GRAMMAR_ELEMENT_H
