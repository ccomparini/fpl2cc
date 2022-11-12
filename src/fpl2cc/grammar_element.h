#ifndef GRAMMAR_ELEMENT_H
#define GRAMMAR_ELEMENT_H

namespace fpl {

struct grammar_element {
    std::string expr; // either a string, regex, or name of product
    typedef enum {
        NONE,
        TERM_EXACT,  // exact string match
        TERM_REGEX,  // regular expression match
        TERM_CUSTOM, // custom code for matching terminal
        LACK_OF_SEPARATOR, // pseudoterminal indicating no separator
        NONTERM_PRODUCTION,    // the result of reducing a rule
        NONTERM_SUBEXPRESSION, // parenthesized expression
        _TYPE_CAP
    } Type;
    Type type;

    static const char *Type_to_str(Type t) {
        static const char *strs[] = {
            "NONE",
            "TERM_EXACT",
            "TERM_REGEX",
            "TERM_CUSTOM",
            "LACK_OF_SEPARATOR", // convert to TERM_CUSTOM?
            "NONTERM_PRODUCTION",
            "NONTERM_SUBEXPRESSION",
        };
        if(t >= NONE && t < _TYPE_CAP) {
            return strs[t];
        }
        return "invalid grammar_element::Type";
    }

    grammar_element() : expr(""), type(Type::NONE) { }

    grammar_element(const std::string &str, Type tp)
        : expr(str), type(tp) { }

    // returns a negative, 0, or positive value depending on
    // if this element can be considered <, ==, or > than the
    // other element:
    inline int compare(const grammar_element &other) const {
        int cmp = type - other.type;
        if(cmp == 0) {
            cmp = expr.compare(other.expr);
        }
        return cmp;
    }

    friend bool operator<(const grammar_element& left, const grammar_element& right) {
        return left.compare(right) < 0;
    }

    friend bool operator==(const grammar_element& left, const grammar_element& right) {
        return left.compare(right) == 0;
    }


    inline bool is_nonterminal() const {
        return (type >= NONTERM_PRODUCTION);
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
            case TERM_CUSTOM:
                lb = "&";
                rb = "";
                break;
            case LACK_OF_SEPARATOR:
                lb = "⸢";
                rb = "⸣";
                break;
            case NONTERM_PRODUCTION:
                lb = "";
                rb = "";
                break;
            case NONTERM_SUBEXPRESSION:
                lb = "(";
                rb = ")";
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

std::string to_string(grammar_element::Type t) {
    return grammar_element::Type_to_str(t);
}

} // namespace fpl

#endif // GRAMMAR_ELEMENT_H
