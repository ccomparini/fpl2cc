#ifndef GRAMMAR_ELEMENT_H
#define GRAMMAR_ELEMENT_H

namespace fpl {

struct grammar_element {
    std::string expr; // either a string, regex, or name of product
    typedef enum {
        NONE,
        TERM_EXACT,
        TERM_REGEX,
        NONTERM_PRODUCTION,
        LACK_OF_SEPARATOR, // pseudoterminal indicating no separator
        _TYPE_CAP
    } Type;
    Type type;

    static const char *Type_to_str(Type t) {
        static const char *strs[] = {
            "NONE",
            "TERM_EXACT",
            "TERM_REGEX",
            "NONTERM_PRODUCTION",
            "LACK_OF_SEPARATOR",
        };
        if(t > NONE && t < _TYPE_CAP) {
            return strs[t];
        }
        return "invalid grammar_element::Type";
    }

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

    inline bool is_terminal() const {
        return (type != NONTERM_PRODUCTION);
    }

    std::string nonterm_id_str(bool full_name = true) const {
        if(type == NONTERM_PRODUCTION) {
            // always prefix with underscore as a hack to avoid
            // colliding with target language keywords:
            std::string out = "_" + expr;

            if(full_name)
                return "NontermID::" + out;

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
            case NONTERM_PRODUCTION:
            case LACK_OF_SEPARATOR:
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

}

#endif // GRAMMAR_ELEMENT_H
