#ifndef CUSTOM_TERMINAL_H
#define CUSTOM_TERMINAL_H

#include "code_block.h"
#include "grammar_element.h"

#include "util/jerror.h"
#include "util/src_location.h"
#include "util/stringformat.h"

#include <list>
#include <string>

namespace fpl {

// used for supporting @terminal, @scanner, and @assertion
struct custom_terminal {
    fpl::grammar_element gel;

    // some types of terminals (eg group and compound terminals)
    // are composed of sub terminals:
    std::list<fpl::grammar_element> components;

    // ... other terminals have specific code telling it how to
    // match.
    code_block                      code;

    custom_terminal() { }

    custom_terminal(const custom_terminal &src) :
        gel(src.gel),
        components(src.components),
        code(src.code) { }

    custom_terminal(const fpl::grammar_element &g) : gel(g) { }

    // construct from a code block, inferring the grammar element type
    // from the code source language (regex etc).
    // "default" language creates a custom terminal (as opposed to,
    // say, an assertion)
    custom_terminal(
        const code_block &c,
        bool inv = false,
        src_location caller = CALLER()
    ) : code(c)
    {
        fpl::grammar_element::Type type = fpl::grammar_element::Type::NONE;
        switch(code.language) {
            case code_block::REGEX:
                type = fpl::grammar_element::TERM_REGEX;
                break;
            case code_block::EXACT_MATCH:
                type = fpl::grammar_element::TERM_EXACT;
                break;
            case code_block::DEFAULT:
                type =  fpl::grammar_element::TERM_CUSTOM;
                break;
            default:
                jerror::error(
                    stringformat(
                        "bad code_block language '{}' in custom terminal",
                        to_string(code.language)
                    ), caller
                );
                inv = false; // can't invert it, so avoid further errors
                break;
        }

        gel = fpl::grammar_element(code.code, type);
        if(inv) {
            gel.invert_type();
        }
    }

    custom_terminal(
        const fpl::grammar_element &g,
        const code_block &c
    ) : gel(g),
        code(c) {
    }

    custom_terminal(
        const fpl::grammar_element &g,
        const std::list<fpl::grammar_element> &se
    ) : gel(g),
        components(se) {
    }

    bool is_stub() const { return code.is_stub; }

    std::string source_location()          const { return code.location(); }
    code_block::source_language language() const { return code.language; }

    fpl::grammar_element::Type el_type() const {
        return gel.type;
    }

    fpl::grammar_element element() const {
        return gel;
    }

    // If an inverse can be automatically generated,
    // returns true.
    // otherwise, returns false.
    bool can_auto_invert() const {
        switch(el_type()) {
            case grammar_element::TERM_ASSERTION:
            case grammar_element::TERM_ASSERTION_INV:
            case grammar_element::TERM_EXACT:
            case grammar_element::TERM_EXACT_INV:
            case grammar_element::TERM_REGEX:
            case grammar_element::TERM_REGEX_INV:
            case grammar_element::TERM_GROUP:
            case grammar_element::TERM_GROUP_INV:
            case grammar_element::TERM_COMPOUND:
            case grammar_element::TERM_COMPOUND_INV:
                return true;
            default:
                break;
        }
        return false;
    }

    // if this terminal is or can be implemented as a builtin
    // grammar element type (for example, if it's an exact match),
    // returns a grammar_element for that type.
    // Otherwise, returns a false grammar_element.
    fpl::grammar_element builtin_el() const {
        auto type = el_type();
        switch(type) {
            case grammar_element::TERM_EXACT:
            case grammar_element::TERM_EXACT_INV:
            case grammar_element::TERM_REGEX:
            case grammar_element::TERM_REGEX_INV:
                return fpl::grammar_element(code.code, type);
            default:
               break;
        }

        return fpl::grammar_element("", fpl::grammar_element::NONE);
    }
};

} // end namespace fpl

#endif //  CUSTOM_TERMINAL_H
