
/**

Tests part of the guts of the code used to determine which steps go with
which reduce parameters - specifically, the part which determines which
step is the "canonical" step for a given parameter - i.e. the step from
which all paramter stack entries holding values for the parameter can be
found.

 */

#include "fpl2cc/productions.h"
#include "util/jerror.h"
#include "util/src_location.h"
#include "util/stringformat.h"

#include <iostream>
#include <string>
#include <vector>

using namespace fpl;

struct cs_test {
    const utf8_buffer            src;
    const std::string            whence;

    cs_test(const char *s, src_location caller = CALLER()) :
        src((const utf8_byte *)s),
        whence(caller) {
    }

    void run() {
        std::cout << stringformat("{}:  {::n}\n", whence, src);

        const char *fn[] = { __FILE__, nullptr };
        fpl_options opts(1, fn, 0, 0);
        productions prds(opts, make_shared<fpl_reader>(src, whence));
        prds.parse_fpl();

        prds.resolve_terminal_masking();
        prds.resolve_steps();
        prds.resolve_melds();

        for(int rulenum = 0; rulenum < prds.rule_count(); ++rulenum) {
            auto rule = prds.rule(rulenum);
            if(rule.is_subexpression())
                continue;

            std::cout << stringformat("  rule {}: {}\n", rulenum, rule);

            for(auto pname : rule.parameter_names()) {
                auto step = prds.canonical_step_for_param(rulenum, pname);
                int offset = step.final_offset();
                if(!offset) {
                    std::cout << stringformat("    {}:  {}\n", pname, step);
                } else {
                    std::cout << stringformat("    {}: [{}] {}\n", pname, offset, step);
                }
            }
        }
    }
};

static std::vector<cs_test> tests = {
    {
        "a b c d -> e;"
    },
    {
        "(a b)* c d -> e;"
    },
    {
        "(foo:aaa b) bar:aaa d -> e;"
    },
    {
        "a* -> foo;\n"
        "a foo b -> bar;"
    },
    {
        // src/fpl2cc/test/reserve.test/reserve.fpl_expect
        "(thing ';'^)* -> things;"
    },
    {
        "stuff* -> goal\n"
        "a_with_trailing_bs  -> stuff;\n"
        "abbb                -> stuff;\n"
        "just_a              -> stuff;\n"
        "bb                  -> stuff;\n"
        "'b'                 -> stuff;\n"
        "'a'^ 'b':first (', '^ 'b')*:tr -> a_with_trailing_bs;\n"
        "'a'^ ('b' 'b' 'b')+:bees -> abbb;\n"
        "'a'^ -> just_a;\n"
        "'b' 'b' -> bb;"
    },
};

int main(int argc, const char **argv) {
    std::cout << stringformat("... testing canonical steps for parameters\n");
    for(auto test : tests) {
        test.run();
    }
}
