
#include "fpl2cc/productions.h"
#include "util/jerror.h"
#include "util/src_location.h"
#include "util/stringformat.h"

#include <iostream>
#include <string>
#include <vector>

using namespace fpl;

struct path_test {
    const std::string name;
    const utf8_buffer src;
    const std::string whence;

    path_test(const char *n, const char *s, src_location caller = CALLER())
        : name(n), src((utf8_byte *)s, strlen(s)), whence(caller) { }

    void run() {
        const char* tname[] = { name.c_str() };
        fpl_options opts(1, tname, 0, 0);
        productions prds(opts, make_shared<fpl_reader>(src, whence));

        prds.parse_fpl();
        prds.resolve_steps();

        std::cout << stringformat("{}:  {}\n", name, src);

        //for(int rulenum = prds.rule_count() - 1; rulenum >= 0; --rulenum) {
        for(int rulenum = 0; rulenum < prds.rule_count(); ++rulenum) {
            auto rule = prds.rule(rulenum);
/*
            std::cout << stringformat(
                "    flat paths for {}: {}\n",
                rule, rule.meld_paths()
            );
 */
            if(!rule.is_subexpression()) {
                for(auto path : prds.meld_paths(rulenum)) {
                    std::cout << stringformat("    {}\n", path);
                }
            }
        }
    }
};

static std::vector<path_test> tests = {
    // Note:  not all the expressions here are actually valid.
    // Many of them result in impossible melds.  We need to be
    // able to generate meld paths for them in order to detect
    // that fact, though, and that's the point of this test.
    {
        "simple",
        "a -> b;",
    },
    {
        "a then b",
        "a b -> x;",
    },
    {
        "b repeats and is optional",
        "a b* -> x;"
    },
    {
        "a repeats",
        "a+ -> b;"
    },
    {
        "a repeats and is optional",
        "a* -> b;"
    },
    {
        "a is opt, followed by opt",
        "a? b? -> c;"
    },
    {
        "simple subexpression",
        "(a b) -> c;"
    },
    {
        "eject entire subexpression",
        "(a b)^ -> c;"
    },
    {
        "rep before",
        "opt* (suba subb) -> foo;"
    },
    {
        "simple opts and reps",
        "opt? mult* minmult+ -> foo;"
    },
    {
        "multiple subexpression",
        "(a b^)+ -> c;"
    },
    {
        "multiple optional subexpression",
        "(a b^)* -> c;"
    },
    {
        "sub subs ejected at top",
        "topa (sa (ssa ssb)+)^+ -> c;"
    },
    {
        "sub subs ejected one down",
        "topa (sa^ (ssa ssb)^+)+ -> c;"
    },
    {
        "sub subs ejected two down",
        "topa (sa (ssa^ ssb^)+)+ -> c;"
    },
    {
        "sub subs individually ejected",
        "topa (sa^ (ssa^ ssb^)+)+ -> c;"
    },
    {
        "optional multiple subexpression",
        "a (a b^)* c -> x;"
    },
    {
        "weird meld test",
        "'a'^ ((b ','^ b) c)* 'd'^ -> foo;"
    },
    {
        "optional subex",
        "'u'?:not_signed 'int'^ (~ size_bits)? -> type;"
    },
    {
        "optional subex with optional start",
        "a (b* c)* d -> x;"
    },
    {
        "expression which failed in rparams.test",
        "'('^ (fruit (','^ fruit)*)? ')'^ -> fruit_list;\n"
        "fruit_list -> list;",
    },
};

int main(int argc, const char **argv) {
    std::cout << stringformat("... testing rule paths\n");
    for(auto test : tests) {
        test.run();
    }
}

