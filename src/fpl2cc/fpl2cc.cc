#include <cassert>
#include <climits>
#include <list>
#include <map>
#include <memory>
#include <queue>
#include <regex>
#include <set>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdexcept>
#include <string>
#include <vector>

#include "searchpath.h"
#include "fpl_reader.h"
#include "util/fs.h"
#include "util/src_location.h"

#include "options.h"
#include "productions.h"

using namespace fpl;

enum ExitVal {
    OK = 0,
    FAIL = 1,
    BAD_ARGS = 2
};

void fail(const std::string &msg, src_location caller = CALLER()) {
    if(msg[msg.length() - 1] != '\n') {
        fprintf(stderr, "%s\n", msg.c_str());
    } else {
        fprintf(stderr, "%s", msg.c_str());
    }

    exit(ExitVal::FAIL);
}

static int num_warnings = 0;
void warn(const std::string &msg, src_location caller = CALLER()) {
    // indent warnings so that errors stand out:
    if(msg[msg.length() - 1] != '\n') {
        fprintf(stderr, "    warning: %s\n", msg.c_str());
    } else {
        fprintf(stderr, "    warning: %s", msg.c_str());
    }

    num_warnings++;
}

/*

  fpl grammar:

   <exprs to match> -> <production name> ;
                   or
   <exprs to match> -> <production name> <code_block>
                   or

  In the first case, the ; tells it to reduce using a default/stub
  function, OR if an implementation file has been specified, to call
  a reduce function specified in that file.

  In the second case, reduce using the code_block specified.

  Expressions may be any of:
   - double-quoted string ("xxx") - match text
   - regular expression within slashes (eg /0x[0-9a-fA-F]+/)
   - names of other productions as plain text (no spaces)

  Expressions may be followed (no space) by one of *, +, or ?
  to mean 0-or-more, 1-or-more, or 0-or-1 respectively.

  Comments start with "#" and go to the end of the line.

  All input tokens are separated by space.

  Code blocks:

  Code blocks are enclosed in +{ }+.

  Code blocks access their corresponding expressions via pseudo-positional
  argument variables named arg[0..x] (so the first is called arg0, second
  arg1, etc).  The type of each argument variable depends on the types
  and (possible) repetitions of the expressions:
    - quoted string      -> std::string
    - regular expression -> std::smatch  XXX this is lies presently
    - production name    -> Jest::Member

  Code blocks should use a normal "return" statement to return a pointer
  to a Jest::Member.

 */

/*
 TODO/fix

Thu Apr 14 08:54:41 PDT 2022
  - separate stack for arguments.  this will allow ejected params
    to not be stored in the first place and simplify argument passing.
    Also lets us go back to recursive ascent (potentially)
  - add timings so we can compare algorithms/implementations
  - make it so you can do regex separators/comments.
    this will allow separators to be specified in "pure" fpl
  - Refactor:
    - separate parsing out of Productions
    - make an fpl based fpl parser, then add:
      - parens to join steps for purposes of repetition, so as to easily
        support old-style languages which don't allow trailing commas
        and such.  eg allow:   foo (',' foo)* ->  foo_list ;
      - [ ] for precedence grouping
        - allow 'right-to-left' after such grouping to specify associativity
      - -> expression ( fpl ) for sub-parsers!
        i.e. ... see grammarlib/strf.fpl
      - some way to do errors in pure fpl

  Abstract the target language/application:
  - error handling and messaging is _terrible_ right now (with
    the default, anyway)
  - bug:  if everything in your fpl grammar is optional, it generates
    some kind of infinite loop, I guess looking for nothing.
    eg:
      foo* -> done ;
      'a' -> foo ;
    .. actually similar issue on regexes which match 0-length, if match
    count is > 1 - it'll keep looping on a 0 length match, because
    0 length doesn't advance the read pointer, which means it'll match
    again (and again and again...)
  - get jest in here :D
  - lots of stuff is misnamed:
    - "num_args" should be count or size or something sensible
    - @comment_style and @separator - maybe just have @elider and
      allow multiple?  default also doesn't have to be space.
      lots of modern stuff puts space in the grammar.
  - if a generated fpl encounters an unexpected anything, it stops
    parsing (by design).  this could be used for incremental parsing
    in cases where you are parsing a buffer as it's being filled
    (such as parsing network input or even just from the command line).
    the current buffering framework doesn't allow that, though.
  - precedence.  maybe an @prec (... )? or ^other_rule?
  o document the fpl (see docs dir)
    - docs suck and are out of date.  probably want to inline them

 */

/*

Production rules are ordered;  first one matches.

Production rules can be looked up the name of the thing they
produce.

Each production rule is an array of things to match (items),
the type of thing produced (string/grammar_element) and typically
a code block (string).  Each item has a minimum and maximum
number of times to match (default 1; may be 0, 1, or, in the
max case, infinite).

 */

// temporary adapter for fpl_reader errors:
// (or not temporary?)
void fail_reader_adapter(const std::string &msg) {
    fail(msg, "(fpl_reader)");
}

// self generated parser class:
//#include "fpl_parser.h"

// returns an exit()-appropriate status (i.e. 0 on success)
ExitVal fpl2cc(const options &opts) {
    if(opts.src_fpl.size() == 0)
        fail("Error:  no source fpl specified");
    auto inp = make_shared<fpl_reader>(opts.src_fpl, fail_reader_adapter);

    // parse the input file into a set of productions:
    productions prds(opts, inp); // XXX don't pass inp here
    if(opts.new_parser) {
        fail("Sorry, new parser not supported\n");
/*
        fpl_parser parser(inp);
        parser.prds = &prds;
        parser.parse();
 */
    } else {
        prds.parse_fpl();
    }

    // OH hai!  turns out there's already a standard for this,
    // which we should support (and which will make this drop
    // into ninja relatively easily):
    //  https://clang.llvm.org/docs/ClangCommandLineReference.html#dependency-file-generation
    if(opts.dump_dependencies)
        for(auto dep : prds.imported_files())
            fprintf(stdout, "%s\n", dep.c_str());

    std::string output;
    if(opts.generate_code)
        output = prds.generate_code();

    // states are generated as a side effect of generate_code,
    // which is not great, but I'm not going to fix it right now,
    // so dump_states has to go after generate_code():
    if(opts.dump_states)
        prds.dump_states();

    // uhh... this is easy, if hokey:
    if(opts.out) {
        if(output.length())
            fprintf(opts.out, "%s\n", output.c_str());
    } else {
        fail("no open output - fail\n");
        return ExitVal::BAD_ARGS;
    }

    return ExitVal::OK;
}

int main(int argc, const char** argv) {
    jerror::handler erhand(jerror::error_channel, fail);
    jerror::handler wahand(jerror::warning_channel, warn);
    options opts(argc, argv);
    ExitVal status = ExitVal::FAIL;

    if(opts.errors.size()) {
        for(auto err : opts.errors) {
            std::cerr << ensure_nl(err);
        }
        opts.usage();
        status = ExitVal::BAD_ARGS;
    } else {
        if(opts.help)
            opts.usage();

        status = fpl2cc(opts);
    }

    if(num_warnings > 0) {
        fprintf(stderr, "fpl2cc: %i warnings\n", num_warnings);
    }

    exit(status);
}


