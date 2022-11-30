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

#include "fpl_reader.h"
#include "util/fs.h"
#include "util/src_location.h"

#include "fpl_options.h"
#include "productions.h"

#define VERSION_MAJ 0
#define VERSION_MIN 9

using namespace fpl;

enum ExitVal {
    OK = 0,
    FAIL = 1,
    BAD_ARGS = 2
};

void fail(const std::string &msg, src_location caller = CALLER()) {
    std::cerr << ensure_nl(msg);

    exit(ExitVal::FAIL);
}

static int num_warnings = 0;
void warn(const std::string &msg, src_location caller = CALLER()) {
    // (indent warnings so that errors stand out)
    std::cerr << ensure_nl(stringformat("    warning: {}", msg));
    num_warnings++;
}

/*

  fpl grammar:

   <exprs to match> -> <production name> ;
                   or
   <exprs to match> -> <production name> <code_block>

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
  - add timings so we can compare algorithms/implementations
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
  - @comment_style and @separator - maybe just have @elider and
    allow multiple?  default also doesn't have to be space.
    lots of modern stuff puts space in the grammar.
  - precedence: square brackets?
  o document the fpl (see docs dir)
    - docs suck and are out of date.  probably want to inline them

 */

/*

Production rules are ordered;  first one matches.

Production rules can be looked up by the name of the thing they
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

// 
// Writes a make-style dependency file, which can be used by ninja (or scons,
// or I suppose, make) to know what other fpls are imported.
//  https://clang.llvm.org/docs/ClangCommandLineReference.html#dependency-file-generation
//  https://ninja-build.org/manual.html#ref_headers
//  https://scons.org/doc/production/HTML/scons-user.html#idm46358248900448
//
// the format appears to be old-school make lines, with newlines escaped.
//  eg:
//    some_grammar.cc: /foo/bar/some_grammar.fpl\
//               /grammalib/json.fpl
// (with some_grammar.cc being exactly at start of line, because space
// is significant in make)
void write_depfile(const productions &prds, const fpl_options &opts) {

    FILE *depf = fopen(opts.depfile.c_str(), "w");
    if(!depf) {
        fail(stringformat(
            "can't open dependency file '{}' for write: {}",
            opts.depfile, strerror(errno)
        ));
    } else {
        // we only process one .fpl at this point, so all dependencies
        // are assumed to go under that:
        fprintf(depf, "%s:", opts.output_fn.c_str());

        fprintf(depf, " %s", opts.src_fpl.c_str());

        for(auto dep : prds.imported_files()) {
            fprintf(depf, "\\\n %s", dep.c_str());
        }
        fprintf(depf, "\n\n");

        fclose(depf);
    }
}

// returns an exit()-appropriate status (i.e. 0 on success)
ExitVal fpl2cc(const fpl_options &opts) {
    std::shared_ptr<fpl_reader> inp;

    if(opts.src_fpl.size() == 0) {
        inp = make_shared<fpl_reader>(
            std::cin, "<stdin>", fail_reader_adapter
        );
    } else {
        inp = make_shared<fpl_reader>(opts.src_fpl, fail_reader_adapter);
    }

    std::string output_fn;
    std::ofstream output_file;
    if(!opts.check_only) {
        output_fn = opts.out_filename();
        // If we're writing to a file, remove any existing
        // copy of that file so that if we get some kind
        // of error, no one will mistake an old copy for
        // the output.
        if(output_fn.length())
            fs::remove(output_fn);
    }

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

    // (see also opts.depfile)
    if(opts.dump_dependencies)
        for(auto dep : prds.imported_files())
            fprintf(stdout, "%s\n", dep.c_str());

    if(opts.depfile.length())
        write_depfile(prds, opts);

    std::string output;
    if(opts.generate_code)
        output = prds.generate_code(opts);

    if(output.length() && !opts.check_only) {
        if(!output_fn.length()) {
            std::cout << output;
        } else {
            if(FILE *out = fopen(output_fn.c_str(), "w")) {
                // uhh... this is easy, if hokey:
                fprintf(out, "%s\n", output.c_str());
                fclose(out);
            } else {
                fail(stringformat(
                    "--out: can't open '{}' for write: {}",
                    output_fn, strerror(errno)
                ));
            }
        }
    }

    return ExitVal::OK;
}

int main(int argc, const char** argv) {
    jerror::handler erhand(jerror::error_channel, fail);
    jerror::handler wahand(jerror::warning_channel, warn);
    fpl_options opts(argc, argv, VERSION_MAJ, VERSION_MIN);
    ExitVal status = ExitVal::FAIL;

    if(opts.errors.size()) {
        for(auto err : opts.errors) {
            std::cerr << ensure_nl(err);
        }
        opts.usage();
        status = ExitVal::BAD_ARGS;
    } else if(opts.help) {
        opts.usage();
        status = ExitVal::OK;
    } else {
        status = fpl2cc(opts);
    }

    if(num_warnings > 0) {
        std::cerr << stringformat(
            "{}: {} warnings\n", opts.src_filename(), num_warnings
        );
    }

    exit(status);
}


