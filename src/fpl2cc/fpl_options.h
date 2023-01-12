#ifndef FPL_OPTIONS_H
#define FPL_OPTIONS_H

#include <list>
#include <string>

#include "util/fs.h"
#include "util/searchpath.h"

namespace fpl {

struct fpl_options {
    const int version_maj;
    const int version_min;

    std::string src_fpl;
    Searchpath src_path;
    std::string output_fn;

    bool check_only;
    bool debug;
    std::list<std::string> entry_points;
    bool generate_code;
    bool generate_main;
    bool help;
    bool single_step;
    std::string depfile;
    std::string statedump;
    bool dump_dependencies;

    int lr_stack_reserve;
    int param_stack_reserve;

    bool new_parser;

    std::list<std::string> errors;
    inline void error(const std::string &errst) {
        errors.push_back(errst);
    }

    std::list<std::string> impl_sources;

    void add_source(const std::string &fn) {
        std::string ext(fs::path(fn).extension());
        if(ext != ".fpl") {
            impl_sources.push_back(fn);
        } else {
            if(src_fpl.size() != 0) {
                error("only one source fpl is supported at present");
            } else {
                src_fpl = fn;
            }
        }
    }

    // for some options, we allow the user to specify
    // just a filename extension (by starting the name
    // with a '.'), in which case the full filename
    // will be based on the output_fn.
    std::string infer_filename(const std::string &spec) {
        if(spec[0] != '.') {
            // assume it's a full filename:
            return spec;
        } else {
            // starts with a "." so it's a filename extension:
            if(!output_fn.length()) {
                error("can't infer file name: no output specified");
            } else {
                std::filesystem::path pt(output_fn);
                pt.remove_filename();
                std::string bn = std::filesystem::path(output_fn).stem();
                return pt.string() + bn + spec;
            }
        }
        // theoretically, we can't get here:
        error("mass confusion in infer_filename");
        return "";
    }

    static bool arg_to_int(int &val, const std::string &arg) {
        if(!arg.length())
            return false;

        bool worked = false;
        try {
            val = std::stod(arg);
            worked = true;
        } catch(std::invalid_argument const& ex) {
            // (just return false);
        } catch(std::out_of_range const& ex) {
            // (just return false);
        } // anything else indicates a bug

        return worked;
    }

    // janky, but good enough:
    // (move to some kind of json spec to describe options in
    // general)
    fpl_options(int argc, const char* const* argv, int vmaj, int vmin) :
        version_maj(vmaj),
        version_min(vmin),
        src_path(),
        check_only(false),
        debug(false),
        generate_code(true),
        generate_main(false),
        help(false),
        single_step(false),
        dump_dependencies(false),
        lr_stack_reserve(1000),
        param_stack_reserve(1000),
        new_parser(false)
    {
        for(int argi = 1; argi < argc; argi++) {
            // c++ -- :P
            #define SCAN_VALUE() \
                if(val.empty()) { \
                    argi++; \
                    if(argi < argc) { \
                        val = std::string(argv[argi]); \
                    } \
                }

            const char *arg = argv[argi];

            if(!arg)    continue; // should not be able to happen, but...
            if(!arg[0]) continue; // I guess ignore blank args

            if(arg[0] == '-') {
                if(arg[1] == '-') {
                    // double dash '--foo' style args:
                    std::string opt(arg + 2); // (+2 skips dashes)
                    std::string val;
                    size_t pos = opt.find_first_of("=");
                    if(pos != std::string::npos) {
                        val = opt.substr(pos + 1);
                        opt = opt.substr(0, pos);
                    }

                    if(opt == "check-only") {
                        check_only = true;
                    } else if(opt == "debug") {
                        debug = true;
                    } else if(opt == "debug-single-step") {
                        debug = true;
                        single_step = true;
                    } else if(opt == "depfile") {
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--depfile requires filename");
                        depfile = val;
                    } else if(opt == "dump-dependencies") {
                        dump_dependencies = true;
                    } else if(opt == "goal") {
                        // specifies what we want out of this parser.
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--goal requires a value");
                        entry_points.push_back(std::string(val));
                    } else if(opt == "generate-main") {
                        generate_main = true;
                    } else if(opt == "help") {
                        help = true;
                    } else if(opt == "new-parser") {
                        // undocumented/temporary option for side-by-side
                        // testing of fpl based fpl parser
                        new_parser = true;
                    } else if(opt == "no-generate-code") {
                        generate_code = false;
                    } else if(opt == "out") {
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--out requires a value");
                        output_fn = val;
                    } else if(opt == "lr-stack-reserve") {
                        SCAN_VALUE();
                        if(!arg_to_int(lr_stack_reserve, val))
                            errors.push_back("--lr-stack-reserve requires a numeric value");
                    } else if(opt == "param-stack-reserve") {
                        SCAN_VALUE();
                        if(!arg_to_int(param_stack_reserve, val))
                            errors.push_back("--param-stack-reserve requires a numeric value");
                    } else if(opt == "src-path") {
                        SCAN_VALUE();
                        src_path.append(val);
                    } else if(opt == "statedump") {
                        SCAN_VALUE();
                        if(val.empty())
                            errors.push_back("--statedump requires filename");
                        statedump = val;
                    } else {
                        error("Unknown option: --" + opt);
                    }
                } else {
                    // single-dash arg:
                    error("Unknown option: " + std::string(arg));
                }
            } else {
                add_source(arg);
            }
            #undef SCAN_VALUE
        }

        // special case for depfiles:  if the name begins with a ".",
        // consider the depfile name to be the extension and use the
        // fpl base name.  this makes it easier to jam into scons,
        // which basically wants a separate depfile per source.
        depfile = infer_filename(depfile);
        statedump = infer_filename(statedump);
    }

    // returns the relative-path source filename
    std::string src_filename() const {
        return fs::relative(src_fpl, fs::path("."));
    }

    std::string out_filename() const {
        if(output_fn.length()) {
            return output_fn;
        } else {
            // no output filename was specified.
            // base it on the input name.
            if(src_fpl.length()) {
                std::filesystem::path pt(src_fpl);
                pt.replace_extension("cc");
                return pt.filename();
            }
        }

        return "";
    }

    int version_major() const { return version_maj; }
    int version_minor() const { return version_min; }
    std::string version() const {
        return stringformat("v{}.{}", version_major(), version_minor());
    }

    void usage() {
        std::cerr << stringformat(
            "\nfpl2cc {}\n"
            "\nUsage:    fpl2cc [options] <fpl source> [sources]\n\n"
            "If no [target] is specified, prints to stdout.\n"
            "[sources] is 0 or more non-fpl source files to integrate\n"
            "into the target file.\n\n"
            "Options:\n"
            "\t--debug - emebed debug blather in target code\n"
            "\t--debug-single-step - as above plus pauses\n"
            "\t--depfile=<fn> - generate ninja/make depend file\n"
            "\t--dump-dependencies - print import dependencies\n"
            "\t--goal=<product> - specify a target production\n"
            "\t--generate-main - generate main() function\n"
            "\t--help - show this page\n"
            "\t--no-generate-code - parse (and other options) only\n"
            "\t--out=<fn> - write to fn instead of stdout\n"
            "\t--src-path=<path> - search the dirs given (':' delimited)\n"
            "\t--statedump=<fn> - dump generated states to file\n",
            version()
        );
    }

    std::string to_str() const {
        std::string out;

        out += stringformat(
            "    src: {}\n"
            "    src_path: {}\n"
            "    output: {}\n"
            "    generate_main: {}\n"
            "    debug: {}\n"
            "    single_step: {}\n",
            src_fpl, src_path, output_fn, generate_main, debug, single_step
        );

        if(entry_points.size() > 0) {
            out += "    goals:\n";
            for(auto goal : entry_points) {
                out += "        " + goal + "\n";
            }
        }
        if(impl_sources.size()) {
            out += "    impl: ";
            for(auto impl : impl_sources) {
                out += impl + " ";
            }
            out += "\n";
        }

        return out;
    }
};

}

#endif // FPL_OPTIONS_H
