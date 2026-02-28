#ifndef STRINGFORMAT_H
#define STRINGFORMAT_H

#ifndef GENERATED_FPL
  #include "c_str_escape.h"
  #include "is_iterable.h"
  #include "to_hex.h"
#endif

#include <any>
#include <cstdlib>
#include <ctype.h>
#include <list>
#include <tuple>

#include <iostream> // debug


// XXX this is going into the weeds.  what's the immediate goal?
//   - show type as possible conversion
//   - the usual hex conversion etc not as post-processing
// Do I need any of these immediately?  no.
// How's this for a new plan:
//   - run everything through the equivalent of stringformat_post_processor.
//     ":" separates functions.  Each function gets both the original argument
//     (whatever type) and the result of the prior function.  The lack of
//     a function defaults to string formatting as before.  Then :: "just works"
//     (as before) but we don't need the distinction between formatting/post
//     processing.  Then things like indent and hex conversion don't have
//     to be post processed with the silly conversion back to a number;
//     things like string escaping and columnation stay basically the same.
// This isn't what you've implemented here with the "opts" stuff but it's
// actually simpler and more flexible!
// One remaining question is how to handle things like nested maps and/or
// types with mixed data.  Maybe the caller wants to print every other integer
// in hex or whatever - something where different members get different formats.
// Maybe we're not too worried about that, though? (maybe there's a formatter
// where the caller supplies callbacks and we do the traversal?)

// OK Punting for the moment because I don't immediately need this and
// I could really end up in the weeds

// returns the string passed, with a newline at the end
// if the string didn't have one already.
inline std::string ensure_nl(const std::string &src) {
    if(!src.length() || src[src.length() - 1] != '\n')
        return src + "\n";

    return src;
}

// returns a copy of the string passed, with all trailing newlines removed.
inline std::string chop_nl(std::string str) {
    auto new_size = str.length();
    while((new_size > 0) && str[new_size - 1] == '\n')
        --new_size;

    str.resize(new_size);
    return str;
}

// OK SO #include<format> doen't seem to exist on my machine.
// let the reinvention commence.

inline std::string _stringformat(const uint8_t *s, std::string_view opts) {
    if(!s) {
        return "";
    } else {
        return std::string((const char *)s);
    }
}

inline std::string _stringformat(const char *s, std::string_view opts) {
    if(!s) {
        return "";
    } else {
        return std::string(s);
    }
}

inline std::string _stringformat(char * s, std::string_view opts) {
// XXX do we even need this function?  if so, make it not crash on nulls
    return std::string(s);
}

inline std::string _stringformat(char c, std::string_view opts) {
    return std::string(1, c);
}

inline std::string _stringformat(bool b, std::string_view opts) {
    if(b) return "true";
    return "false";
}

// You can call to_string on normal c types such
// as int, float etc and expect a reasonable result,
// but there's no such (identity) sub for std::string.
// Supplying one here makes the _stringformat template
// version below work for things which can convert
// themselves to string.  (sigh)
inline std::string to_string(const std::string &in) {
    return in;
}

// this must be declared BEFORE the most general _stringformat,
// or else it never gets matched, but since it calls the
// more general one, the definition is later
template <typename T, typename U>
std::string _stringformat(std::pair<T, U> &x, std::string_view opts);

// some alternative syntax models I think the WG21 should
// consider for templates:
//   http://shakespearelang.sourceforge.net/report/shakespeare/shakespeare.html
//   https://www.dangermouse.net/esoteric/chef.html

// the next 2 templates are c++ hackery to detect if a thing
// passed has a to_str() method:
template <typename T, typename = int>
struct _has_to_str
    : std::false_type
{};

template <typename T>
struct _has_to_str <T, decltype(&T::to_str, 0)>
    : std::true_type
{};

template <typename T, typename = void>
struct _to_string_exists_for
    : std::false_type
{};

template <typename T>
struct _to_string_exists_for<T,
    std::void_t<decltype(to_string(std::declval<T>()))>>
    : std::true_type
{};

template <typename T, typename = void>
struct _std_to_string_exists_for
    : std::false_type
{};

template <typename T>
struct _std_to_string_exists_for<T,
    std::void_t<decltype(std::to_string(std::declval<T>()))>>
    : std::true_type
{};

template<typename T>
std::string _stringformat(T &in, std::string_view opts = "") {
    if constexpr (std::is_convertible_v<T, std::string> or
                  std::is_convertible_v<T, std::string_view>) {
        // it's either already a string or directly convertible
        // Note:  a const char *foo = nullptr is considered
        // directly convertible, but will also directly crash.
        // at the moment this is avoided via the more specific
        // _stringformat(const char *) function...
        return in;
    } else if constexpr (_has_to_str<T>::value) {
        // it has a to_str method, so use that:
        return in.to_str();
    } else if constexpr (_to_string_exists_for<T>::value) {
        // second choice is a supplied to_string
        return to_string(in);
    } else if constexpr (_std_to_string_exists_for<T>::value) {
        // ... or fall back on std::to_string:
        return std::to_string(in);
    } else if constexpr (is_iterable(T)) {
        // or if it's iterable, recursively compose something
        // from its elements:
        std::string out;
        for(auto el = in.begin(); el != in.end(); ++el) {
            if(std::next(el) == in.end())
                out += _stringformat(*el);
            else
                out += _stringformat(*el) + ", ";
        }
        return "{ " + out + " }";
    } else {
        // otherwise the best we can do is hex dump:
        return "0x" + to_hex(in);
    }
}

template<typename... Args>
std::string _stringformat(std::tuple<Args...> &in, std::string_view opts) {
    // I read online someplace that std::pair was a special case
    // of std::tuple, so I was hoping this would cover std::map
    // entries, but of course it doesn't.  why would I expect
    // any generality?  whatevs.  leaving it because it does
    // work for std::tuple.
// what's the right thing to do with opts in this case?  just pass the options on, regardless of the subtypes?  that could get weird.
    std::string out;
    std::apply([&out](auto &&... args) {
        const int num_args = sizeof...(args);
        const std::string str_arg[] = { _stringformat(args) ... };
        for(int argn = 0; argn < num_args; argn++) {
            if(argn > 0) {
                out += ", ";
            }
            out += str_arg[argn];
        }
    }, in);
    return "( " + out + " )";
}

template <typename T, typename U>
std::string _stringformat(std::pair<T, U> &x, std::string_view opts) {;
// again, what's the right thing to do with opts in this case?
    return _stringformat(x.first) + " => " + _stringformat(x.second);
};

// (as above, but const pair)
template <typename T, typename U>
std::string _stringformat(const std::pair<T, U> &x, std::string_view opts) {;
// again, what's the right thing to do with opts in this case?
    return _stringformat(x.first) + " => " + _stringformat(x.second);
};

class stringformat_alternate_formatter {
}

class stringformat_post_processor {
    // {::c} -> columnate output (on tabs)
    static std::string process_c(const std::string &in) {
        std::list<int> colwidths;

        // find out the starts and widths of the columns:
        int cw = 0;
        colwidths.push_back(0);
        auto col = colwidths.begin();
        for(size_t pos = 0; pos < in.length(); ++pos) {
            cw++; // (the column terminator counts as part of the column)
            // tab, newline, or end of string ends the column:
            if(in[pos] == '\t' || in[pos] == '\n' || (pos + 1 >= in.length())) {
                if(cw > *col) {
                    *col = cw;
                }

                if(std::next(col) == colwidths.end())
                    colwidths.push_back(0);

                // .. and newline puts us back to the first column:
                if(in[pos] == '\n') col = colwidths.begin();
                else                col = std::next(col);
                cw = 0;
            }
        }

        std::string out;
        auto colw = colwidths.begin();
        for(size_t pos = 0; pos < in.length(); pos++) {
            size_t eoc = in.find_first_of("\t\n", pos);
            if(eoc == std::string::npos) eoc = in.length();

            size_t len = eoc - pos;
            out += in.substr(pos, len);

            if(in[eoc] == '\n' || std::next(colw) == colwidths.end()) {
                out += "\n";
                colw = colwidths.begin();
            } else {
                for(int pad = 0; pad < *colw - len; ++pad)
                    out += " ";
                colw = std::next(colw);
            }

            pos = eoc;
        }
        return out;
    }

    // {::e} -> do a c string ecape
    static std::string process_e(const std::string &in) {
        return c_str_escape(in);
    }

    // {::i} -> indent by the level indicated in the string
    // (which needs to be numeric, but, annoyingly, will
    // already have been stringformatted)
// XXX we actually have the originals now, too, so we can use them.
// here's a thought:  arbitrary numbers of ":" in the conversion; they happen one after the other; each is given the original and the result
    static std::string process_i(const std::string &in) {
        int indent = 0;
        int exponent = 1;
        for(int ch = in.length() - 1; ch >= 0; --ch) {
            if((in[ch] < '0') || (in[ch] > '9')) {
                // non-numeric "digit" - just pop out of the loop.
                // in theory we -could- reasonably do something
                // with negatives, but let's not for now.
                break;
            } else {
                indent += (in[ch] - '0') * exponent;
                exponent *= 10;
            }
        }
        std::string out;
        for(int len = 0; len < indent; len++) {
            out += "  ";
        }
// return std::string(indent * 2, ' '); // XXX this is the neater way.  why did we do 2 spaces?  oh 2 spaces is a "level".. hmm

        return out;
    }

    // {::n} -> translate newlines to "\n"
    static std::string process_n(const std::string &in) {
        std::string out;
        size_t last_pos = 0;
        size_t pos = in.find("\n");
        for( ; pos != std::string::npos; pos = in.find("\n", last_pos)) {
            out += in.substr(last_pos, pos - last_pos);
            out += "\\n";
            last_pos = pos + 1;
        }
        out += in.substr(last_pos);

        return out;
    }

    // {::T} -> remove Trailing newlines
    static std::string process_t(const std::string &in) {
        return chop_nl(in);
    }

    // {::U} -> translate characters to upper case
    static std::string process_U(const std::string &in) {
        std::string out;
        for(auto ch : in) {
            out += toupper(ch);
        }
        return out;
    }

    // {::l} -> translate characters to lower case
    static std::string process_l(const std::string &in) {
        std::string out;
        for(auto ch : in) {
            out += tolower(ch);
        }
        return out;
    }

public:
    static std::string process(char fmt, const std::string &in) {
        switch(fmt) {
            case 'c': return process_c(in);  // columnate (tab-delimited)
            case 'e': return process_e(in);  // c-string escape
            case 'i': return process_i(in);  // indent this level
            case 'l': return process_l(in);  // lowercase
            case 'n': return process_n(in);  // translate newlines to '\n'
            case 'T': return process_t(in);  // remove Trailing newlines
            case 'U': return process_U(in);  // uppercase
        }
        // .. would be nice to warn about missing format here....
        return in;
    }
};

template <typename... Args>
std::string stringformat(std::string_view fmt, Args&&... args) {

    const int num_args = sizeof...(args);

    // ok whatevs here's something kinda like format():
    // https://en.cppreference.com/w/cpp/language/parameter_pack
    std::function<std::string(std::string_view)> converters[] = {
        std::function<std::string(std::string_view)>(
            [&args](std::string_view opts) -> std::string {
                return _stringformat(args, opts);
                //return _stringformat(args);
            }
        )...
    };

    std::string out;
    const size_t inlen = fmt.size();
    if(inlen == 0) return ""; // (because inlen might be unsigned)

    int argi = 0;
    size_t ind;
    for(ind = 0; ind < inlen; ++ind) {
        if(fmt[ind] == '{') {
            ++ind; // skip the begin brace
            if(fmt[ind] == '{') {
                // '{{' evaluates to a single '{' (it's how you escape '{')
                out += '{';
            } else {
                // parse the contents of the {}:
                // Within {}, let's say the _future_ format is:
                //  [ variable name ] [':' [ to-string args ] [':' [ post processing ] ] ]
                // .. but there's no way to see the names of the variables,
                // of course.  So, we'll use 0-based argument numbers.
                size_t ts_ind = 0; // pos of to-string args, if any
                size_t ts_len = 0; // length of to-string args
                size_t pp_ind = 0; // pos of post processing function, if any
                long arg_num = argi;
                if(fmt[ind] >= '0' && fmt[ind] <= '9') {
                    const char *istart = fmt.data() + ind;
                    char       *iend   = const_cast<char *>(istart + 1);
                    arg_num = strtol(istart, &iend, 10);
                    ind += iend - istart - 1;
                }
                while(fmt[ind] && (fmt[ind] != '}')) {
                     if(fmt[ind] == ':') {
                         if(!ts_ind)
                             ts_ind = ind + 1;
                         else if(!pp_ind)
                             pp_ind = ind + 1;
                         // else for now we're ignoring spurious ':'
                     }
                     ind++;
                }

                if(arg_num < num_args) {
                    std::string sub = converters[arg_num](std::string_view(fmt.data() + ts_ind, ts_len));
                    if(ts_ind) {
                        while(fmt[ts_ind]) {
                            ts_ind++;
                            if(fmt[ts_ind] == ':' || fmt[ts_ind] == '}')
                                break;
                        }
                    }
                    if(pp_ind) {
                        while(fmt[pp_ind]) {
                            sub = stringformat_post_processor::process(
                                fmt[pp_ind], sub
                            );
                            pp_ind++;
                            if(fmt[pp_ind] == ':' || fmt[pp_ind] == '}')
                                break;
                        }
                    }
                    out += sub;
                } else {
                    out += "(missing arg " + std::to_string(arg_num) + ")";
                }
                argi++;
            }
        } else if((fmt[ind] == '}') && (fmt[ind + 1] == '}')) {
            // turn '}}' into a single '}' so that we can balance
            // braces if there's an embedded '{{' (by doing nothing
            // here and thus skipping the first '}')
        } else {
            out += fmt[ind];
        }
    }

    return out;
}


#endif // STRINGFORMAT_H

