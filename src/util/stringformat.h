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

// returns the string passed, with an extra newline at the end
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
// Oh, interesting. this is a nightmare in c++.

//inline std::string _stringformat(const utf8_byte *s) {
inline std::string _stringformat(const uint8_t *s) {
    if(!s) {
        return "";
    } else {
        return std::string((const char *)s);
    }
}

inline std::string _stringformat(const char *s) {
    if(!s) {
        return "";
    } else {
        return std::string(s);
    }
}

inline std::string _stringformat(char * s) {
    return std::string(s);
}

inline std::string _stringformat(char c) {
    return std::string(1, c);
}

inline std::string _stringformat(bool b) {
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
std::string _stringformat(std::pair<T, U> &x);

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
std::string _stringformat(T &in, const std::string &opts = "") {
    if constexpr (std::is_convertible_v<T, std::string> or
                  std::is_convertible_v<T, std::string_view>) {
        // it's either already a string or directly convertible
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
std::string _stringformat(std::tuple<Args...> &in) {
    // I read online someplace that std::pair was a special case
    // of std::tuple, so I was hoping this would cover std::map
    // entries, but of course it doesn't.  why would I expect
    // any generality?  whatevs.  leaving it because it does
    // work for std::tuple.
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
std::string _stringformat(std::pair<T, U> &x) {;
    return _stringformat(x.first) + " => " + _stringformat(x.second);
};

// (as above, but const pair)
template <typename T, typename U>
std::string _stringformat(const std::pair<T, U> &x) {;
    return _stringformat(x.first) + " => " + _stringformat(x.second);
};

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

    std::function<std::string()> converters[] = {
        std::function<std::string()>(
            [&args]() {
                return _stringformat(args);
            }
        )...
    };

    // possibly better syntax.  see the notes file.

    // ok whatevs here's something kinda like format():
    // https://en.cppreference.com/w/cpp/language/parameter_pack
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
                //  [ variable name ] [':' [ to-string function ] [':' [ post processing ] ] ]
                // But, we can't, presently, because we can't see the names
                // of the parameters and we've already converted everything
                // to string.  So let's allow numeric parameter index instead
                // of the variable name.
                size_t ts_ind = 0; // pos of to-string function, if any
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
                    std::string sub = converters[arg_num]();
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

