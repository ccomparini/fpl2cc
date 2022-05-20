#ifndef STRINGFORMAT_H
#define STRINGFORMAT_H

#include "to_hex.h"
#include <list>

// OK SO #include<format> doen't seem to exist on my machine.
// let the reinvention commence.
// The good thing is maybe I can fpl up the jest string formatting
// and slap it in here.

inline std::string _stringformat(const std::string &s) {
    return s;
}

inline std::string _stringformat(const char * s) {
    return std::string(s);
}
inline std::string _stringformat(char * s) {
    return std::string(s);
}

template<typename T>
std::string _stringformat(T in, const std::string &opts = "") {
/*
    // XXX TODO field width, padding, time formats, etc
    if(opts == "x")
        return to_hex(in);
 */
    return std::to_string(in);
}

template<typename T>
std::string _stringformat(const std::list<T> &list, const char *j = ", ") {
    std::string out;
    for(auto el = list.begin(); el != list.end(); ++el) {
        if(std::next(el) == list.end())
            out += _stringformat(*el);
        else
            out += _stringformat(*el) + j;
    }
    return out;
}

template <typename... Args>
std::string stringformat(std::string_view fmt, Args&&... args) {

    // ok this works at all - string convert each argument,
    // before and regardless of if it's going to be used.
    // not my fave structure, but it's not unreasonable
    // to expect to have to convert each argument anyway.
    // in fact it might be worth warning if there are extra
    // (or not enough) arguments.
    const int num_args = sizeof...(args);
    const std::string str_arg[] = { _stringformat(args) ... };

    // possibly better syntax.  see the notes file.

    // ok whatevs here's something kinda like format():
    // https://en.cppreference.com/w/cpp/language/parameter_pack
    // OMG you cannot iterate the Args list.  you have to recurse.
    // oh but you _can_ expand to a tuple and sorta iterate that,
    // but not at run time.
    // I _think_ the c++ way you have to do this backwards
    // and for each agument substitute that arg into the string,
    // as opposed to going through the string as I'm doing. ohwell.
    std::string out;
    const size_t inlen = fmt.size();
    if(inlen == 0) return ""; // (because inlen might be unsigned)

    int argi = 0;
    size_t ind;
    for(ind = 0; ind < inlen; ++ind) {
        if(fmt[ind] == '{') {
            if(fmt[ind + 1] == '{') {
                // '{{' evaluates to a single '{' (it's how you escape '{')
                ++ind;
                out += '{';
            } else if(fmt[ind + 1] == '}') {
                // positional substitution:
                ++ind; // (eat the '}')
                if(argi < num_args) {
                    out += str_arg[argi];
                } else {
                    out += "(missing arg " + std::to_string(argi) + ")";
                }
                argi++;
            } else {
                
                // it's not clear to me how named arguments can/should work
                // for this in c++, (I guess it could alternate argname, value)
                // so... blowing off non-positional substitutions for now. 
/*
                size_t pend = fmt.find("}", ind);
                if(pend == npos) {
                    // no closing '}'.. I guess just plow on and .. copy
                    // this is another reason to go with the simpler % format.
                } else {
                }
 */
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

// returns the string passed, with an extra newline at the end
// if the string didn't have one already.
inline std::string ensure_nl(const std::string &src) {
    if(src[src.length() - 1] != '\n') 
        return src + "\n";

    return src;
}


#endif // STRINGFORMAT_H

