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

template<typename T>
std::string _stringformat(T in, const std::string &opts = "") {
    using namespace std;
    return to_string(in);
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

class stringformat_post_processor {
    // {::n} -> translate newlines to "\n"
    static std::string n(const std::string &in) {
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

public:
    static std::string process(char fmt, const std::string &in) {
        switch(fmt) {
            case 'n': return n(in);
        }
        // .. would be nice to warn about missing format here....
        return in;
    }
};

template <typename... Args>
std::string stringformat(std::string_view fmt, Args&&... args) {

    // ok this works at all - string convert each argument,
    // before and regardless of if it's going to be used.
    // not my fave structure, not least because it prevents
    // but it's not unreasonable
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
            } else {
                // parse the contents of the {}:
                // Within {}, let's say the _future_ format is:
                //  [ variable name ] [':' [ to-string function ] [':' [ post processing ] ] ]
                // But, NOTE: presently, because we can't see the names
                // of the parameters and we've already converted everything
                // to string, the only one of these we support is the post
                // processing part.  I'm implementing that part now because
                // I specifically want it (for killing newlines).
                size_t ts_ind = 0; // pos of to-string function, if any
                size_t pp_ind = 0; // pos of post processing function, if any
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

                if(argi < num_args) {
                    std::string sub = str_arg[argi];
                    if(ts_ind) {
                        while(fmt[ts_ind]) {
                            // (no to-string function support presently,
                            // but it would go here)
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
                    out += "(missing arg " + std::to_string(argi) + ")";
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

// returns the string passed, with an extra newline at the end
// if the string didn't have one already.
inline std::string ensure_nl(const std::string &src) {
    if(src[src.length() - 1] != '\n') 
        return src + "\n";

    return src;
}


#endif // STRINGFORMAT_H

