#ifndef SEARCHPATH_H
#define SEARCHPATH_H

#include<list>
#include<string>

#include"fs.h"

class Searchpath {

    bool initialized;
    std::list<fs::path> directories;

public:
    // default constructor creates an uninitialized and therefore
    // "false" searchpath:
    Searchpath() : initialized(false) { }

    // takes a ':' delimited set of directories to search.
    Searchpath(const std::string &path) : initialized(true) {
        size_t start = 0;
        size_t colon;
        while((colon = path.find(':', start)) != std::string::npos) {
            // note:  blanks considered ok for now, so that you
            // can pass a filename relative to root (if blank is
            // included in the path).  (is this too subtle?)
            append(path.substr(start, colon - start));
            start = colon + 1;
        }
        if(start < path.length()) {
            append(path.substr(start, path.length() - start));
        }
    }

    operator bool() const {
        return initialized;
    }

    std::string to_str() const {
        std::string out;
        for(auto el : directories) {
            if(out.length() > 0) 
                out += ":";
            out += el;
        }
        return out;
    }

    void append(const fs::path &element) {
        initialized = true;
        directories.push_back(element);
    }

    void prepend(const fs::path &element) {
        initialized = true;
        directories.push_front(element);
    }

    std::string find(const std::string &fn) const {
        for(auto dir = directories.begin(); dir != directories.end(); ++dir) {
            fs::path fp = fs::path(*dir);
            fp.append(fn);
            if(fs::exists(fp)) {
                return fp;
            }
        }

        // can't find it in the set of directories - return
        // the filename passed.  This makes it so that if
        // the caller simply passes the result to open() or
        // whatever, it'll either work, or they can create a
        // reasonable error message based on what was returned.
        return fn;
    }
};

#endif // SEARCHPATH_H


