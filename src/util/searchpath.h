#ifndef SEARCHPATH_H
#define SEARCHPATH_H

#include<cstdlib>
#include<list>
#include<regex>
#include<string>

#include"fs.h"
#include"stringformat.h" // debugging - remove me
#include<iostream>       // debugging - remove me

class Searchpath {

    bool initialized;
    std::list<fs::path> directories;

public:
    // default constructor creates an uninitialized and therefore
    // "false" searchpath:
    Searchpath() : initialized(false) { }

    // takes a ':' delimited set of directories to search.
    Searchpath(const std::string &path) : initialized(true) {
        append_path(path);
    }

    void append_path(const std::string &path) {
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
        initialized = true;
    }

    void append_from_env(const std::string &var) {
        if(const char *env_p = std::getenv(var.c_str())) {
            append_path(env_p);
        }
    }

    operator bool() const {
        return initialized;
    }

    // Returns a parsable searchpath string showing the full
    // expanded searchpath, with all directories converted
    // to absolute paths (i.e. showing unambiguously what
    // would be searched by find()).
    std::string to_str() const {
        std::string out;
        for(auto el : directories) {
            if(out.length() > 0) 
                out += ":";
            out += std::filesystem::absolute(el);
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

    // Searches the path for the file of the name passed.
    // Returns the name of the first file found, or the
    // name passed if no matching file was found.
    // This allows callers to simply wrap the filename
    // they're passing to an existing function call and
    // not have to add any particular new error handling.
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

    // Searches the path for all files matching the regex string passed.
    // Returns a list of matches.
    // Throws an unhelpful exception if the regex string passed
    // doesn't compile.
    std::list<std::string> find_re(const std::string &search) const {
        std::regex re(search);
        std::list<std::string> found;
        for(auto dir : directories) {
            if(fs::exists(dir)) { // or else throws exceptions!
                for(auto file_i : fs::directory_iterator{dir}) {
                    std::string file_name = file_i.path();
                    std::smatch match;
                    if(std::regex_search(file_name, match, re)) {
                        found.push_back(file_name);
                    }
                }
            }
        }
        return found;
    }

    auto begin() const { return directories.begin(); }
    auto end()   const { return directories.end();   }
};

#endif // SEARCHPATH_H


