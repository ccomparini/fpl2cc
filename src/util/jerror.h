#ifndef JERROR_H
#define JERROR_H

#include <functional>
#include <list>
#include <iostream>
#include <string>
#include "util/src_location.h"
#include "util/stringformat.h"

/* 
 "jest" error system ... or to-be.  I had to call it something.
 
  Usage:

     void my_error_handler(const std::string &msg, src_location caller) { ... }
     void my_warning_handler(const std::string &msg), src_location caller ...

     // install my_error_handler_func as the error handler
     jerror::handler(jerror::error_channel, my_error_handler);
     jerror::handler(jerror::warning_channel, my_warning_handler);

     if(something_of_concern)
         jerror::warning("we have concerns...");
 
     if(something_bad)
         jerror::error("something bad happened");

 Applications or libraries calling jerror::error should _not_ assume 
 that the program or function or anything will terminate, but they
 should also not assume they won't.
 
 Why not use exceptions?  C++ exceptions don't allow warn vs error.
 Also, c++ exceptions lean heavily on making a new type for everything
 which can go wrong, which is a pain.  Also, this lets the receiving
 handler know the src_location whence the error or warning was sent.

 */
class jerror {
public:
    using callback = std::function<
        void(const std::string &error, src_location caller)
    >;

    using channel = enum {
        error_channel   = 0,
        warning_channel,
        NUM_CHANNELS
    };

private:
    using handler_stack = std::list<callback>;

    static inline handler_stack channels[NUM_CHANNELS];

    static void on_msg(
        channel chan, const std::string &msg, src_location caller
    ) {
        handler_stack handlers = channels[chan];

        if(handlers.size()) {
            (handlers.back())(msg, caller);
        } else {
            std::cerr << msg;
            if(chan > error_channel) {
                std::cerr << ensure_nl(stringformat(" at {}", caller));
            } else {
                std::cerr << ensure_nl(stringformat("Aborting at {}", caller));
                exit(2112);
            }
        }
    }

public:

    class handler {
        channel which_chan;
    public:
        handler(channel chan, callback cb, src_location caller = CALLER()) :
            which_chan(chan)
        {
            if((which_chan >= 0) && (which_chan < NUM_CHANNELS)) {
                jerror::channels[which_chan].push_back(cb);
            } else {
                error(stringformat(
                    "{} Can't install handler - there's no channel {}\n",
                     caller, which_chan
                ));
            }
        }
        ~handler() {
            if((which_chan >= 0) && (which_chan < NUM_CHANNELS)) {
                jerror::channels[which_chan].pop_back();
            }
        }
    };

    // call this to throw an error.
    // calls the most recently installed error handler, or, if
    // there's no such thing installed, prints an error message
    // and aborts.
    static void error(
        const std::string &msg, src_location caller = CALLER()
    ) {
        on_msg(error_channel, msg, caller);
    }

    static void warning(
        const std::string &msg, src_location caller = CALLER()
    ) {
        on_msg(warning_channel, msg, caller);
    }
};

#endif // JERROR_H

