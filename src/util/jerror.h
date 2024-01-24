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
     // (they need names so that the destructors don't get
     // called right away)
     jerror::handler x(jerror::error_channel, my_error_handler);
     jerror::handler y(jerror::warning_channel, my_warning_handler);

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
    using channel = enum {
        error_channel   = 0,
        warning_channel,
        NUM_CHANNELS
    };

    using callback = std::function<
        void(const std::string &error, src_location caller)
    >;
    using callback_with_channel = std::function<
        void(channel chan, const std::string &error, src_location caller)
    >;

private:
    using handler_stack = std::list<callback_with_channel>;

    static inline handler_stack channels[NUM_CHANNELS];

    static void on_msg(
        channel chan, const std::string &msg, src_location caller
    ) {
        handler_stack handlers = channels[chan];

        if(handlers.size()) {
            (handlers.back())(chan, msg, caller);
        } else {
            if(chan > error_channel) {
                std::cerr << ensure_nl(stringformat("{} at {}", msg, caller));
            } else {
                std::cerr << ensure_nl(stringformat("Fatal error: {}\tat {}", msg, caller));
                exit(2112);
            }
        }
    }

    static void push_handler(
        channel chan, callback_with_channel cb, src_location caller
    ) {
        if((chan >= 0) && (chan < NUM_CHANNELS)) {
            jerror::channels[chan].push_back(cb);
        } else {
            error(stringformat(
                "{} Can't install handler - there's no channel {}\n",
                 caller, chan
            ));
        }
    }

    static void pop_handler(channel chan) {
        if((chan >= 0) && (chan < NUM_CHANNELS)) {
            jerror::channels[chan].pop_back();
        }
    }

public:

    // handler is a class so that it can push itself to the handler list
    // and pop itself when it goes out of scope
    class handler {
        channel which_chan;
    public:
        handler(channel chan, callback cb, src_location caller = CALLER()) :
            which_chan(chan) {
            push_handler(
                which_chan,
                [cb](channel, const std::string &err, src_location caller) {
                    cb(err, caller);
                },
                caller
            );
        }

        handler(channel chan, callback_with_channel cb, src_location caller = CALLER()) :
            which_chan(chan) {
            push_handler(which_chan, cb, caller);
        }

        ~handler() {

            pop_handler(which_chan);
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

