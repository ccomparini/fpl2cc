#ifndef TO_HEX_H
#define TO_HEX_H

#include<array>
#include<string>

// returns a string containing a big-endian hex dump
// of the thing passed
template<class T>
std::string to_hex(const T &inst) {
    unsigned char *hack = (unsigned char *)&inst;
    static const char hchr[] = {
        '0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };

    std::string out;
    for(int ind = 0; ind < sizeof(T); ind++) {
       out += hchr[(hack[ind] >> 4) & 0x0f];
       out += hchr[ hack[ind]       & 0x0f];
    }
    return out;
}

template<class TT>
std::string bs_to_hex(const TT &in) {
    std::string out;
    for(auto el : in) {
        out += to_hex(el);
        out += ' ';
    }
    return out;
}

#endif // TO_HEX_H
