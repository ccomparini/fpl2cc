#ifndef TO_HEX_H
#define TO_HEX_H

#include<array>
#include<string>

#include<stdio.h>

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

// Because of various endianess issues, to get reasonable results
// for various normal ints, we need this.
// For example, 0x
template<class T>
std::string _int_to_hex(T val) {
    int shift = sizeof(val) * 8;
    std::string out;
    out.reserve(2 * sizeof(val)); // 2 chars per byte
    while((shift -= 8) >= 0) {
        out += to_hex(char((val >> shift) & 0xff));
    }

    return out;
}
std::string to_hex(signed int num) { return _int_to_hex(num); }
std::string to_hex(signed long int num) { return _int_to_hex(num); }
std::string to_hex(signed long long int num) { return _int_to_hex(num); }
std::string to_hex(signed short int num) { return _int_to_hex(num); }
std::string to_hex(unsigned int num) { return _int_to_hex(num); }
std::string to_hex(unsigned long int num) { return _int_to_hex(num); }
std::string to_hex(unsigned long long int num) { return _int_to_hex(num); }
std::string to_hex(unsigned short int num) { return _int_to_hex(num); }

template<class TT>
std::string bs_to_hex(const TT &in, const std::string &separator = " ") {
    std::string out;
    for(auto el : in) {
        if(out.length())
            out += separator;
        out += to_hex(el);
    }
    return out;
}

#endif // TO_HEX_H
