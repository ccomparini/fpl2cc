#ifndef TO_HEX_H
#define TO_HEX_H

#include<string>

// why this not already exist?
template<class T>
std::string to_hex(const T &inst) {
    union {
        T tinst;
        std::array<unsigned char, sizeof(T)> buf;
    } hack;
    hack.tinst = inst;
    static const char hchr[] = {
        '0', '1', '2', '3', '4', '5', '6', '7',
        '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
    };
    std::string out("0x");
    for(int ind = 0; ind < sizeof(T); ind++) {
       out += hchr[(hack.buf[ind] >> 4) & 0x0f];
       out += hchr[ hack.buf[ind]       & 0x0f];
    }
    return out;
}

#endif // TO_HEX_H
