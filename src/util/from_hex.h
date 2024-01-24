#ifndef FROM_HEX_H
#define FROM_HEX_H

#include<cstddef> // for size_t

/*
    Parses a hex number from an ascii-compatible series of bytes
    and returns it in the numeric type specified.

    IN_T must be something which can be addressed via [] returning char
    (or anything char-compatible).  So std::string should work, as should
    char [], etc.

    &pos is the start position from which to parse, and is updated to the
    byte following whatever was parsed.

    OUT_T is assumed numeric and must support bit operations.

    If max_len is nonzero, it indicates the number of source bytes
    to parse.  However, under no circumstances will this parse
    more bytes than will fit in the output type.

    If no hex digits were found at the position passed, this returns 0.

 */
template<typename OUT_T, typename IN_T, typename LEN_T>
OUT_T from_hex(IN_T src, LEN_T &pos, size_t max_len = 0) {
    OUT_T out = 0;
    struct {
        int operator()(char dig) {
            if(dig >= '0' && dig <= '9') return dig - '0';
            if(dig >= 'a' && dig <= 'f') return dig - 'a' + 10;
            if(dig >= 'A' && dig <= 'F') return dig - 'A' + 10;
            return -1;
        }
    } hexdig_int;

    if(max_len <= 0) {
        max_len = sizeof(OUT_T) * 2; // (2 hex digits per byte)
    }

    if(max_len > sizeof(OUT_T) * 2) {
        max_len = sizeof(OUT_T) * 2;
    }

    const LEN_T start = pos;
    while(hexdig_int(src[pos]) >= 0) {
        if(pos - start >= max_len) break;
        pos++;
    }

    LEN_T cur_pos = pos;
    int out_shift = 0;
    while(cur_pos-- > start) {
        char inch = src[cur_pos];
        out |= OUT_T(hexdig_int(inch)) << out_shift;
        out_shift += 4;
    }

    return out;
}

#endif // FROM_HEX_H
