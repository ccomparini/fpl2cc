#ifndef TO_UTF8ISH_H
#define TO_UTF8ISH_H


/*
  "utf8-ish" is utf-8 style encoding, wherein a 31-bit value into 
  a sequence of 1 or more bytes, with the high bits of the first
  byte indicating the total encoded length.

  Unlike official utf-8, this will encode values above 0x10ffff,
  and (consequently) the result may be up to 6 bytes in length
  (as opposed to 4).

  No unicode validity checking is done.

  The result can be written to any std::string compatible object.

  Unencodable results (i.e. source > 31 bits) result in  0 length
  output.
  
 */
template<typename OUT_T>
OUT_T to_utf8ish(uint32_t cp) {

    OUT_T out;
    
    // official utf-8 output is only up to 0x10ffff, which encodes into
    // 4 bytes:
    //  U+0000   U+007F   0xxxxxxx
    //  U+0080   U+07FF   110xxxxx 10xxxxxx
    //  U+0800   U+FFFF   1110xxxx 10xxxxxx 10xxxxxx
    //  U+10000  U+10FFFF 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    // we support continuing this up to 6 bytes output.
    if(cp <= 0x7f) {               // 8 bits
        // fits in one byte - shipit:
        out[0] = cp;
    } else {
        int out_size;
        if(cp <= 0x000007ff) {  // 11 bits
            out_size = 2;
        } else if(cp <= 0x0000ffff) {  // 16 bits
            out_size = 3;
        } else if(cp <= 0x001fffff) {  // 21 bits
            out_size = 4;
        } else if(cp <= 0x03ffffff) {  // 26 bits
            out_size = 5;
        } else if(cp <= 0x7fffffff) {  // 31 bits
            out_size = 6;
        } else {
            // the first byte in the output encodes the length
            // of the whole thing (in the leading 1-bits), which
            // limits the number of data bits to 31. 
            // so this is fail.
            out_size = 0;
        }
        out.resize(out_size);
        for(int pos = out_size - 1; pos > 0; pos--) {
            out[pos] = 0x80 | (cp & 0x3f);
            cp = cp >> 6;
        }
        int8_t header_bits = 0x80;
        header_bits >>= out_size - 1;
        out[0] = header_bits | cp;
    }

    return out;
}

#endif // TO_UTF8ISH_H
