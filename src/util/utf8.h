#ifndef UTF8_H
#define UTF8_H

typedef unsigned char utf8_byte;

namespace utf8 {

/**

utf-8 unicode function miscelany.

These are unicode/utf-8 related functions I ripped from other
code I wrote and which I'm stashing here.  Generally speaking,
these functions take a utf8_byte pointer and return a size_t
telling how many bytes at that position match what they were
asked for.  Also generally, they check if they are passed a
NULL pointer and return a length of 0 for such.

One could probably split hairs and point out that, despite being
in the namespace "utf8", these functions are specific to (de-facto
or official) unicode characters, and utf8 could theoretically be
used to encode other character sets.  I've never seen that, though.

 */

/**
  Returns true if the pointer passed appears to be within a utf-8
  character (as opposed to being at the start of one); false otherwise.

  If this returns true, *at is definitely not the start of a utf-8
  encoded character.  If it returns false, it might be.

  Returns false is given a NULL pointer.
 */
inline bool is_within_character(const utf8_byte *at) {
    if(!at) return false;

    // If the top 2 bits of the byte are 0b10, then
    // it's a continuation byte (i.e. within a character)
    // 0xc0 = 0b11000000
    // 0x80 = 0b10000000
    return (*at & 0xc0) == 0x80;
}


/**
  Returns the number of bytes which the pointer passed would have
  to be advanced to get to the "next" character (assuming there is
  one).

  This means if the pointer passed points to the start of a utf-8
  encoded character, the number of bytes returned will be the
  length of the character, but if the pointer points to the middle
  of a character, callers can still advance to the ostensible start
  of next character by incrementing by the value returned.  It's
  a best-effort approach.

  This all also means (due to the best effort), that if the pointer
  passed points to non-utf-8-encoded data, an arbitrarily large length
  (up to maxlen) may be returned.

  Caveat: The maxlen passed is the maximum length which this function
  will examine, and not necessarily the max length returned.  So if,
  say, this is passed a pointer to the last byte of a buffer, but that
  byte indicates a character length of more than one, the length
  returned might indicate a next character outside the buffer.
  However, this function will not attempt to access memory outside
  of maxlen bytes past the pointer passed.

  Note also: byte 0x00 is, for purposes of this function, a 1-byte
  character.  So don't use this to look for a 0-length character
  at the end of a string.

 */
inline size_t character_length(const utf8_byte *at, size_t maxlen) {

    if(!at) return 0;

    // https://en.wikipedia.org/wiki/UTF-8#Encoding
    // if the high bit isn't set, it's a single byte:
    if((at[0] & 0x80) == 0) return 1;

    // otherwise, if we're at the start of the character,
    // the top 2 bits will be set, and bits following
    // specify the size.  so if the top 2 bits are set,
    // we'll assume we're at the start of a char and take
    // that byte's word for it on the size:
    if((at[0] & 0xe0) == 0xc0) return 2;  // 0b110x xxxx
    if((at[0] & 0xf0) == 0xe0) return 3;  // 0b1110 xxxx
    if((at[0] & 0xf8) == 0xf0) return 4;  // 0b1111 0xxx

    // looks like we're in the middle of a character,
    // so count bytes until we're no longer within the
    // character:
    int len = 1;
    for( ; len < maxlen; ++len) {
        if(!is_within_character(at + len))
            break;
    }

    return len;
}

/*
  Returns the length in bytes of the newline character at the
  position passed (which will be 0 if it's not a newline).

  Note:  This covers de-facto newlines (such as the MS Windows/MS DOS
  0x0d 0x0a newline) as well as official unicode newlines.

 */
inline size_t newline_length(const utf8_byte *at) {
    // For our purposes, any 2 bytes in a row with one each of 0x0a
    // and 0x0d counts as a single newline (which covers Microsoft
    // newlines and some old-fashioned newlines).  Also, either of
    // 0x0d or 0x0a alone counts as a newline (which covers unix
    // newlines and some other old fashioned newlines).

    // Do we want to count other unicode newlines?
    //  https://www.unicode.org/standard/reports/tr13/tr13-5.html
    // this^^ implies yes;  on the other hand, is it more confusing
    // if people put weird newlines in their source code?
    // Probably.  But, for consistency, we probably should - we
    // do already count them as spaces.
    // (Perhaps pass a flag so the caller can say what they want?)

    if(at == NULL)  return 0;

    if(at[0] == 0x0d) {
        if(at[1] == 0x0a) return 2; // Microsoft newline
        return 1; // OS-9 style newline (heh)
    }

    if(at[0] == 0x0a) {
        if(at[1] == 0x0d) return 2; // weirdo old British newline
        return 1; // normal unix newline
    }

    return 0;
}

/*
  Returns the length in bytes of the utf-8 character at *at,
  or 0 if that character isn't a space.
 */
inline size_t space_length(const utf8_byte *at) {
    if(at == NULL) return 0;

    switch(*at) {
        // ascii ones are simple and common:
        case 0x09:    // character tabulation (aka "tab")
        case 0x0A:    // line feed
        case 0x0B:    // line tabulation
        case 0x0C:    // form feed
        case 0x0D:    // carriage return
        case 0x20:    // space
            return 1;
        case 0xc2:
            if(at[1] == 0x85) return 2; // U+0085 = next line
            if(at[1] == 0xa0) return 2; // U+00A0 = no-break space
            return 0;
        case 0xe1:
            if(at[1] == 0x9a && at[2] == 0x80)
                return 3; // 0xe1,0x9a,0x80 = U+1680 = ogham space mark
            return 0;
        case 0xe2:
            if(at[1] == 0x80) {
                if(at[2] >= 0x80 && at[2] <= 0x8a) {
                    // 0xe2,0x80,0x80 -> U+2000 = en quad
                    // 0xe2,0x80,0x81 -> U+2001 = em quad
                    // 0xe2,0x80,0x82 -> U+2002 = en space
                    // 0xe2,0x80,0x83 -> U+2003 = em space
                    // 0xe2,0x80,0x84 -> U+2004 = three-per-em space
                    // 0xe2,0x80,0x85 -> U+2005 = four-per-em space
                    // 0xe2,0x80,0x86 -> U+2006 = six-per-em space
                    // 0xe2,0x80,0x87 -> U+2007 = figure space
                    // 0xe2,0x80,0x88 -> U+2008 = punctuation space
                    // 0xe2,0x80,0x89 -> U+2009 = thin space
                    // 0xe2,0x80,0x8a -> U+200A = hair space
                    return 3;
                }

                if(at[2] == 0xa8)
                    return 3; // 0xe2,0x80,0xa8 -> U+2028 = line separator

                if(at[2] == 0xa9)
                    return 3; // 0xe2,0x80,0xa9 -> U+2029 = paragraph separator

                if(at[2] == 0xaf)
                    return 3; // 0xe2,0x80,0xaf -> U+202F = narrow no-break sp.

            } else if(at[1] == 0x81 && at[2] == 0x9f) {
                return 3; // 0xe2,0x81,0x9f -> 205F = medium mathematical space
            }
            return 0;

        case 0xe3:
            if(at[1] == 0x80 && at[2] == 0x80)
                return 3; // 0xe3,0x80,0x80 = U+3000 ideographic space
            return 0;

        default:
            // not space
            return 0;
    }

    // can't get here.
} 

} // end namespace utf8


#endif // UTF8_H
