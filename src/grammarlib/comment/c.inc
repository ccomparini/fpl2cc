
size_t len = 0;
size_t last_len = -1;
while(inp[len] &&  last_len != len) {
    last_len = len;

    // regular whitespace, incuding various non-ascii spaces:
    while(size_t splen = space_length(inp + len)) {
        len += splen;
    }

    // c style comment:
    if(inp[len] == '/' && inp[len + 1] == '*') {
        size_t comment_start = len;
        len += 2;
        bool got_end = false;
        while(inp[len] && !got_end) {
            if(inp[len] == '*' && inp[len + 1] == '/') {
                // (end of comment)
                len += 2;
                got_end = true;
            } else {
                len++;
            }
        }

        if(!got_end) {
            // no end of the comment.  return such that the
            // parser still points to the start of the comment.
            // we need to do this because if the input has
            // only partially been read, when more input comes
            // in and this routine is called again, it won't know
            // that it's inside a comment.  Also, if the calling
            // parser thinks this is an error, returning the start
            // of the comment should give it (and thus the author
            // of whatever we're compiling) a better idea of where
            // the problem is.
            return comment_start;
        }
    }

    // c++ style line comment:
    if(inp[len] == '/' && inp[len + 1] == '/') {
        size_t comment_start = len;
        len += 2;
        while(inp[len] && inp[len] != '\n') {
            len++;
        }
        if(!inp[len]) {
            // similar to block comments, if we didn't get a
            // newline, don't eat the comment (yet), since
            // more input might be forthcoming, and when/if
            // this routine is called next it won't be aware
            // it was still processing a line comment:
            return comment_start;
        }
    }
}

return len;

