
if(inp[0] == '#') {
    size_t len = 1;
    while(inp[len] && inp[len] != '\n') {
        len++;
    }
    return len;
}

return 0;

