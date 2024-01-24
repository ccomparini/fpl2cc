
size_t len = 0;
if(inp[0] == '#') {
    len++;
    while(inp[len] && inp[len] != '\n') {
        len++;
    }
}

while(inp[len] && (
    inp[len] == ' '   ||
    inp[len] == '\t'  ||
    inp[len] == '\n'  ||
    inp[len] == '\v'  ||
    inp[len] == '\f'  ||
    inp[len] == '\r'
)) {
    len++;
}

return len;

