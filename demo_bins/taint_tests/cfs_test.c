// This file is used for testing the controlled format string primitive

#include <stdio.h>

int main() {
    char buf[32];
    fgets(buf, sizeof(buf), stdin);
    printf(buf); // format string vuln

    return 0;
}
