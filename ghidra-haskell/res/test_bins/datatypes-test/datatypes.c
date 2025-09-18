#include <stdio.h>


struct test {
    int x;
    char y;
    float z;
};

int main() {
    struct test s1;
    s1.x = 5;
    s1.y = 'a';
    s1.z = 0.5;

    for (int i = 0; i < 10; i++) {
        s1.x *= s1.x + 5;
        s1.y = (char) i;
        s1.z += 0.12 * s1.z;
    }

    printf("%d,%c,%f\n", s1.x,s1.y,s1.z);
}





