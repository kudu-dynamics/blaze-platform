#include <stdio.h>
#include <stdlib.h>

void inner(int x) {
    if (x == 42) {
        puts("*");
    } else {
        puts("else");
    }
}

void outer_a() {
    inner(42);
}

void outer_b() {
    inner(1);
}

int main(int argc, char *argv[])
{
  outer_a();
  outer_b();

  return 0;
}
