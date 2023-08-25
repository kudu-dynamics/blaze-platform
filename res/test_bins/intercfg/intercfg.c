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

int single_path_no_calls(int n) {
  return n;
}

int single_path_with_call(int n) {
  printf("Your number: %d\n", n);
  return n;
}

int double_path_no_calls(int n) {
  if (n > 10) {
    return 8;
  } else {
    return 17;
  } 
}


int main(int argc, char *argv[])
{
  outer_a();
  outer_b();

  return 0;
}
