// plt_thunk_demo.c
#include <stdio.h>

void foo(int i, int j) {
  if (i < j) {
    printf("This is the test PLT func!\n");
  }
}

void jenkins() {
  foo(43, 80);
}
