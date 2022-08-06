#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

int biff = 10;
void h();
void i();
void j();
void filmont();

long f(long a, long b) {
  return (a + b);
}

void g(long a, long b) {
  long patty[3] = {1, 2, 3};
  long vv = 3483;
  long vx = 0;
  // long y = 3432;
  float y = 3.234;
  double z = 88.432;
  long x = f(a, 80) + f(b, 45);
  if (vv || 3) {
    printf("billy jones %d\n", &patty);
  }
  if (z < 99.0) {
    printf("z is less than 99.0");
  }
  j();
  printf("what: %ld\n", a);
  if (x > f(30, 80)) {
    printf("You did it: %ld\n", x);
  } else {
    printf("sorry\n");
  }
  h(76 + a);
  j();
}

long bcakes(unsigned long a, long b) {
  signed long k =  a * b;
  return k + 5;
}

unsigned int fcakes(int n, int m) {
  return n & m;
}

void h(long a) {
  long xx = a || 43;
  int logo = biff + 3;
  printf("sorry h %d\n", xx);
  i(a);
  if ((a << 2) > 25) {
    biff = 20;
    filmont();
  }
  logo = biff - 7;
}

void j() {
  int arr[] = { 10, 20, 30, 40 };
  int i;
  for( i = 0; i < 4; i++ ) {
    printf("value of arr[%d]: %d\n", i, arr[i]);
  }
  printf("sorry j\n");
}

void jenkins(long a, long b, long c, long d) {
  int x;
  x = 0;
  if (a && b && c && d)
    x = 1;
  /* else */
  /*   x = 0; */
  printf("Jenkin's result: %d.\n", x);
}

void i(long a) {
  printf("sorry %ld\n", a);
}

void filmont() {
  printf("I only print this.\n");
}

int main() {
  int *jimmy = 0;
  printf("hello world\n");
  g(23, 488);
  
  return 0;
}
