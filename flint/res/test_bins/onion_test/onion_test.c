#include <stdio.h>
#include <stdlib.h>
#include <string.h>


__attribute__((noinline))
void free1(void *ptr) {
  free(ptr);
}

__attribute__((noinline))
void free2(void *ptr) {
  free(ptr);
}

__attribute__((noinline))
void calls_free2(void *ptr) {
  free2(ptr);
}

__attribute__((noinline))
void double_free1(void *ptr) {
  free1(ptr);
  calls_free2(ptr);
}

__attribute__((noinline))
void *malloc1(size_t size) {
  return malloc(size);
}

__attribute__((noinline))
void *calloc1(size_t n, size_t m) {
  return calloc(n, m);
}

void main() {
  void *ptr = malloc1(30);
  double_free1(ptr);

  void *ptr2 = calloc1(8, 10);
  free2(ptr2);

  void *ptr3 = malloc1(10);
  free2(ptr3);


  return;
}
