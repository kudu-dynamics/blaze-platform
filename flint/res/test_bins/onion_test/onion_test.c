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

void use_after_free(){
  char *data = (char *)malloc(20 * sizeof(char));
  strcpy(data, "hello world");
  free(data);
  data[4] = 'z';
}

void a_write(char *data){
  data[3] = 0x41;
}

void use_after_free2(){
  char *data = (char *)malloc(20 * sizeof(char));
  strcpy(data, "hello world");
  free(data);
  a_write(data);
}

int new_user_id = 0;

void int_overflow_func() { 
  new_user_id++;
}

void not_int_overflow_func() {
  if (new_user_id < 1024)
    new_user_id++; 
}


void main() {
  void *ptr = malloc1(30);
  a_write((char *)ptr);
  double_free1(ptr);

  void *ptr2 = calloc1(8, 10);
  free2(ptr2);

  void *ptr3 = malloc1(10);
  free2(ptr3);

  use_after_free();
  use_after_free2();

  int_overflow_func();
  not_int_overflow_func();
  
  return;
}
