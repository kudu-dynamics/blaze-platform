#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUFFER_SIZE 32

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
char global_kernel_buffer[64];

void int_overflow_func() { 
  new_user_id++;
}

void not_int_overflow_func() {
  if (new_user_id < 1024)
    new_user_id++; 
}

typedef struct conn_thang {
  char data[BUFFER_SIZE];
  void (*callback)(void);  
} conn_thang;

void controlled_indirect_call(conn_thang *conn) {
  conn->callback();
}

unsigned long _copy_from_user(void * dst, void * src, unsigned long size) {
  return 0;
}

void unbounded_copy_from_user1(void * src, unsigned long size) {

  if (size > 5) {
    printf("Yah!");
  }

  int x = _copy_from_user(global_kernel_buffer, src, size);

}

void unbounded_copy_from_user2(void * src, unsigned long size) {

  if (size > 5) {
    return;
  }

  int x = _copy_from_user(global_kernel_buffer, src, size);

}

void copy_to_const_addr(void * src, unsigned long size) {
  // this const should not be flagged as a global variable
  int x = _copy_from_user((void *)0xDEADBEEF, src, size);
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

  struct conn_thang c1;
  c1.data[0] = 'a';
  c1.callback = &int_overflow_func;
  controlled_indirect_call(&c1);
 
  void * t = malloc(64);
  unbounded_copy_from_user1(t, 10);  
  unbounded_copy_from_user2(t, 10);
  copy_to_const_addr(t, 10);

  return;
}

