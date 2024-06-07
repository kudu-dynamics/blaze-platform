#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int foo(char *str) {
  size_t n;
  char* newStr = NULL;

  printf("I guess you could say I'll be copying that string, pard.\n");
  newStr = strdup(str);
  printf("See? %s\n", newStr);
  return 1;
}

int main(int argc, char *argv[])
{
  foo(argv[1]);
  return 0;
}
