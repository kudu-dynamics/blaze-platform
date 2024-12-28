#include <stdlib.h>
#include <stdio.h>

int root_node_is_only_goto() {
      int r;
      while (1)
      {
          int r = rand() % 100;;
          if (r == 5)
          {
              break;
          }
          printf("Bad number: %d\n", r);
      }
      return r;
}

int main() {
  printf("hello\n");
  return 0;
}
