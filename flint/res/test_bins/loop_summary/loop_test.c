#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* =========================================================
   Test binary for loop summarization.
   Compile: gcc -g -O0 -fno-stack-protector -no-pie -o loop_test loop_test.c
   ========================================================= */

volatile int sink;  /* prevent optimizer from removing stores */

/* ---------------------------------------------------------
   1. Simple counting loop: for (i = 0; i < n; i++)
   Expect: linear induction var i, stride +1, bound n
   --------------------------------------------------------- */
__attribute__((noinline))
void simple_count(char *buf, int n) {
    for (int i = 0; i < n; i++) {
        buf[i] = 0;
    }
}

/* ---------------------------------------------------------
   2. Countdown loop: for (i = n-1; i >= 0; i--)
   Expect: linear induction var i, stride -1, bound 0
   --------------------------------------------------------- */
__attribute__((noinline))
void countdown(char *buf, int n) {
    for (int i = n - 1; i >= 0; i--) {
        buf[i] = 0;
    }
}

/* ---------------------------------------------------------
   3. Stride-4 loop: for (i = 0; i < n; i += 4)
   Expect: linear induction var i, stride +4, bound n
   --------------------------------------------------------- */
__attribute__((noinline))
void stride4(int *buf, int n) {
    for (int i = 0; i < n; i += 4) {
        buf[i] = 42;
    }
}

/* ---------------------------------------------------------
   4. Nested loops:
      for (i = 0; i < rows; i++)
        for (j = 0; j < cols; j++)
          mat[i*cols + j] = 0;
   Expect: inner j summarized first, then outer i
   --------------------------------------------------------- */
__attribute__((noinline))
void nested_loops(int *mat, int rows, int cols) {
    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            mat[i * cols + j] = 0;
        }
    }
}

/* ---------------------------------------------------------
   5. Linked list traversal (non-linear: should FALL BACK)
   The "next" pointer is a load, not v +/- C.
   --------------------------------------------------------- */
struct node {
    int val;
    struct node *next;
};

__attribute__((noinline))
int sum_list(struct node *head) {
    int total = 0;
    struct node *cur = head;
    while (cur != NULL) {
        total += cur->val;
        cur = cur->next;
    }
    return total;
}

/* ---------------------------------------------------------
   6. Complex condition: while (x == 0 || (x == 1 && y < 100))
   The branch condition is a compound OR/AND expression.
   With -O0, the compiler may split this into multiple
   basic blocks (short-circuit evaluation), which means the
   "loop condition" is spread across several branch nodes,
   not a single BranchCond. This tests how our summarizer
   handles conditions it can't parse from a single stmt.
   --------------------------------------------------------- */
__attribute__((noinline))
void complex_condition(int *arr) {
    int x = 0;
    int y = 0;
    while (x == 0 || (x == 1 && y < 100)) {
        arr[y] = x + y;
        y++;
        if (y > 50) {
            x = 1;
        }
    }
}

/* ---------------------------------------------------------
   7. Loop with early break
   for (i = 0; i < n; i++) {
     if (buf[i] == needle) break;
   }
   The sampled path takes one specific exit, so the bound
   comes from whichever condition the path actually takes.
   --------------------------------------------------------- */
__attribute__((noinline))
int find_byte(char *buf, int n, char needle) {
    for (int i = 0; i < n; i++) {
        if (buf[i] == needle)
            return i;
    }
    return -1;
}

/* ---------------------------------------------------------
   8. Loop with function call in body
   The body contains a call — the summarized body should
   still include the call statement.
   --------------------------------------------------------- */
__attribute__((noinline))
void process_item(int idx, int val) {
    sink = idx * val;
}

__attribute__((noinline))
void loop_with_call(int *arr, int n) {
    for (int i = 0; i < n; i++) {
        process_item(i, arr[i]);
    }
}

/* ---------------------------------------------------------
   9. Loop with symbolic bound from memory
   The bound comes from a struct field loaded from memory.
   --------------------------------------------------------- */
struct config {
    int max_items;
    int flags;
};

__attribute__((noinline))
void symbolic_bound(int *arr, struct config *cfg) {
    for (int i = 0; i < cfg->max_items; i++) {
        arr[i] = cfg->flags;
    }
}

/* ---------------------------------------------------------
   10. Triple-nested loop
   for (i...) for (j...) for (k...)
   --------------------------------------------------------- */
__attribute__((noinline))
void triple_nested(int *cube, int x, int y, int z) {
    for (int i = 0; i < x; i++) {
        for (int j = 0; j < y; j++) {
            for (int k = 0; k < z; k++) {
                cube[i * y * z + j * z + k] = i + j + k;
            }
        }
    }
}

/* ---------------------------------------------------------
   11. Do-while loop (post-test)
   The backedge structure differs from a for/while loop:
   the condition check is at the tail, not the header.
   --------------------------------------------------------- */
__attribute__((noinline))
int do_while_sum(int n) {
    int i = 0;
    int sum = 0;
    do {
        sum += i;
        i++;
    } while (i < n);
    return sum;
}


int main(int argc, char **argv) {
    char buf[256];
    int ibuf[256];
    int mat[256];
    int cube[512];

    simple_count(buf, 100);
    countdown(buf, 100);
    stride4(ibuf, 100);
    nested_loops(mat, 4, 8);

    struct node n3 = {30, NULL};
    struct node n2 = {20, &n3};
    struct node n1 = {10, &n2};
    sink = sum_list(&n1);

    complex_condition(ibuf);
    sink = find_byte(buf, 256, 'x');
    loop_with_call(ibuf, 10);

    struct config cfg = {50, 0xFF};
    symbolic_bound(ibuf, &cfg);

    triple_nested(cube, 2, 3, 4);
    sink = do_while_sum(10);

    return 0;
}
