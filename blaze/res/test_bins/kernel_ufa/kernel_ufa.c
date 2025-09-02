#include <stdlib.h>

struct Wrapper {
    void * ptr;
};

void destroy(struct Wrapper w) {
    void *x = w.ptr;
    free(x);
    w.ptr = NULL;
}

int main() {
    struct Wrapper wrapper;
    wrapper.ptr = malloc(5);
    destroy(wrapper);

    return 0;
}





