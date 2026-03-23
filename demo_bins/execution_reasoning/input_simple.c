#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUF_SIZE 256
#define NAME_SIZE 32

typedef struct {
    char name[NAME_SIZE];
    int active;
} Session;

__attribute__((noinline))
void handle_request(Session *sess, const char *input, size_t len) {
    // CWE-120: buffer overflow — len is attacker-controlled, name is only 32 bytes
    memcpy(sess->name, input, len);
    sess->name[NAME_SIZE - 1] = '\0';
}

__attribute__((noinline))
void event_handler(int fd) {
    char buf[BUF_SIZE];
    Session sess;
    memset(&sess, 0, sizeof(sess));
    sess.active = 1;

    ssize_t n = read(fd, buf, BUF_SIZE - 1);
    if (n <= 0) return;
    buf[n] = '\0';

    handle_request(&sess, buf, (size_t)n);
}

int main(void) {
    event_handler(0);  // stdin as input
    return 0;
}
