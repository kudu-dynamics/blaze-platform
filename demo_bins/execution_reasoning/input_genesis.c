// input_genesis.c - Test binary for Path Genesis from Input Nodes (Task 4)
//
// Simulates a network service where recv() provides attacker-controlled data
// that flows through a dispatcher into multiple event handlers. Each handler
// demonstrates a different vulnerability class reachable from the input node.
//
// Build: gcc -g -O0 -fno-stack-protector -no-pie -U_FORTIFY_SOURCE -z norelro -o input_genesis input_genesis.c
// (or just: make)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define BUF_SIZE 256
#define NAME_SIZE 32

// ---------------------------------------------------------------------------
// Global state (simulates server state)
// ---------------------------------------------------------------------------

char g_log_buffer[64];
char g_config[128];
void *g_session = NULL;

typedef struct {
    int active;
    char username[NAME_SIZE];
    void (*cleanup)(void *);
} Session;

// ---------------------------------------------------------------------------
// Input sources - these are the "system input nodes" that Task 4 targets.
// In a real binary, recv/read provide attacker-controlled data.
// ---------------------------------------------------------------------------

// Wrapper so the symbol is clearly visible. In a real binary this would be
// the actual libc recv(). We simulate it with read() from stdin.
__attribute__((noinline))
ssize_t net_recv(int fd, void *buf, size_t len, int flags) {
    (void)flags;
    return read(fd, buf, len);
}

// A second input source: reads a "config" from a file descriptor.
// Demonstrates that multiple input sources can exist.
__attribute__((noinline))
ssize_t config_read(int fd, void *buf, size_t len) {
    return read(fd, buf, len);
}

// ---------------------------------------------------------------------------
// Event handlers - each processes input differently and has a distinct vuln
// ---------------------------------------------------------------------------

// Handler 1: Controlled format string
// recv'd data used directly as printf format string
__attribute__((noinline))
void handle_log(const char *input) {
    sprintf(g_log_buffer, input);  // CWE-134: format string
    printf("Logged: %s\n", g_log_buffer);
}

// Handler 2: Unbounded copy (heap overflow)
// recv'd data copied to heap buffer without length check
__attribute__((noinline))
void handle_update_name(Session *sess, const char *input, size_t len) {
    memcpy(sess->username, input, len);  // CWE-120: buffer overflow
    sess->username[NAME_SIZE - 1] = '\0';
}

// Handler 3: Use-after-free / dangling pointer
// Frees session then accesses it
__attribute__((noinline))
void handle_disconnect(Session *sess) {
    free(sess);
    // Bug: still references sess after free
    printf("Disconnected user: %s\n", sess->username);  // CWE-416: use after free
}

// Handler 4: Double free via two code paths
// "User logout" frees session, "admin purge" frees it again
__attribute__((noinline))
void handle_logout(Session *sess) {
    if (sess->cleanup) {
        sess->cleanup(sess);
    }
    free(sess);  // first free
}

__attribute__((noinline))
void handle_admin_purge(Session *sess) {
    free(sess);  // CWE-415: double free if handle_logout already ran
}

// Handler 5: Controlled indirect call
// Attacker-provided data overwrites a function pointer
__attribute__((noinline))
void handle_set_callback(Session *sess, const char *input) {
    void (*fp)(void *);
    memcpy(&fp, input, sizeof(fp));  // Copy controlled data into func ptr
    sess->cleanup = fp;  // CWE-427: controlled indirect call target
}

// Handler 6: Copy to global (write-what-where primitive)
// Input data written to a global buffer at controlled offset
__attribute__((noinline))
void handle_config_update(const char *input, size_t offset, size_t len) {
    if (offset + len <= sizeof(g_config)) {
        memcpy(g_config + offset, input, len);  // write to global
    }
}

// ---------------------------------------------------------------------------
// Dispatcher - routes input to the appropriate handler
// ---------------------------------------------------------------------------

#define CMD_LOG         1
#define CMD_UPDATE_NAME 2
#define CMD_DISCONNECT  3
#define CMD_LOGOUT      4
#define CMD_ADMIN_PURGE 5
#define CMD_SET_CALLBACK 6
#define CMD_CONFIG      7

typedef struct {
    int cmd;
    size_t offset;
    size_t len;
} Header;

__attribute__((noinline))
void dispatch(Session *sess, int cmd, const char *payload, size_t offset, size_t len) {
    switch (cmd) {
        case CMD_LOG:
            handle_log(payload);
            break;
        case CMD_UPDATE_NAME:
            handle_update_name(sess, payload, len);
            break;
        case CMD_DISCONNECT:
            handle_disconnect(sess);
            break;
        case CMD_LOGOUT:
            handle_logout(sess);
            break;
        case CMD_ADMIN_PURGE:
            handle_admin_purge(sess);
            break;
        case CMD_SET_CALLBACK:
            handle_set_callback(sess, payload);
            break;
        case CMD_CONFIG:
            handle_config_update(payload, offset, len);
            break;
    }
}

// ---------------------------------------------------------------------------
// Event loop - reads from network input, parses, dispatches
// ---------------------------------------------------------------------------

__attribute__((noinline))
void event_loop(int sockfd) {
    char buf[BUF_SIZE];
    Session *sess = (Session *)malloc(sizeof(Session));
    memset(sess, 0, sizeof(Session));
    sess->active = 1;
    strcpy(sess->username, "anonymous");

    while (sess->active) {
        // Read a command header
        Header hdr;
        ssize_t n = net_recv(sockfd, &hdr, sizeof(hdr), 0);
        if (n <= 0) break;

        // Read the payload
        size_t payload_len = hdr.len;
        if (payload_len > BUF_SIZE - 1) payload_len = BUF_SIZE - 1;

        n = net_recv(sockfd, buf, payload_len, 0);
        if (n <= 0) break;
        buf[n] = '\0';

        dispatch(sess, hdr.cmd, buf, hdr.offset, (size_t)n);
    }
}

// ---------------------------------------------------------------------------
// Secondary event loop - reads config updates from a different input source
// Demonstrates multiple genesis points
// ---------------------------------------------------------------------------

__attribute__((noinline))
void config_loop(int config_fd) {
    char cbuf[128];
    while (1) {
        ssize_t n = config_read(config_fd, cbuf, sizeof(cbuf) - 1);
        if (n <= 0) break;
        cbuf[n] = '\0';

        // Config data used as format string (another path to same vuln class)
        handle_log(cbuf);

        // Config data also written to global
        handle_config_update(cbuf, 0, (size_t)n);
    }
}

// ---------------------------------------------------------------------------
// Main - sets up the "server"
// ---------------------------------------------------------------------------

int main(int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    // In a real server these would be socket fds
    int sockfd = 0;     // stdin simulates network socket
    int config_fd = 0;  // same fd for simplicity

    printf("Input Genesis Test Server\n");

    // Primary event loop: network input -> dispatch -> handlers
    event_loop(sockfd);

    // Secondary loop: config input -> handlers
    config_loop(config_fd);

    return 0;
}
