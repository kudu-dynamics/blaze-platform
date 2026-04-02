// Heap buffer overflow via attacker-controlled length truncation.
//
// flint-shell workflow:
//   taint-add custom_read src:0 ret
//   sample sample_me
//   prim-def TaintedHeapAllocThenCopy { * ; alloc: def p = call malloc(sz[tainted:global]) ; * ; copy: call memcpy(@p, src, len) }
//   check-wmi TaintedHeapAllocThenCopy 0..1

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

uint8_t g_input[1 << 20];

__attribute__((noinline))
uint32_t custom_read(const uint8_t *buf) {
    return *(const uint32_t *)buf;
}

__attribute__((noinline))
void *sample_me(void) {
    uint32_t packet_len = custom_read(g_input);
    uint16_t alloc_len = (uint16_t)packet_len;

    uint8_t *buf = (uint8_t *)malloc(alloc_len);
    if (!buf) {
        return NULL;
    }

    memcpy(buf, g_input + 4, packet_len);
    return buf;
}

int main(void) {
    void *r = sample_me();
    free(r);
    return 0;
}
