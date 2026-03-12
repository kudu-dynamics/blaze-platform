



#include <stdio.h>
#include <string.h>

#define MESSAGE_SIZE 1024
extern char message_buffer[MESSAGE_SIZE] __attribute__((weak));

void load_message_to_memory(void) {
    char code_segment[MESSAGE_SIZE];

    if (!message_buffer)
        goto out;

    char *message_separator  = (char *)memchr(message_buffer, '\0', MESSAGE_SIZE);
    
    if (message_separator == NULL) {
        printf("separator not found\n");
        goto out;
    }

    if (message_separator - message_buffer == MESSAGE_SIZE - 1) {
        printf("separator was at end\n");
        goto out;
    }

    char *code_start = message_separator + 1;
    char* code_end = message_buffer + MESSAGE_SIZE;
    size_t code_size = code_end - code_start;

    if (code_size > MESSAGE_SIZE) {
        printf("Code size exceeds buffer capacity\n");
        goto out;
    }

    memcpy(code_segment, code_start, code_size);
    for (int i = 0; i < code_size-2; i++) {
        if (code_segment[i] == 0x00 &&
             code_segment[i+1] == 0xff && 
              code_segment[i+2] == 0x00)
            goto out;
    }

    // TODO: Replicate the stack execution code at userland
    //      - we need to test out the detectStackExec primitive
    // make_dynamic_area();

out:
    return;
}

/*
void make_dynamic_area(void) {
    struct task_struct *task = current;
    unsigned long stack_start = (unsigned long)task_stack_page(task);
    unsigned long addy =((unsigned long int)stack_start + (PAGE_SIZE *3));

    pte_t* pte = virt_to_kpte(addy);
    if (!pte) goto out;

    if (!pte_exec(*pte)) {
        set_pte_at(current->mm, addy, pte, pte_mkexec(*pte));
    }

    out:
}
*/

int main() {
    load_message_to_memory();
    return 0;
}
