#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

const int PAYLOAD_SIZE = 4;

struct Node {
    int payload;
    struct Node* next;
};


void add_to_linked_list(int load, struct Node *node) {
    if (node->next != NULL) {
        return;
    }

    struct Node nextNode = {load, NULL};
    node->next = &nextNode;
}

void print_linked_list (struct Node *node) {
    struct Node *currNode = node;

    while (currNode != NULL) {
        printf("%d\n", node->payload);
        currNode = currNode->next;
    }
}

int main() {
    struct Node node = {1, NULL};
    add_to_linked_list(2, &node);
    add_to_linked_list(3, &node);
    print_linked_list(&node);
}
