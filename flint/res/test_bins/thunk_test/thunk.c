#include <stdio.h>
#include <stdlib.h>

int main() {
    int *ptr = (int *)malloc(20);
    
    // Populate the array
    for (int i = 0; i < 5; i++)
        ptr[i] = i + 1;
        
    // Print the array
    for (int i = 0; i < 5; i++)
        printf("%d ", ptr[i]);
    return 0;
}
