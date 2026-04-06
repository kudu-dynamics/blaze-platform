#include <stdio.h>
#include <string.h>

/* Test 1: Nested for loops (primary target) */
void nested_for() {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            printf("i: %d j: %d\n", i, j);
        }
        printf("i: %d\n", i);
    }
}

/* Test 2: Bubble sort - nested for + if + swap */
void bubble_sort(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int tmp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = tmp;
            }
        }
    }
}

/* Test 3: While loop with break/continue */
int find_first_negative(int arr[], int n) {
    int i = 0;
    while (i < n) {
        if (arr[i] < 0) return arr[i];
        if (arr[i] == 0) { i++; continue; }
        printf("positive: %d\n", arr[i]);
        i++;
    }
    return 0;
}

/* Test 4: If/else-if/else chains */
int classify(int x) {
    if (x > 100) return 3;
    else if (x > 50) return 2;
    else if (x > 0) return 1;
    else return 0;
}

/* Test 5: Switch statement */
void test_switch(int num) {
    switch (num) {
        case 1: printf("one\n"); break;
        case 2: printf("two\n"); break;
        case 3: printf("three\n"); break;
        default: printf("other\n");
    }
}

/* Test 6: Mixed for + switch (stress test) */
int process(char *buf, int len) {
    int sum = 0;
    for (int i = 0; i < len; i++) {
        switch (buf[i]) {
            case 'a': sum += i; break;
            case 'b': sum *= 2; break;
            default:
                if (buf[i] > 'z') return -1;
                sum += buf[i];
        }
    }
    return sum;
}

int main() {
    nested_for();

    int arr[] = {5, 3, 8, 1, 9, 2, 7};
    bubble_sort(arr, 7);

    int arr2[] = {3, 0, -1, 5, -7, 0, 2};
    int neg = find_first_negative(arr2, 7);
    printf("first negative: %d\n", neg);

    printf("classify 150: %d\n", classify(150));
    printf("classify 75: %d\n", classify(75));
    printf("classify 25: %d\n", classify(25));
    printf("classify -5: %d\n", classify(-5));

    test_switch(2);

    char buf[] = "abcxyz";
    int result = process(buf, strlen(buf));
    printf("process result: %d\n", result);

    return 0;
}
