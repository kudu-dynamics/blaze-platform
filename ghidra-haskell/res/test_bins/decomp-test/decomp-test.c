#include <stdio.h>
#include <stdlib.h>

struct test {
    int num;
    char c;
};

struct test test_field(int num, char c) {
    struct test s1;
    s1.num = num;
    s1.c = c;

    int lim = s1.num;
    for (int i = 0; i < lim; i++) {
        s1.num += i;
    }

    return s1;
}

void test_switch(int num) {
    switch (num) {
        case 1:
            printf("case 1\n");
            break;
        case 2:
            printf("case 2\n");
            break;
        case 3:
            printf("case 3\n");
            break;
        case 4:
            printf("case 4\n");
            break;
        case 5:
            printf("case 5\n");
            break;
        case 6:
            printf("case 6\n");
            break;
        case 7:
            printf("case 7\n");
            break;
        default:
            printf("default\n");
    }
}


void test_switch_2() {
    int x,y,z;
    scanf("%d", &x);
    scanf("%d", &y);
    scanf("%d", &z);

    switch((x - y) % z) {
        case 25:
            x += 25;
            y += 50;
            int w = x + (y % z);
            printf("%d", w);
            break;
        case 105:
            x *= 3;
            y /= 5;
            int q = (x * y);
            printf("%d", q);
            break;
        case 60:
            x /= 55;
            y -= 23;
            int t = (x * y) % z;
            printf("%d", t);
            break;
        case 1025:
            x /= y;
            y = z % x;
            printf("%d", y);
            break;
        case 5:
            y /= z;
            x = x % z;
            printf("%d", x);
            break;
        case 6:
            x /= z % y;
            printf("%d", x);
        case 11:
            y *= 5;
            x /= y;
            printf("%d", x % z);
        default:
            printf("ya mom\n");

    }
}

int general_test_1() {
  int x = 0;
  int y = 0;

  int z = x + y;

  return 0;
}

int sum(int x, int y) {
    int res = x + y;
    return res;
}

int general_test_2() {
  int x = 0;
  int y = 5;
  int res = sum(x,y);
  return res;
}

void swap(int* xp, int* yp){
    int temp = *xp;
    *xp = *yp;
    *yp = temp;
}

void sort_array(int arr[25], int n) {
    int i, j;
    int swapped;
    for (i = 0; i < n - 1; i++) {
        swapped = 0;
        for (j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                swap(&arr[j], &arr[j + 1]);
                swapped = 1;
            }
        }
        
        if (swapped == 0)
            break;
    }
}

void progressive_sum(int arr[25], int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < i; j++) {
            arr[i] += j;
        }
    }
}

void printArray(int arr[], int size) {
    for (int i = 0; i < size; i++)
        printf("%d ", arr[i]);
}

void printString(char str[]) {
    printf("%s",str);
}

void func1(int arr[], int size) {
    for (int i = size - 1; i >= 0; i--) {
        for (int j = 0; j < i; j++) {
           arr[i] += arr[j]; 
        }
    }
}

void func2(int arr[], int size) {
    for (int i = size - 1; i >= 0; i--) {
        for (int j = 0; j < i; j++) {
           arr[i] *= arr[j]; 
        }
    }
}

void func3(int arr[], int size) {
    for (int i = size - 1; i >= 0; i--) {
        for (int j = 0; j < i; j++) {
           arr[i] -= arr[j]; 
        }
    }
}

int main() {
    int num;
    
    scanf("%d", &num);
    struct test s1 = test_field(num, 'M');
    printf("%d, %c\n", s1.num, s1.c);

    scanf("%d", &num);
    test_switch(num);

    num = general_test_1();
    printf("%d", num);
    
    num = general_test_2();
    printf("%d", num);

    const int prime_arr_len = 25;
    int prime_arr[prime_arr_len] = { 43, 61, 17, 2, 97, 47, 13, 71, 23, 79, 41, 7, 53, 29, 73, 5, 59, 19, 67, 89, 31, 11, 83, 37, 3 };
    sort_array(prime_arr, prime_arr_len);
    printArray(prime_arr, prime_arr_len);

    int magic_num = 123234;
    const int dyn_arr_len = 35;
    int *dyn_arr = malloc(dyn_arr_len * sizeof(int));
    for (int i = 0; i < dyn_arr_len; i++) {
        dyn_arr[i] = i * i * magic_num;
    }

    // test to see if ghidra understands that the array contains integers
    for (int i = prime_arr_len - 1; i >= 0; i--) {
        for (int j = 0; j < i; j++) {
           prime_arr[i] += prime_arr[j]; 
        }
    }

    void (*fptr)(int *, int);

    if (prime_arr[0] < 0) {
        fptr = &func1;
    } else if (prime_arr[0] == 0) {
        fptr = &func2;
    } else {
        fptr = &func3;
    }

    fptr(prime_arr, prime_arr_len);
    printArray(prime_arr,prime_arr_len);

    

    sort_array(dyn_arr,dyn_arr_len);
    printArray(dyn_arr,dyn_arr_len);

    //const int str_len = 32 + 1;
    char str[] = "What's up my dudes and dudettes!";
    printString(str);

    return 0;

}

