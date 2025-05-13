#include <stdio.h>

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

}