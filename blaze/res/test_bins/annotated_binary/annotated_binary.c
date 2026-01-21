#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

double MAGIC_NUM = 555.555;

struct person {
    int age;
    float height;
    double secondsAlive; // idk lol
    bool isLeftHanded;
    char firstLetterOfName;
    int ssn[9];
    char* extraData;
};

typedef struct person Person;

// this function has no meaning, just wrote it to test stuff
double calculateSecretAttribute (Person *p) {
    double temp[9] = {0.0};

    for (int i = 0; i < 9; i++) {
        temp[i] = ((p->ssn[i] * p->age) + p->height) / p->secondsAlive;
    }

    if (p->isLeftHanded && p->firstLetterOfName == 'C') {
        temp[0] *= MAGIC_NUM;
    } else {
        temp[0] *= 2;
    }

    double res = 0.0;

    for (int i = 0; i < 9; i++) {
        res += temp[i];
    }

    MAGIC_NUM += 1;

    return res;

}

int calculateNumberOfChars(char string[50], int strLen, char c) {
    int count = 0;
    for (int i = 0; i < strLen; i++) {
        if (string[i] == c) {
            count++;
        }
    }
    return count;
} 

int main() {
    char *data = (char *) malloc(20);
    for (int i = 0; i < 20; i++) {
        data[i] = 'a';
    }

    Person person = {25, 5.11, 93234.23423, false, 'Z', {1,2,3,4,5,6,7,8,9}, data};
    double secretAttribute = calculateSecretAttribute(&person);
    printf("Magic num: %f\n", MAGIC_NUM);
    printf("Secret attribute: %f\n", secretAttribute);

    char testStr[50] = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwx";
    int count = calculateNumberOfChars(testStr, 50, 'a');
    printf("Count num: %d\n", count);
    printf("Magic num: %f\n", MAGIC_NUM);

}
