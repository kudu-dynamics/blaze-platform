#include <stdio.h>
#include <stdlib.h>

int main() {
  char answer1;
  char guess[20];
  int guess_num;

  puts("Are you sure you want to be a loser? ");
  answer1 = getchar();
  getchar(); // get enter

  if (answer1 == 'y') {
    puts("Try to guess what number I'm thinking of: ");

    fgets(guess, 20, stdin);
    guess_num = atoi(guess);

    if (guess_num == 1337) {
      puts("Dang, you got it.\n");
    }
    else {
      printf("You guessed %d? Really? Totally wrong.\n", guess_num);
    }
  }
  else {
    puts("Good for you.");
  }

  return 0;
}
