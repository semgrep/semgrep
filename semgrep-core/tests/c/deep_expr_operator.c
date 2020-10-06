// from: https://semgrep.dev/s/boKP/

#include <stdio.h>
#include <stdlib.h>

typedef struct name {
  char *myname;
  void (*func)(char *str);
} NAME;

void myprint(char *str) { printf("%s\n", str); }
void printmyname() { printf("call print my name\n"); }

int main() {

  NAME *a;
  //ERROR: match
  a = (NAME *) malloc(sizeof(struct name));
  a->func = myprint;
  a->myname = "I can also use it";
  a->func("this is my function");

  // free without modify

  free(a);
  a->func("I can also use it");

  // free with modify

  a->func = printmyname;
  a->func("this is my function");

  // set NULL

  a = NULL;
  printf("this pogram will crash...\n");
  a->func("can not be printed...");

}
