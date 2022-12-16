// callbacks
#include <stdlib.h>
#include <stdio.h>

int global;

// Where this function can be called?
int proc_error() {
  global = 42;
  int v = 1;
  return v;
}

int (*error)();


void foo() {
  // proc_error can be called here, because of main() assignment
  //int x = (*error)();
  //actually there is sugar here, one can do too:
  int x = error();
}

int main() {
  error = &proc_error;
  foo();
  int v = global;
  printf("%d\n", global);
}
