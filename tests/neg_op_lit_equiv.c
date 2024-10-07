#include <limits.h>
#include <stdlib.h>

void func_one(int i_a) {
  //ruleid: c-string-equality
  int result = -i_a;
}

void func_three(int i_a) {
  int result;
  if (i_a == INT_MIN) {
    exit(EXIT_FAILURE);
  } else {
    //ok: c-string-equality
    result = -i_a;
  }
}

void func_four(int i_a) {
  int result;
  if (i_a == -123123) {
    exit(EXIT_FAILURE);
  } else {
    //ok: c-string-equality
    result = -i_a;
  }
}
