void clobbers(int *out, int value) {
  __asm__ __volatile__(R"(op)" : "=r" (out[1]) : "r" (value) : R"(memory)", "c" "c");
}
