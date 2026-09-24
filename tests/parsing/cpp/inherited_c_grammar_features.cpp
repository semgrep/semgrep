void static_array(int values[static const 4]);

int * _Nonnull nonnull_pointer;

alignas(16) int aligned_value;

void labeled_declaration() {
label:
  int value = 1;
}

int extension_expression() {
  return __extension__ (1 + 2);
}

void attributed_parameter(int value [[maybe_unused]]);

void extended_asm(int *output, int input) {
  __asm __volatile__ (R"(mov %1, %0)" : "=r" (*output) : "r" (input));
}
