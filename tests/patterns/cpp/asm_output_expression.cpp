void assembly(int *output, int input) {
  //ERROR: match
  __asm __volatile__ ("" : "=r" (*output));
  __asm __volatile__ ("" : "=r" (input));
}
