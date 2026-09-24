void range_for_initializer() {
  for (int initializer; int value : {1}) {
    (void)value;
  }
}
