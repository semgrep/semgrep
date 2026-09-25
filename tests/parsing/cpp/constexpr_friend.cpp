struct Number {
  constexpr friend bool operator==(Number, Number) {
    return true;
  }
};
