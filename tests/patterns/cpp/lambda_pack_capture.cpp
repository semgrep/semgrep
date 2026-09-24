template <typename... Args>
void captures(Args... args) {
  //ERROR: match
  auto copied = [...values = args]() { return 42; };
  //ERROR: match
  auto referenced = [&...values = args]() { return 42; };
  auto negative = [...values = args]() { return 0; };
}
