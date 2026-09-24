template <typename... Args>
void captures(Args... args) {
  auto copied = [args...] { return 1; };
  auto forwarded = [...values = args] { return 2; };
  auto referenced = [&...values = args] { return 3; };
}
struct Member {
  void captures(int arg) {
    auto copied = [=, *this] { return arg; };
    auto referenced = [this, &arg] { return arg; };
  }
};
