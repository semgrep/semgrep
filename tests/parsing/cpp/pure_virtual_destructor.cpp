struct Interface {
  virtual ~Interface() = 0;
};
struct Convertible { virtual operator bool() const = 0; };
