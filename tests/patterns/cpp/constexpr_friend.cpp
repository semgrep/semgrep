//ERROR: match
namespace [[gnu::visibility("default")]] selected {
struct Number {
  constexpr friend bool operator==(Number, Number) { return true; }
};
}
namespace [[gnu::visibility("default")]] ignored {
struct Other {
  friend bool operator==(Other, Other) { return true; }
};
}
