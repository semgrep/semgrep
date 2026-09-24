//ERROR: match
namespace [[gnu::visibility("default")]] selected {
  void strings() { consume("prefix" R"(raw)" "suffix"); }
}
namespace [[gnu::visibility("default")]] ignored {
  void strings() { consume("prefix" R"(different)" "suffix"); }
}
