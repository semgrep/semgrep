//ERROR: match
namespace [[gnu::visibility("default")]] selected {
  void annotated(int value __attribute((unused)));
}
namespace [[gnu::visibility("default")]] ignored {
  void unannotated(int value);
}
