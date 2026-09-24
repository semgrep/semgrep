//ERROR: match
namespace [[gnu::visibility("default")]] selected {
  void expressions(bool flag) {
    int value = flag ? first(), selected() : fallback();
  }
}
namespace [[gnu::visibility("default")]] ignored {
  void expressions(bool flag) {
    int value = flag ? first(), ignored() : fallback();
  }
}
