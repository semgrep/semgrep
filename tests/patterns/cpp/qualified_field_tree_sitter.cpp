//ERROR: match
namespace [[gnu::visibility("default")]] selected {
  void call(Derived object) { object.Base::method(); }
}
namespace [[gnu::visibility("default")]] unqualified {
  void call(Derived object) { object.method(); }
}
namespace [[gnu::visibility("default")]] other_method {
  void call(Derived object) { object.Base::other(); }
}
