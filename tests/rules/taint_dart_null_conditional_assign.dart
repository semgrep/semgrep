class Box {
  String? value;
  String? other;
}

// `b?.value = ...` writes through `b` (null-conditional assignment).
void write(Box b) {
  b?.value = source();
  // ruleid: taint
  sink(b.value);
}

void otherField(Box b) {
  b?.value = source();
  // ok: taint
  sink(b.other);
}
