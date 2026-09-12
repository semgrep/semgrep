// This is a bug and a nice example of a MISSING node introduced by
// tree-sitter.
void hello() {
  return true and true;
}
