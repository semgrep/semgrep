void expressions(bool flag) {
  //ERROR: match
  int value = flag ? first(), selected() : fallback();
  int other = flag ? first(), ignored() : fallback();
}
