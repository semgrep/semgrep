void g() {
  std::ifstream file = source();
  int data;
  // Reads an integer from `file` into `data` using
  // std::basic_istream<CharT,Traits>::operator>>
  file >> data;
  // ruleid: test
  sink(data);
}
