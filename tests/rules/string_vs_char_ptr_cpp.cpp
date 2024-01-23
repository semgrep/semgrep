using namespace std;

void f() {
  // This is an `std::string`
  string f;
  // ruleid: string-type
  int x = f.length();

  // `s` has type `const char *`
  const char *s;
  // The string literal has type `char const [4]`
  // ok: string-type
  s = "foo";
}
