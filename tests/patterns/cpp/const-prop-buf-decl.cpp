/* from issue: 8037 */
void fun() {
  int fixed = 42;
  // ruleid: match_fixed_size_buffer
  #ERROR:
  char a[42];
  // ruleid: match_fixed_size_buffer
  #ERROR:
  char b[fixed];
  // ok: match_fixed_size_buffer
  char c[unknown];
}
