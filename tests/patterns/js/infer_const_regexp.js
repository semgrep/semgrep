// Constant propagation should work with regexp literals just like string
// literals.
//
// Note that 'new RegExp(/a/)' is equivalent to 'new Regexp(new RegExp("a"))',
// which is semantically equivalent to 'new RegExp("a")' or '/a/'.
//
function b() {
  const exp = /a/;
  // ERROR:
  const reg = new RegExp(exp);
}
