
fn main() {
  let file = tainted; 

  // ruleid: tainted-pattern-lval
  sink(file);

  let (file2, arg) = tainted;

  // todo: tainted-pattern-lval
  sink(file2);

  let file3 : ty = tainted;

  // ruleid: tainted-pattern-lval
  sink(file3)
}
