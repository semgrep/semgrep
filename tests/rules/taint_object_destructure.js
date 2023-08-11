const foo = ({ x, y }, z) => {
  return (
    // todo: taint-object-destructure
    // no longer works because of https://github.com/returntocorp/semgrep/pull/8442
    sink(x),
    // todo: taint-object-destructure
    // no longer works because of https://github.com/returntocorp/semgrep/pull/8442
    sink(y),
    // ok: taint-object-destructure
    sink(z)
  );
};