const foo = ({ x, y }, z) => {
  return (
    // ruleid: taint-object-destructure
    sink(x),
    // ruleid: taint-object-destructure
    sink(y),
    // ok: taint-object-destructure
    sink(z)
  );
};