// ERROR:
function foo(x: number) {
  let y = x + 1;
  return x;
}

// TODO: those are parsed as TodoK "TypeAssert" for now
function foo(x: number) {
  let y = x + 1;
  return <string>x;
}
