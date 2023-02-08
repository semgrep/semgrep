// ERROR:
function foo(x: number) {
  let y = x + 1;
  return x;
}

// ERROR: 
function foo(x: number) {
  let y = x + 1;
  return <string>x;
}
