myArray = tainted;
myArray.forEach((x) => {
  foobar();
  //ruleid: test
  sink(x);
});
