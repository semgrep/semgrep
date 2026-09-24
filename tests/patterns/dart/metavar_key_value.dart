void foo() {
  // ERROR:
  var thisdict = {
    "key": "value",
    "key2": "value2",
    "key3": "value3",
  };

  // ERROR:
  var foo = {
    "foo": "bar",
  };
}
