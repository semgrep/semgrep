class Foo {
  _initialize() {
     copyProperties(this, {
      periodInputDisabled: false,

       // optional semicolon here
      inputPeriod: null
    });
  }
}
