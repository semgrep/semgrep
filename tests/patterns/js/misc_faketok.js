//ERROR: match
var express = require('express');

class Foo extends Bar {
  constructor() {
    //ERROR: match
    normal_function(x);
    //ERROR: match
    super(x);
  }
}
