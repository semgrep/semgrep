function test1() {
  var config = taint;
  config = ok;
  //ok: test
  sink(config);
}

function test2() {
  var config = [taint];
  config = [process.env.FOO];
  //ok: test
  sink(config);
}

function test3() {
  var config = {
    foo: taint
  };
  config = {
    foo: process.env.FOO
  };
  //ok: test
  sink(config);
}
