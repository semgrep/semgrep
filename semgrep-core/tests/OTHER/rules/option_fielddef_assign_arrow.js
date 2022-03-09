class TestClas {
  test() {
    //ruleid:test
    const a = () => { return; };
    //ruleid:test
    const b = () => ({});
    //ruleid:test
    const c = () => false;
    return;
  }

  // arrow function
  //ruleid:test
  test1 = () => {
    return;
  }

  // private arrow function
  //ruleid:test
  #test2 = () => {
    return;
  }

  // arrow function w/o body
  //ruleid:test
  test3 = () => 1;

  // regular function w/o body
  // Not here!
  test4 = function () { return; }
}
