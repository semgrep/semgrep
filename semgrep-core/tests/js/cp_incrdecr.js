// https://github.com/returntocorp/semgrep/issues/4667

function test1(event) {
  var newModelsCnt = 0;
  //ERROR:
  if (newModelsCnt == 0) {
    return;
  }
}

function test2(event) {
  var newModelsCnt = 0;
  newModelsCnt++;
  //OK:
  if (newModelsCnt == 0) {
    return;
  }
}

function test3(event) {
  var newModelsCnt = 0;
  ++newModelsCnt;
  //OK:
  if (newModelsCnt == 0) {
    return;
  }
}

function test4(event) {
  var newModelsCnt = 0;
  newModelsCnt--;
  //OK:
  if (newModelsCnt == 0) {
    return;
  }
}

function test5(event) {
  var newModelsCnt = 0;
  --newModelsCnt;
  //OK:
  if (newModelsCnt == 0) {
    return;
  }
}