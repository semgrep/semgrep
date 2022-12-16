<?php
// Copyright 2004-present Facebook. All Rights Reserved.

function foo() {
  $x = array(
    'foo' => 'bar',
    3 => 4,
    5 => 6,
    'aaaaaa' => 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
  );

  $html =
    <div>
      <fbt secret="SI">foo</fbt>
      {$child1 = <x:frag />}
    </div>;

  if (true) {
    $child1->appendChild(<h1>a</h1>);
  }
}

some_function(
  $a,
  $b,
  $cccccccccccccccccccccccccccccccccccccccccccccccccccccccccc,
  array(
    'foo' => 'bar',
    3 => 4,
    5 => 6,
    'aaaaaa' => 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
  )
);

$x = <div>fadsfd</div>;

$x =
  <div
    a="aaaaaaaaaaaaaa"
    b="bbbbbbbbbbbbbbbbbbbbb"
    z="ccccccccccccccc"
    d="ddddddddddddddd"
  >
    foo
  </div>;
