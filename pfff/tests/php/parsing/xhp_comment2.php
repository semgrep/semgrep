<?php

// before people were doing that
$o = <div>{ 1 /* comment */ }</div>;

// Only single-line, doc, block comments work outside XHP
$xhp =
  <div>
    <!--XML comments only work within XHP children contexts-->
  </div>;


$n2 =
  <node>
    {'a'}
    <!--hey-->
    <!--
      multiline comment
      with multiple lines
    -->
    {'b'}
    <node>
      {'c'}
      <!--nested comment-->
    </node>
  </node>;


$foo = <blah> <!-- comment-with some -- extra das---hes --> </blah>;
