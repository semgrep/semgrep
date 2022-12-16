<?php

class Core { }

class A {
  //TODO? allows type hints for member variables apparently ... not
  // sure about the semantic
  protected /*Core*/ $x;
}
