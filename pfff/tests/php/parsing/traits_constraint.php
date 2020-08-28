<?php

class A {
}

interface I {
}

trait T {
  require extends A;
  require implements I;
}


class B extends A implements I {
  use T;
}
