<?hh

foo('callback1', 1);
foo('A::callback2', 1);
foo(array('A', 'callback3'), 1);
foo(array($this, 'callback4'), 1);
foo(array($object, 'callback5'), 1);

class X {
  public function foo() {
    foo(array(__CLASS__, 'callback6'), 1);
  }
}

function foo() {
  map(__FUNCTION__, array(1,2));
  map(__function__, array(1,2));
}

function foo ((function(int): ?DataObject) $x) {

}

function foo (?(function(int): ?DataObject) $x = null) {

}

class X {
  private (function(): string) $meh = null;

  private ?(function(): string) $meh2 = null;
}
