<?php

//-----------------------------------------------------------------------------
// Simple type inference
//-----------------------------------------------------------------------------

// should infer: number -> number
function t1($i) {
  return $i + 1;
}

//-----------------------------------------------------------------------------
// Type inference with union types and variants
//-----------------------------------------------------------------------------

// :: number -> number|null
function t2($i) {
  if ($i > 0) {
    return $i + 1;
  } else {
    return null;
  }
}

// :: number -> mixed
function t3($i) {
  if ($i > 0) {
    return $i + 1;
  } else {
    return array('foo');
  }
}


// :: array -> mixed
function t4($x) {
  return $x['fld1'];
}

//-----------------------------------------------------------------------------
// Polymorphic types
//-----------------------------------------------------------------------------

// :: array('foo' => <A>) -> <A>)

function t5($x) {
  return $x['foo'];
}


//-----------------------------------------------------------------------------
// Interprocedural bottom-up type inference
//-----------------------------------------------------------------------------

function foo($x) {
  return "a string";
}

// :: <A> -> string
function t6($x) {
  return foo($x);
}

////-----------------------------------------------------------------------------
//// Type inference for methods and object field access
////-----------------------------------------------------------------------------
//
//// :: object(fields => $x :: string; methods => foo :: string -> string)
//class A {
//  public $x;
//  public function foo($s) { return $s . $x; }
//}
//
//// :: object(A) -> string
//function t7(A $o) {
//  return $o->foo("bar");
//}
