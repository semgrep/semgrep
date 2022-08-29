// fnptr_impl.c
// Code for fnptr_impl.h.

#include "fnptr_impl.h"                // this module

#define CAML_NAME_SPACE                // Only declare names starting with "caml".
#include <caml/alloc.h>                // caml_alloc_small
#include <caml/fail.h>                 // caml_failwith, caml_raise_out_of_memory
#include <caml/memory.h>               // CAMLparam*, CAMLlocal*, CAMLreturn
#include <caml/mlvalues.h>             // value, etc.

#include <assert.h>                    // assert


// A random number, used in assertions to identify 'fnptr' values.
enum { FNPTR_MAGIC = 263264791 };


// The implementation here is based on an answer at:
// https://stackoverflow.com/questions/61537623/is-it-posible-to-pass-a-c-function-as-callback-to-ocaml
//
// It uses an "Abstract" block, which means its contents are not scanned
// by the GC, which is appropriate since the elements are not OCaml
// values.
//
// However, Abstract is somewhat limited, as it does not allow attaching
// code to handle comparison or finalization.  If/when we need that,
// this should use Custom instead.
value Val_fnptr(fnptr_t fp, void *extra)
{
  value fpval = caml_alloc_small(3, Abstract_tag);
  if (!fpval) {
    caml_raise_out_of_memory();
  }

  Field(fpval, 0) = (value)fp;
  Field(fpval, 1) = (value)extra;
  Field(fpval, 2) = (value)FNPTR_MAGIC;

  return fpval;
}


void invalidate_fnptr(value fpval)
{
  assert(Is_block(fpval));
  assert(Tag_val(fpval) == Abstract_tag);

  Field(fpval, 0) = (value)0;
  Field(fpval, 1) = (value)0;
  Field(fpval, 2) = (value)0;
}


void *get_fnptr_extra(value fpval)
{
  assert(Is_block(fpval));
  assert(Tag_val(fpval) == Abstract_tag);

  return (void*)Field(fpval, 1);
}


EXTERN_C CAMLprim value call_fnptr(value fpval, value arg)
{
  CAMLparam2(fpval, arg);
  CAMLlocal1(ret);

  assert(Is_block(fpval));
  assert(Tag_val(fpval) == Abstract_tag);

  fnptr_t fp    = (fnptr_t)Field(fpval, 0);
  void*   extra = (void*)  Field(fpval, 1);
  int     magic = (int)    Field(fpval, 2);

  if (magic == 0) {
    caml_invalid_argument("Invalid fnptr invoked.");
  }

  // A 0 magic is invalid but "expected" in the sense that it can happen
  // without violating the OCaml type system.  Any other value (besides
  // FNPTR_MAGIC) would mean the OCaml type system was subverted
  // somehow.
  assert(magic == FNPTR_MAGIC);

  ret = (*fp)(arg, extra);

  CAMLreturn(ret);
}


// EOF
