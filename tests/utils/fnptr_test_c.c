// fnptr_test_c.c
// Test of 'Fnptr' module, C side.

// See Fnptr_test_ml.ml for descriptions of what the exported (not
// 'static') functions do.

#include "fnptr_impl.h"                // module under test

#define CAML_NAME_SPACE                // Only declare names starting with "caml".
#include <caml/memory.h>               // CAMLparam*, CAMLlocal*, CAMLreturn
#include <caml/mlvalues.h>             // value, etc.

#include <assert.h>                    // assert


static value add5(value arg, void *extra)
{
  CAMLparam1(arg);
  CAMLlocal1(ret);

  assert(Is_long(arg));
  int a = Int_val(arg);

  ret = Val_int(a + 5);

  CAMLreturn(ret);
}


EXTERN_C CAMLprim value get_add5(value unit)
{
  CAMLparam1(unit);
  CAMLlocal1(ret);

  ret = Val_fnptr(add5, NULL);

  CAMLreturn(ret);
}


typedef struct AddNExtra {
  // Amount to add.
  int m_n;
} AddNExtra;


static value addN(value arg, void *extra)
{
  CAMLparam1(arg);
  CAMLlocal1(ret);

  AddNExtra *ane = (AddNExtra*)extra;

  assert(Is_long(arg));
  int a = Int_val(arg);

  ret = Val_int(a + ane->m_n);

  CAMLreturn(ret);
}


EXTERN_C CAMLprim value get_addN(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(ret);

  assert(Is_long(arg));
  int n = Int_val(arg);

  AddNExtra *ane = (AddNExtra*)malloc(sizeof(AddNExtra));
  ane->m_n = n;

  ret = Val_fnptr(addN, ane);

  CAMLreturn(ret);
}


EXTERN_C CAMLprim value dispose_addN(value arg)
{
  CAMLparam1(arg);
  CAMLlocal1(ret);

  assert(Is_block(arg));

  void *extra = get_fnptr_extra(arg);
  assert(extra);
  free(extra);

  invalidate_fnptr(arg);

  ret = Val_unit;

  CAMLreturn(ret);
}


// EOF
