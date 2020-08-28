#include "caml/mlvalues.h"

CAMLextern void caml_print_exception_backtrace(void);

CAMLprim value print_exception_backtrace_stub(value /*__unused*/ unit)
{
  caml_print_exception_backtrace();
  return Val_unit;
}
