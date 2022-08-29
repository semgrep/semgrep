// fnptr_impl.h
// C implementation of 'Fnptr' module.

#ifndef UTILS_FNPTR_IMPL_H
#define UTILS_FNPTR_IMPL_H

#define CAML_NAME_SPACE                // Only declare names starting with "caml".
#include <caml/mlvalues.h>             // value

// Mark a function as 'extern "C"' in C++ so its name does not get
// mangled, and hence is callable from OCaml after an "external"
// declaration.  I would have thought that CAMLprim would include this,
// but it does not.
//
// I do not wrap all function declarations in this module with extern
// "C" because, if we want to compile the whole thing as C++, it is
// reasonable to let the other names get mangled for slightly improved
// type safety.
#ifndef EXTERN_C
  #ifdef __cplusplus
    #define EXTERN_C extern "C"
  #else
    #define EXTERN_C extern
  #endif
#endif

// Type of a function pointer suitable for calling from OCaml.
typedef value (*fnptr_t)(value, void *extra);

// Wrap C function pointer 'fp' and its 'extra' as an OCaml value.
//
// This calls the OCaml allocator, so may invoke the GC.
value Val_fnptr(fnptr_t fp, void *extra);

// Given the result of 'Val_fnptr', extract the 'extra'.
void *get_fnptr_extra(value fpval);

// Zero out 'fpval' to indicate that it is no longer valid.  This should
// be done before whatever its 'extra' points to is deallocated, since
// the OCaml code can hold onto 'fpval' indefinitely.
void invalidate_fnptr(value fpval);

// Invoke 'fpval' (created by 'Val_fnptr') on 'arg', returning whatever
// it returns.
//
// This is meant to be invoked from the OCaml side.  It is declared in
// this header only for completeness.
//
// This throws an Invalid_argument exception if 'fpval' is invalid.
EXTERN_C CAMLprim value call_fnptr(value fpval, value arg);

#endif // UTILS_FNPTR_IMPL_H
