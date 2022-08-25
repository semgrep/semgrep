// bridge_ml.c
// Bridge layer interfacing to OCaml.

#include "bridge_ml.h"                 // this module

#define CAML_INTERNALS                 // Required for caml_init_signals.
#define CAML_NAME_SPACE                // Only declare names starting with "caml".
#include <caml/alloc.h>                // caml_copy_string
#include <caml/callback.h>             // caml_callback
#include <caml/fail.h>                 // caml_raise_out_of_memory
#include <caml/memory.h>               // CAMLparam*, CAMLlocal*, CAMLreturn
#include <caml/mlvalues.h>             // value, etc.
#include <caml/signals.h>              // caml_init_signals

#include "fnptr_impl.h"                // Val_fnptr

#include <assert.h>                    // assert
#include <stdlib.h>                    // abort
#include <string.h>                    // memcpy, memset, strdup


// Pointer to value that holds the closure for the
// 'semgrep_cli_lib_main' function in Semgrep_bridge.ml.
static value const *semgrep_cli_lib_main_ptr = NULL;

// Pointer to value for 'exn_to_string' defined in Exn_to_string.ml.
static value const *exn_to_string_ptr = NULL;


// Just a random number.  The specific value is unimportant.
enum {
  RFCE_MAGIC = 475410507,
};


void bridge_ml_startup()
{
  // Check that we have not called this more than once.  According to
  // the OCaml docs, it is not possible to start the OCaml runtime a
  // second time, even after shutting it down, unless the entire shared
  // library containing it is completely unloaded and then reloaded.
  // Since I assume this module would be a part of such a library, doing
  // that should also reset this pointer to NULL, although I have not
  // tested that.
  assert(semgrep_cli_lib_main_ptr == NULL);

  // Initialize OCaml.  Use an empty 'argv'.  We may want to invoke
  // the analysis multiple times between startup and shutdown, and the
  // inputs should come from the 'argv' we pass with those calls.
  char *n = NULL;
  caml_startup(&n);

  // Tell OCaml to install its signal handler that detects stack
  // overflow and raises the Stack_overflow exception.  Without this,
  // the run-time just segfaults on overflow due to having no active
  // SIGSEGV handler.
  //
  // See https://github.com/ocaml/ocaml/issues/11486 .
  caml_init_signals();

  // Get the registered entry points.
  semgrep_cli_lib_main_ptr = caml_named_value("semgrep_cli_lib_main");

  // If this assertion fails, the registration in Semgrep_bridge.ml most
  // likely did not run, perhaps because something is wrong with the
  // build and it wasn't included in the shared library.
  assert(semgrep_cli_lib_main_ptr);

  exn_to_string_ptr = caml_named_value("exn_to_string");
  assert(exn_to_string_ptr);
}


void bridge_ml_shutdown()
{
  // Make sure it has been started.
  assert(semgrep_cli_lib_main_ptr != NULL);

  // Balance the init above.  This deactivates the SIGSEGV handler.
  caml_terminate_signals();

  caml_shutdown();
}


// The 'extra' for 'call_read_file'.
struct ReadFileCallbackExtra {
  // Magic number RFCE_MAGIC.
  int m_magic;

  // Underlying function pointer.
  ReadFileFunc m_read_file;

  // The 'extra' to pass to 'm_read_file'.
  void *m_extra;
};


// 'arg' is a string, and the return value is a (bytes, int) pair.
//
// That signature is defined by the type of the 'read_file_fnptr'
// parameter to 'semgrep_cli_lib_main' in Semgrep_bridge.ml.
static value call_read_file(value arg, void *extra)
{
  CAMLparam1(arg);
  CAMLlocal3(contentsVal, errorCodeVal, resultVal);

  assert(Is_block(arg));
  assert(Tag_val(arg) == String_tag);

  struct ReadFileCallbackExtra *rfce = (struct ReadFileCallbackExtra*)extra;
  assert(rfce->m_magic == RFCE_MAGIC);

  // This pointer is valid until next OCaml allocator call.
  char const *fname = String_val(arg);

  // Invoke the function pointer.  This will end up calling back into
  // the Python code.
  char *contentBytes = NULL;
  size_t contentsLen = 0;
  int errorCode = rfce->m_read_file(
    fname, &contentBytes, &contentsLen, rfce->m_extra);

  // Nullify for safety since it will be invalidated shortly.
  fname = NULL;

  if (contentBytes) {
    // Copy the contents into an OCaml bytes object.  The
    // 'caml_alloc_string' function creates it since, internally,
    // strings and byte objects are the same thing.
    contentsVal = caml_alloc_string(contentsLen);

    // Although the documentation does not say anything about this, I
    // can see from the implementation (runtime/memory.c,
    // caml_alloc_shr_aux()) that if allocation fails even after GC,
    // the allocation functions can return NULL.
    if (contentsVal) {
      memcpy(Bytes_val(contentsVal), contentBytes, contentsLen);
    }

    free(contentBytes);
  }
  else {
    // 'm_read_file' only returned a code, but we need to return a
    // string too in order to adhere to the OCaml type system.
    contentsVal = caml_copy_string("");
  }

  if (!contentsVal) {
    // I have not tested this code path.
    caml_raise_out_of_memory();
  }

  errorCodeVal = Val_int(errorCode);

  // Package the contents and code as a tuple.
  resultVal = caml_alloc_tuple(2);
  if (!resultVal) {
    caml_raise_out_of_memory();
  }
  Field(resultVal, 0) = contentsVal;
  Field(resultVal, 1) = errorCodeVal;

  CAMLreturn(resultVal);
}


// Copy an OCaml string onto the C heap.
static char *strdup_val(value strVal)
{
  return strdup(String_val(strVal));
}


char *bridge_ml_semgrep_analyze(
  char const * const *argv,
  ReadFileFunc read_file, void *read_file_extra)
{
  CAMLparam0();
  CAMLlocal4(argvVal, readFileVal, resVal, exnStringVal);

  // The cast here is required because the callee is not declared with
  // the right (most accurate) type.
  argvVal = caml_copy_string_array((char const **)argv);

  struct ReadFileCallbackExtra rfce;
  memset(&rfce, 0, sizeof(rfce));
  rfce.m_magic     = RFCE_MAGIC;
  rfce.m_read_file = read_file;
  rfce.m_extra     = read_file_extra;
  readFileVal = Val_fnptr(call_read_file, &rfce);

  char *errorMessage = NULL;

  enum { NUM_ARGS = 2 };
  value args[NUM_ARGS] = {
    argvVal,
    readFileVal
  };
  resVal = caml_callbackN_exn(*semgrep_cli_lib_main_ptr, NUM_ARGS, args);

  if (Is_exception_result(resVal)) {
    // This must be done immediately, and must overwrite 'resVal'.
    resVal = Extract_exception(resVal);

    // Call into OCaml to convert the exception to a string.
    exnStringVal = caml_callback_exn(*exn_to_string_ptr, resVal);
    if (Is_exception_result(exnStringVal)) {
      // Don't try to recover.
      assert(!"After core_analysis raised an exception, exn_to_string did too.");
      abort();
    }

    // Copy the string onto the C heap.
    errorMessage = strdup_val(exnStringVal);

    // Now, we simply drop 'resVal'; the exception has been handled.
  }

  // The OCaml code could hold onto these values forever, but they
  // contain pointers to local variables that are about to go out of
  // scope.  Invalidate them so any future use will raise
  // Invalid_argument rather than accessing freed memory.
  invalidate_fnptr(readFileVal);

  CAMLreturnT(char*, errorMessage);
}


// EOF
