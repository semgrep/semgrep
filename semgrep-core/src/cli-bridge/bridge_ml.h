// bridge_ml.h
// ML side of the bridge module, meant to be called by the Python side.

#ifndef CLI_BRIDGE_BRIDGE_ML_H
#define CLI_BRIDGE_BRIDGE_ML_H

#include <stddef.h>                    // size_t


// In order to facilitate using this library with an explicit dlopen()
// call rather than implicit loading, all of the function signatures are
// first declared as a typedef, and then the function itself is declared
// using that typedef.


// Start or stop the OCaml runtime.
//
// It is not possible to start the runtime more than once unless the
// containing shared library is completely unloaded and reloaded.
typedef void bridge_ml_startup_t(void);
typedef void bridge_ml_shutdown_t(void);

// Result codes for the function pointers here.
typedef enum BridgeErrorCode {
  BEC_OK            = 0,     // No error.
  BEC_OUT_OF_MEMORY = 1,     // Out of memory.
  BEC_UNKNOWN_ERROR = 2,     // Another error, the details of which are lost.
  BEC_ERROR_MESSAGE = 3,     // Error for which a message is conveyed.
} BridgeErrorCode;

// Type of a callback to read the contents of a named file.
//
// If this succeeds, set 'contentsBytes' to point at malloc'd memory
// holding the file contents, and 'contentsLen' to its size in bytes,
// not including any NUL terminator.  The caller is then responsible for
// freeing the data.
//
// If this fails, then set 'contentsBytes' to an error message, with its
// length in 'contentsLen'.  Again, the caller must free it.
//
// However, in the out of memory case, the returned 'contentsBytes' will
// be NULL.
typedef BridgeErrorCode (*ReadFileFunc)(
  char const *fname,
  char **contentsBytes,
  size_t *contentsLen,
  void *extra);

// Call the semgrep core.
//
// 'argv' is a pointer to a NULL-terminated array of pointers to
// argument strings.  The other parameters are callbacks whose behavior
// is explained above.
//
// If there is an error, return an error message in a malloc'd string
// that must subsequently be free'd.  Returns NULL if there is no error.
typedef char *bridge_ml_semgrep_analyze_t(
  char const * const *argv,
  ReadFileFunc read_file, void *read_file_extra);


// If BRIDGE_ML_USE_DLOPEN is defined, then we intend to load the
// library explicitly using dlopen().  This should only be defined when
// this file is directly included by a .c file, since it allocates space
// in the corresponding object file.
#ifdef BRIDGE_ML_USE_DLOPEN

#include "dlhelp.h"                    // dlhelp_dlsym

// Pointers to the functions in the shared library.
static bridge_ml_startup_t         *bridge_ml_startup;
static bridge_ml_shutdown_t        *bridge_ml_shutdown;
static bridge_ml_semgrep_analyze_t *bridge_ml_semgrep_analyze;

// Load the library symbols.
static void bridge_ml_get_symbols(
  void *libhandle,           // Returned by 'dlopen'.
  char const *errorContext)  // Context for error messages.
{
  #define BRIDGE_ML_GET_SYM(funcname) \
    dlhelp_dlsym((void**)&funcname, errorContext, libhandle, #funcname)

  BRIDGE_ML_GET_SYM(bridge_ml_startup);
  BRIDGE_ML_GET_SYM(bridge_ml_shutdown);
  BRIDGE_ML_GET_SYM(bridge_ml_semgrep_analyze);

  #undef BRIDGE_ML_GET_SYM
}


#else // !BRIDGE_ML_USE_DLOPEN

// These are the declarations for the actual functions, only usable when
// using implicit loading when the program loads.
extern bridge_ml_startup_t         bridge_ml_startup;
extern bridge_ml_shutdown_t        bridge_ml_shutdown;
extern bridge_ml_semgrep_analyze_t bridge_ml_semgrep_analyze;


#endif // !BRIDGE_ML_USE_DLOPEN


#endif // CLI_BRIDGE_BRIDGE_ML_H
