// bridge_ml.h
// ML side of the bridge module, meant to be called by the Python side.

#ifndef CLI_BRIDGE_BRIDGE_ML_H
#define CLI_BRIDGE_BRIDGE_ML_H

#include <stddef.h>                    // size_t


// Start or stop the OCaml runtime.
//
// It is not possible to start the runtime more than once unless the
// containing shared library is completely unloaded and reloaded.
void bridge_ml_startup();
void bridge_ml_shutdown();

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
char *bridge_ml_semgrep_analyze(
  char const * const *argv,
  ReadFileFunc read_file, void *read_file_extra);


#endif // CLI_BRIDGE_BRIDGE_ML_H
