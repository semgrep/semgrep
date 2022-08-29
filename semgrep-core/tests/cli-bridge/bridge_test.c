// bridge_test.c
// main() function to call directly into bridge_ml.c.

#define BRIDGE_ML_USE_DLOPEN           // We will use dlopen.

#include "bridge_ml.h"                 // bridge_ml_*, dlhelp_*

// libc
#include <ctype.h>                     // isdigit
#include <stdio.h>                     // printf
#include <stdlib.h>                    // free, atoi
#include <string.h>                    // strdup


// Serve file request by reading from disk.
static BridgeErrorCode read_file(
  char const *fname,
  char **contentsBytes,
  size_t *contentsLen,
  void *extra)
{
  BridgeErrorCode ret = BEC_OK;
  long size = 0;

  FILE *fp = fopen(fname, "rb");
  if (!fp) {
    *contentsBytes = strdup("failed to open file");
    *contentsLen = strlen(*contentsBytes);
    return BEC_ERROR_MESSAGE;
  }

  if (fseek(fp, 0, SEEK_END) < 0) {
    *contentsBytes = strdup("failed to seek to end");
    *contentsLen = strlen(*contentsBytes);
    ret = BEC_ERROR_MESSAGE;
    goto err1;
  }

  size = ftell(fp);
  if (size < 0) {
    *contentsBytes = strdup("failed to ftell");
    *contentsLen = strlen(*contentsBytes);
    ret = BEC_ERROR_MESSAGE;
    goto err1;
  }

  if (fseek(fp, 0, SEEK_SET) < 0) {
    *contentsBytes = strdup("failed to seek to beginning");
    *contentsLen = strlen(*contentsBytes);
    ret = BEC_ERROR_MESSAGE;
    goto err1;
  }

  *contentsLen = size;
  *contentsBytes = (char*)malloc(size+1);

  if (fread(*contentsBytes, 1, size, fp) != *contentsLen) {
    free(*contentsBytes);
    *contentsBytes = strdup("failed to read all of the file");
    *contentsLen = strlen(*contentsBytes);
    ret = BEC_ERROR_MESSAGE;
    goto err1;
  }

err1:
  (void)fclose(fp);
  return ret;
}


int main(int argc, char **argv)
{
  printf("in bridge_test\n");
  fflush(stdout);

  void *libhandle = dlhelp_dlopen_relative(
    "BRIDGE_TEST_LIBNAME",
    argv[0],
    argv[0],
    "semgrep_bridge_core.so",
    0 /*flags*/);

  bridge_ml_get_symbols(libhandle, "bridge_test");


  bridge_ml_startup();

  char *errorMessage = bridge_ml_semgrep_analyze(
    // This cast is required because the C implicit conversion rules are
    // a little too restrictive.  It would not be required in C++.
    (char const * const *)argv,

    read_file, NULL);

  if (errorMessage) {
    printf("errorMessage: %s\n", errorMessage);
    free(errorMessage);
    return 2;
  }

  bridge_ml_shutdown();


  // Close the library.
  dlhelp_dlclose(argv[0], libhandle);

  printf("returning from bridge_test\n");
  return 0;
}


// EOF
