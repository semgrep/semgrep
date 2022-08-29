// dlhelp.c
// Code for dlhelp.h

// Enable 'dladdr' from 'dlfcn.h' on Linux.
#define _GNU_SOURCE

#include "dlhelp.h"                    // this module

// libc
#include <assert.h>                    // assert
#include <stddef.h>                    // ptrdiff_t
#include <stdio.h>                     // printf
#include <stdlib.h>                    // getenv, exit, malloc, free
#include <string.h>                    // memset, strrchr

// POSIX
#include <dlfcn.h>                     // dlopen, dlsym, dlclose, dlerror


void *dlhelp_dlopen_relative(
  char const *envvar,
  char const *siblingFilename,
  char const *errorContext,
  char const *filename,
  int flags)
{
  if (flags == 0) {
    // By default, resolve all unresolved symbols.
    flags = RTLD_NOW;
  }

  if (envvar) {
    char const *s = getenv(envvar);
    if (s) {
      filename = s;
    }
  }

  // If not NULL, a pointer to an array that needs to be freed.
  char *allocFilename = NULL;

  if (siblingFilename && filename[0] != '/') {
    char const *slash = strrchr(siblingFilename, '/');
    if (slash) {
      // Concatenate the directory portion of 'siblingFname' with
      // 'filename'.
      ptrdiff_t dirlen = slash - siblingFilename;
      allocFilename = (char*)malloc(dirlen + 1 + strlen(filename) + 1);
      memcpy(allocFilename, siblingFilename, dirlen);
      allocFilename[dirlen] = '/';
      strcpy(allocFilename+dirlen+1, filename);

      filename = allocFilename;
    }
  }

  // Clear any existing error message.
  dlerror();

  // Call dlopen.
  void *libhandle = dlopen(filename, flags);

  // Check for error.
  char const *err = dlerror();
  if (err) {
    fprintf(stderr, "%s: dlopen: %s\n", errorContext, err);
    fflush(stderr);
    free(allocFilename);
    exit(2);
  }

  free(allocFilename);
  return libhandle;
}


void dlhelp_dlsym(
  void **funcptrptr,
  char const *errorContext,
  void *libhandle,
  char const *funcname)
{
  dlerror();

  *funcptrptr = dlsym(libhandle, funcname);

  char const *err = dlerror();
  if (err) {
    fprintf(stderr, "%s: dlsym(\"%s\"): %s\n", errorContext, funcname, err);
    exit(2);
  }
}


void dlhelp_dlclose(
  char const *errorContext,
  void *libhandle)
{
  dlerror();

  dlclose(libhandle);

  char const *err = dlerror();
  if (err) {
    fprintf(stderr, "%s: dlclose: %s\n", errorContext, err);
    exit(2);
  }
}


char const *dlhelp_get_filename_with_address(
  void *address)
{
  Dl_info info;
  memset(&info, 0, sizeof(info));

  int res = dladdr(address, &info);
  if (res) {
    char const *ret = info.dli_fname;
    assert(ret[0]);

    // On the systems I have tested, 'Dl_info' contains pointers to
    // memory allocated elsewhere.  To be sure of that, clear 'info' and
    // then re-check that the data we care about is still valid.  This
    // would catch a case where 'dli_fname' was a pointer into the
    // 'Dl_info' structure itself.
    memset(&info, 0, sizeof(info));
    assert(ret[0]);

    return ret;
  }

  else {
    fprintf(stderr, "Failed to get filename for library symbol %p.",
                    address);
    exit(2);
    return NULL; // Not reached.
  }
}


// EOF
