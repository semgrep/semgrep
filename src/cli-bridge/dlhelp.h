// dlhelp.h
// Wrapper around dlopen() to do relative lookup, etc.

#ifndef CLI_BRIDGE_DLHELP_H
#define CLI_BRIDGE_DLHELP_H

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus


// Like 'dlopen(filename, flags)', but:
//
// * If 'flags' is 0, then treat it like RTLD_NOW.  (RTLD_NOW is good
//   for ensuring symbol errors are caught immediately, and making it
//   the default means clients of this library do not have to include
//   dlfcn.h, which is system-dependent.)
//
// * If 'envvar' is not NULL, and the corresponding environment variable
//   is set, then use that variable's value in place of 'filename'.
//
// * If 'siblingFilename' is not NULL, contains a slash, and 'filename'
//   does not begin with a slash, then concatenate the directory portion
//   of it with 'filename' (separated by a path separator, and the
//   latter possibly as overridden by 'envvar'), so we can load a
//   library from a location relative to that sibling file.
//
// * If we fail to open the library, print the error message to stderr
//   and call exit(2).  (This routine has no provision for recovery.)
//
// errorContext: This is printed as error message context if an error
// message is printed.
//
void *dlhelp_dlopen_relative(
  char const *envvar,
  char const *siblingFilename,
  char const *errorContext,
  char const *filename,
  int flags);


// Like '*funcptrptr = dlsym(libhandle, funcname)', except if the
// attempt fails, print the error message to stderr and exit(2).
//
// errorContext: This is printed as error message context if an error
// message is printed.
void dlhelp_dlsym(
  void **funcptrptr,
  char const *errorContext,
  void *libhandle,
  char const *funcname);


// Like 'dlclose(libhandle)', but print and exit on error.
void dlhelp_dlclose(
  char const *errorContext,
  void *libhandle);


// Return the name of the file containing the symbol at 'address'.  One
// use is to retrieve the name of the shared library that contains the
// running code, where 'address' can be the address of the current
// function.
//
// If the file name cannot be obtained, prints an error to stderr and
// calls exit(2).
char const *dlhelp_get_filename_with_address(
  void *address);


#ifdef __cplusplus
}
#endif // __cplusplus

#endif // CLI_BRIDGE_DLHELP_H
