// bridge_py.c
// Bridge layer that interfaces to Python.

// Python
#define PY_SSIZE_T_CLEAN               // Use Py_ssize_t for sizes of Python objects.
#include <Python.h>                    // Python API

// this dir
#define BRIDGE_ML_USE_DLOPEN           // Use dlopen to load the library.
#include "bridge_ml.h"                 // bridge_ml_*, dlhelp_*

// libc
#include <assert.h>                    // assert
#include <stdlib.h>                    // free


// Handle to the shared library.
static void *bridge_ml_handle = NULL;


// Docstring for the entire module.
PyDoc_STRVAR(bridge_module_doc,
"Call the Semgrep analysis directly from Python.\n\
\n\
This module contains the entire Semgrep analysis core, along with the\n\
OCaml run-time system, so it can be run from within a Python process\n\
directly instead of by launching an external program.");


// The syntax for including function signatures in docstrings defined in
// C is not documented (as far as I can tell), but is explained here:
// https://stackoverflow.com/questions/1104823/python-c-extension-method-signatures-for-documentation
//
// Unfortunately, that syntax does not allow type annotations.
PyDoc_STRVAR(bridge_startup_doc,
"startup()\n\
--\n\
\n\
Start the OCaml runtime.  Required before calling 'semgrep_analyze'.");

static PyObject *bridge_startup(PyObject *self, PyObject *args)
{
  if (!bridge_ml_handle) {
    char const *myself_filename =
      dlhelp_get_filename_with_address(&bridge_startup);
    bridge_ml_handle = dlhelp_dlopen_relative(
      "SEMGREP_BRIDGE_CORE_LIB",
      myself_filename,
      myself_filename,
      "semgrep_bridge_core.so",
      0 /*flags*/);
    assert(bridge_ml_handle);

    bridge_ml_get_symbols(bridge_ml_handle, myself_filename);
  }

  bridge_ml_startup();
  return Py_None;
}


// One cleanup task semgrep-core does is shut down the language server
// connection if it is active.
PyDoc_STRVAR(bridge_shutdown_doc,
"shutdown()\n\
--\n\
\n\
Stop the OCaml runtime.\n\
\n\
This allows it to perform cleanup tasks, but even after a shutdown, it\n\
cannot be started again in the same process.");

static PyObject *bridge_shutdown(PyObject *self, PyObject *args)
{
  bridge_ml_shutdown();

  // I could also close the library here.  For now, for simplicity, I
  // choose not to.

  return Py_None;
}


// Invoke Python callback in 'extra' that reads file contents from an
// in-memory file system.
//
// The signature of the callable is (string) -> (bytes, int).
static BridgeErrorCode call_py_read_file(
  char const *fname,
  char **contentsBytes,
  size_t *contentsLen,
  void *extra)
{
  BridgeErrorCode ret = BEC_OK;

  PyObject *callable = (PyObject*)extra;

  PyObject *args = Py_BuildValue("(s)", fname);
  if (!args) {
    PyErr_Clear();
    return BEC_OUT_OF_MEMORY;
  }

  PyObject *result = PyObject_CallObject(callable, args);
  if (!result) {
    PyErr_Clear();
    ret = BEC_UNKNOWN_ERROR;
    goto err1;
  }

  // The result should be a (bytes, int) tuple.
  Py_buffer contents;
  int code;
  if (!PyArg_ParseTuple(result, "y*i", &contents, &code)) {
    PyErr_Clear();

    *contentsBytes = strdup("read_file result must be a (bytes, int) tuple.");
    *contentsLen = strlen(*contentsBytes);
    ret = BEC_ERROR_MESSAGE;

    goto err2;
  }

  // Buffers evidently can be fairly complicated.  I'm hoping that I
  // only ever see the simple form here.  I looked at a few examples of
  // getting Py_buffer from PyArg_ParseTuple in the Python sources (for
  // example, Modules/ossaudiodev.c), and none of them appear to handle
  // non-contiguous buffers either.
  assert(PyBuffer_IsContiguous(&contents, 'C'));

  *contentsBytes = (char*)malloc(contents.len);
  if (!*contentsBytes) {
    ret = BEC_OUT_OF_MEMORY;
    goto err3;
  }

  memcpy(*contentsBytes, contents.buf, contents.len);
  *contentsLen = contents.len;

  ret = code? BEC_ERROR_MESSAGE : BEC_OK;

err3:
  PyBuffer_Release(&contents);

err2:
  Py_DECREF(result);

err1:
  Py_DECREF(args);
  return ret;
}


// Free the elements of NULL-terminated 'array', then free the array
// itself.
static void free_argv_array(char **array)
{
  for (char **p = array; *p; p++) {
    free(*p);
  }
  free(array);
}


// Allocate and return a NULL-terminated array of strings containing
// the same elements as 'list'.
//
// On error, raise a Python exception and return NULL.
static char **array_from_python_list(PyObject *list)
{
  // Number of elements, not including the final NULL pointer.
  Py_ssize_t numElements = PyList_Size(list);

  // The caller should have already checked that 'list' is a list.
  assert(numElements >= 0);

  size_t argvArraySize = (numElements+1) * sizeof(char*);
  char **argvArray = (char**)malloc(argvArraySize);
  if (!argvArray) {
    PyErr_NoMemory();
    return NULL;
  }

  // Clear the entire array, thus ensuring we can always treat it as
  // NULL-terminated, including during error handling.
  memset(argvArray, 0, argvArraySize);

  for (Py_ssize_t index = 0; index < numElements; index++) {
    // This returns a borrowed reference, so its reference count should
    // not be decremented.
    PyObject *pyStr = PyList_GetItem(list, index);
    if (!pyStr) {
      // Index out of bounds.  Conceivably this could happen if the list
      // changes size while this loop is running, although I doubt that
      // is really possible.  Anyway, clean up and bail out, preserving
      // the exception.
      goto err;
    }

    // 'strData' is not a tuple, so we use Parse rather than ParseTuple.
    //
    // The error message fragment after "s:" obviously ties this
    // function to its context, but I like clear messages.
    char *strData;
    if (!PyArg_Parse(pyStr, "s:Element of 'argv' in semgrep_analyze", &strData)) {
      // The element was not a string.
      goto err;
    }

    argvArray[index] = strdup(strData);
    if (!argvArray[index]) {
      PyErr_NoMemory();
      goto err;
    }
  }

  return argvArray;

err:
  free_argv_array(argvArray);
  return NULL;
}


PyDoc_STRVAR(bridge_semgrep_analyze_doc,
"semgrep_analyze(argv, read_file)\n\
--\n\
\n\
Invoke the Semgrep analysis.\n\
\n\
argv: List of command line strings.  The first element is the program\n\
name.  The rest follow the same syntax as the semgrep-core stand-alone\n\
program.\n\
\n\
read_file(fname: str) -> (bytes, int): Invoked to read the contents of\n\
a named file.  On success, return the contents and 0.  On failure,\n\
return an error message encoded as UTF-8 and 1.\n\
\n\
If a callback throws an exception, its details will be lost and\n\
semgrep-core will report a generic error.\n\
\n\
returns: If no error occurred, None.  Otherwise, a string describing\n\
the error.");

static PyObject *bridge_semgrep_analyze(PyObject *self, PyObject *args)
{
  // Expect that 'args' is a tuple.
  //
  // The pointers we extract remain valid for the duration of this call
  // because tuples are immutable, and the calling machinery has a
  // strong reference to the 'args' tuple.
  PyObject *argv_list;
  PyObject *read_file;
  if (!PyArg_ParseTuple(args, "OO",
                        &argv_list,
                        &read_file)) {
    return NULL;                       // Exception raised (e.g., type error).
  }
  if (!PyList_Check(argv_list)) {
    PyErr_SetString(PyExc_TypeError, "'argv' must be a list.");
    return NULL;
  }
  if (!PyCallable_Check(read_file)) {
    PyErr_SetString(PyExc_TypeError, "'read_file' must be callable.");
    return NULL;
  }

  // Copy 'argv' into a C array.
  char **argvArray = array_from_python_list(argv_list);
  if (!argvArray) {
    return NULL;   // Exception already raised.
  }

  // Call into the core.
  char *errorMessage = bridge_ml_semgrep_analyze(
    // This cast is required because the C implicit conversion rules are
    // a little too restrictive.  It would not be required in C++.
    (char const * const *)argvArray,

    call_py_read_file, read_file);

  free_argv_array(argvArray);

  // Convert the result into a Python object.
  PyObject *result;
  if (errorMessage) {
    // Note: This could raise an exception, leading to a NULL return.
    // As the code stands, that's fine, but keep this in mind when
    // changes are made.
    result = Py_BuildValue("s", errorMessage);
    free(errorMessage);
  }
  else {
    result = Py_None;
    Py_INCREF(result);
  }

  // Ownership transfers to the caller, who will (eventually) decrement
  // this, counteracting the increment in Py_BuildValue or Py_INCREF.
  return result;
}


// Method table for 'semgrep_bridge' module.
static PyMethodDef bridgeMethods[] = {
  {
    "startup",
    bridge_startup,
    METH_VARARGS,
    bridge_startup_doc
  },
  {
    "shutdown",
    bridge_shutdown,
    METH_VARARGS,
    bridge_shutdown_doc
  },
  {
    "semgrep_analyze",
    bridge_semgrep_analyze,
    METH_VARARGS,
    bridge_semgrep_analyze_doc
  },
  {0,0,0,0}
};


// Module definition.
static struct PyModuleDef bridgeModule = {
  PyModuleDef_HEAD_INIT,
  "semgrep_bridge_python",   // Module name.
  bridge_module_doc,         // Module documentation.
  -1,                        // Size of heap-allocated module state; -1 means none.
  bridgeMethods              // Module table.
};


// Initialization function called by the Python interpreter.
//
// The name of the function corresponds to the module name in Python,
// "semgrep_bridge_python".
PyMODINIT_FUNC PyInit_semgrep_bridge_python(void)
{
  return PyModule_Create(&bridgeModule);
}


// EOF
