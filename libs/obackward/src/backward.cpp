// Needed for:
//	- source filename
//	- line and column numbers
//	- source code snippet (assuming the file is accessible)
//
// On linux:
#if defined(__linux) || defined(__linux__)
#define BACKWARD_HAS_DW 1
// On macOS:
#elif defined(__APPLE__)
#define BACKWARD_HAS_DWARF 1
#endif

// Gives slightly better backtraces, but this is annoying to get working on
// windows
#ifndef _WIN32
#define BACKWARD_HAS_LIBUNWIND 1
#endif

#include "backward.h"

// backward::SignalHandling sh;
//
backward::SignalHandling *sh;
extern "C" {
// Let's use a c interface so we can easily setup a signal handler
bool register_sh() {
    sh = new backward::SignalHandling();
    return sh->loaded();
}
}
