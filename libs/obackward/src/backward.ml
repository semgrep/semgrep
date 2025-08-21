(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* A basic OCaml wrapper around https://github.com/bombela/backward-cpp.
   Depending on the platform and compilation options, this library will register
   a signal handler that will print a gdb style backtrace when a some critical
   signals are received occurs, in OCaml or C FFI otherwise.

   The following signals will be backtraced:
    SIGABRT, // Abort signal from abort(3)
    SIGBUS,  // Bus error (bad memory access)
    SIGFPE,  // Floating point exception
    SIGILL,  // Illegal Instruction
    SIGIOT,  // IOT trap. A synonym for SIGABRT
    SIGQUIT, // Quit from keyboard
    SIGSEGV, // Invalid memory reference
    SIGSYS,  // Bad argument to routine (SVr4)
    SIGTRAP, // Trace/breakpoint trap
    SIGXCPU, // CPU time limit exceeded (4.2BSD)
    SIGXFSZ, // File size limit exceeded (4.2BSD)
    if darwin:
    SIGEMT, // emulation instruction executed

   And additionally SIGUSR1, so we can trigger this manually for debugging
   purposes (say there is an infinite loop).

   If -g is passed to the compiler, you will get at the least:
   - faulting address
   - stack trace
   - function names

   If a symbolization library is available, you will also get:
   - source file names
   - line numbers

   If the source is available, and the root of it (this repository), is your CWD
   you'll get:
   - code snippets!
*)

external register : unit -> bool = "ml_register"

(* A basic interface for setting up the backward-cpp signal handler *)
let register () =
  if not (register ()) then
    Error
      "Failed to register unwind handler for some critical signals, such as \
       SIGSEGV. If we segfault you are on your own and you will receive no \
       backtraces"
  else Ok ()
