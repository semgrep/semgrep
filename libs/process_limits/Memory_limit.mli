(*
   Avoid segfaults when the process runs out of memory.
*)

exception ExceededMemoryLimit of string

(*
   Set an approximate limit the combined heap+stack size,
   resulting in a 'Out_of_memory' exception. To help prevent stack overflows,
   we also produce a warning when the stack is detected to be too large.

   This is done by checking the GC's stats at the end of each major GC
   cycle. It is approximate but OS-independent. The goal is to reliably
   raise an exception when running out of memory rather than getting
   a segfault, which is unrecoverable in OCaml.

   'Gc.compact ()' is called before re-raising any 'Out_of_memory' exception
   so as to reclaim some space.

   Note that tuning the GC with Gc.set may not interact well with
   Memory_limit and its use of Gc.alarm. Indeed, the Gc.alarm triggers
   only at major cycle, so tuning the GC may influence how frequently
   major cycles occur.

   As of ocaml 4.12, segfaults often occur when running out of physical
   memory. Segfaults used to occur also on stack overflows on some
   architectures and/or operating systems, although this
   has been fixed in ocaml 4.10, at least partially. It should be fine
   on Linux/x86_64 where we'd get a 'Stack_overflow' exception.
   See:
   - https://discuss.ocaml.org/t/is-there-any-value-in-having-a-maximum-stack-size/8214/10
   - https://github.com/semgrep/semgrep/issues/3640

   The limits set by this function call should be lower than the system
   limits so as to get exceptions instead of segfaults.
   The system limits are:

   - for stack overflows, there's usually a limit. Check 'ulimit -s'
     from bash. On Linux, 'ulimit -Ss unlimited' will remove the soft limit
     in the current process and its future children. This is allowed if
     the hard limit ('ulimit -Hs') is 'unlimited', which is the default.
     The default (soft) limit on Linux and Darwin is 8 MiB. The corresponding
     POSIX system call 'setrlimit' for setting the limit is said to be broken
     on Darwin, where there's a hard limit of about 64 MiB.
     On Windows, the limit is harcoded in the executable and defaults to 1 MiB.
   - for OOM errors, the limit is dictated by how much physical memory is
     available to all the processes. A Docker container typically has its
     own memory limit which ensures the host won't be taken down if
     processes in the container use too much memory.

   Function parameters:

   - stack_warning_kb: a warning will be printed if the stack size
     is found to exceed this value (in KiB). This is often not detected
     early enough or at all if there's not enough allocation (GC activity).
   - heap_warning_mb: a warning will be printed if the heap size
     is found to exceed this value (in MiB).
   - mem_limit_mb: an 'Out_of_memory' exception will be raised if the
     combined heap+stack size is found to exceed this value (in MiB).

   All the parameters are expressed in MiB (2^20 bytes). A value of 0
   indicates no limit.

   Default values are the minimum values we're willing to support for
   semgrep-core:
   - default stack_warning_kb: 100 KiB
   - default heap_warning_mb: 500 MiB
*)
val run_with_memory_limit :
  < Cap.memory_limit > ->
  ?get_context:(unit -> string) ->
  ?stack_warning_kb:int ->
  ?heap_warning_mb:int ->
  mem_limit_mb:int ->
  (unit -> 'a) ->
  'a

val default_stack_warning_kb : int
