(*
   Avoid segfaults when the process runs out of memory.
*)

(*
   Set approximate limits on stack size and combined heap+stack size,
   resulting in 'Stack_overflow' and 'Out_of_memory' exceptions,
   respectively.
   This is done by checking the GC's stats at the end of each major GC
   cycle. It is approximate but OS-independent. The goal is to reliably
   raise an exception when running out of memory rather than getting
   a segfault, which is unrecoverable in OCaml.

   'Gc.compact ()' is called before re-raising any 'Out_of_memory' exception
   so as to reclaim some space.

   As of ocaml 4.12, segfaults often occur when running out of physical
   memory. Segfaults used to occur also on stack overflows, although this
   has been fixed in ocaml 4.10, at least partially.
   See:
   - https://discuss.ocaml.org/t/is-there-any-value-in-having-a-maximum-stack-size/8214/10
   - https://github.com/returntocorp/semgrep/issues/3640

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

   - stack_size_warning_mb: a warning will be printed if the stack size
     is found to exceed this value (in MiB).
   - stack_size_limit_mb: a 'Stack_overflow' exception will be raised if
     the stack size is found to exceed this value (in MiB).
   - mem_limit_mb: an 'Out_of_memory' exception will be raised if the
     combined heap+stack size is found to exceed this value (in MiB).

   All the parameters are expressed in MiB (2^20 bytes). A value of 0
   indicates no limit.

   Default values are the minimum values we're willing to support for
   semgrep-core:
   - default stack_size_warning_mb: 5 MiB
   - default stack_size_limit_mb: 7 MiB (based on Linux default of 8 MiB)

   Recommendations:
   - for testing, use the lowest stack size limit that we're willing
     to support.
   - for production, use something like 85% of the system's maximum stack size
     if there is one and it's known. If it's known to be unlimited,
     something like 50% of 'mem_limit_mb' should be fine.
*)
val run_with_memory_limit :
  ?stack_warning_mb:int ->
  ?stack_limit_mb:int ->
  mem_limit_mb:int ->
  (unit -> 'a) ->
  'a

val default_stack_warning_mb : int

val default_stack_limit_mb : int
