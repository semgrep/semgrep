(*
   Unit tests for out-of-memory errors and stack overflows.

   It should not crash the whole test process even if it doesn't work.
   i.e. don't trigger segfaults by exceeding the system limits
   (memory or stack).

   Important: these tests *assume* that the system's maximum stack size
   is 8 MiB or greater. This is usually correct on Linux and MacOSX,
   and usually incorrect on Windows. See detailed notes in
   Memory_limit.mli.
*)

val tests : < Cap.memory_limit > -> Testo.t list
