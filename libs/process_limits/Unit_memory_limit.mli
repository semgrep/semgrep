(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
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

val tests : unit -> Testo.t list
