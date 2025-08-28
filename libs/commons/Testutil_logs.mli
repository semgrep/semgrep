(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   A function that masks the timestamps in log output so that we can compare
   logs from one run to another. To be used as:

     Testo.create
         ~checked_output:(Testo.stderr ())
         ~normalize:[Testutil_logs.mask_time] ...

   This is crude. Beware false positives.
*)
val mask_time : string -> string

(*
   Mask all lines that look like log lines. This won't work for multiline
   logs:

     Testo.create
        ~checked_output:(Testo.stderr ())
        ~normalize:[Testutil_logs.mask_log_lines]
        ...

   This is crude. Beware false positives.
*)
val mask_log_lines : string -> string
