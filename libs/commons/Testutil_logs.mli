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
