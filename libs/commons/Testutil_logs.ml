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
(* Masking functions useful to be used with Testo.
 * coupling: the regexp must match the format in Logs_.mk_reporter
 *)
let mask_time =
  Testo.mask_pcre_pattern
    ~replace:(fun _ -> "<MASKED TIMESTAMP>")
    {|\[([0-9]+\.[0-9]{2})\]|}

let mask_log_lines =
  Testo.mask_pcre_pattern
    ~replace:(fun _ -> "<MASKED LOG LINE>")
    {|\[[0-9]+\.[0-9]{2}\][^\n]*|}
