(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Small wrapper around the easy_logging library.
 *
 * My requirements for a logging library are:
 *  (1) different log levels (Debug, Info, Warning, Error)
 *  (2) easy logging calls with sprintf style formatting by default
 *  (3) ability to log on stdout/stderr or in a file
 *  (4) easy one-line logger creation
 *  (5) hierarchical loggers where you can enable/disable a whole set
 *      of loggers
 *  (6) configurable with an external log config file
 *
 * There are a few OCaml logging libraries, but they usually miss one or more
 * of the requirements above:
 *  - Logs: popular library according to OPAM metrics. Howver, Logs is heavily
 *    functorized (no (4)), and it requires to encapsulate your logging calls
 *    in a closure 'Log.info (fun m -> m "xxx")' (no (2)). It also lacks
 *    (5) and (6).
 *  - dolog: very basic logging library, very easy to use with (1), (2),
 *    but lacks especially (5) and (6)
 *  - Bolt: not updated since 2013
 *  - merlin/logging.ml: not available directly under OPAM and seems
 *    more limited than easy_logging
 *  - easy_logging: not really popular according to OPAM (no package depends
 *    on it), use OCaml objects, some documentation but no good examples,
 *    some unintuitive interfaces, some issues to compile without -42
 *    due to ambiguous constructors, 0.8 still not in OPAM, etc.
 *    But, it supports all the requirements if you know how to use it!
 * => I use easy_logging (actually I use easy_logging_yojson for (6))
 *
 *)
open Easy_logging_yojson

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* it takes a list, so you can have a deep hierarchy like
 * get_logger ["Generic_vs_generic"; "Env"]
 *)
let get_logger xs =
  let final_name = ("Main"::xs) |> String.concat "." in
  Logging.get_logger final_name

(* see log_config.json for an example of configuration *)
let load_config_file file =
  Logging.load_global_config_file file
