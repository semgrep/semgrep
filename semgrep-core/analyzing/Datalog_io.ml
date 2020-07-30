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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Writing/reading datalog facts on disk.
 *
 * This was started from pfff/h_program-lang/datalog_code.ml which
 * handled the BDDBDDB and lui-based datalog engines.
 * This file handles DOOP/Souffle.
 *)

(*****************************************************************************)
(* Write *)
(*****************************************************************************)
let write_facts_for_doop _facts _dir =
  raise Todo
