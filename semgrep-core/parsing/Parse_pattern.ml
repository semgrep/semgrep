(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
*)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Mostly a wrapper around pfff Parse_generic.
*)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_pattern lang str =
  let any =
    match lang with
    (* use pfff *)
    | _ -> Parse_generic.parse_pattern lang str
  in
  Check_semgrep.check_pattern lang any;
  any
