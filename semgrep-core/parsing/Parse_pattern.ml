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
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse_pattern lang str =
  let any =
    match lang with
    | Lang.Csharp ->
        let res = Parse_csharp_tree_sitter.parse_pattern str in
        (* todo: double check there is no error?
         * todo: move in helper conversion of tree-sitter result to pattern
        *)
        (match res.program, res.errors with
         | None, _ -> failwith "no pattern found"
         | Some x, [] -> x
         (* todo: return error location, use Error module like in Parse_code *)
         | Some _, _::_ -> failwith "error parsing the pattern"
        )
    (* use pfff *)
    | _ -> Parse_generic.parse_pattern lang str
  in
  Check_semgrep.check_pattern lang any;
  any
