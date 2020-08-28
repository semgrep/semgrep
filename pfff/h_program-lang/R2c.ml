(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
module J = Json_type
module PI = Parse_info
module E = Error_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

(* copy-paste: checked_return.ml *)
let loc_to_json_range loc =
  (* pfff (and Emacs) have the first column at index 0, but not r2c *)
  let adjust_column x = x + 1 in
  J.Object [
    "line", J.Int loc.PI.line;
    "col", J.Int (adjust_column loc.PI.column);
  ],
  J.Object [
    "line", J.Int loc.PI.line;
    "col", J.Int (adjust_column (loc.PI.column + String.length loc.PI.str));
  ],
  loc.PI.line

let hcache = Hashtbl.create 101
let lines_of_file (file: Common.filename) : string array =
  Common.memoized hcache file (fun () ->
   try
    Common.cat file |> Array.of_list
   with _ -> [|"EMPTY FILE"|]
  )

let error_to_json err =
   let file = err.E.loc.PI.file in
   let lines = lines_of_file file in
   let (startp, endp, line) = loc_to_json_range err.E.loc in
   let check_id = E.check_id_of_error_kind err.E.typ in
   let message = E.string_of_error_kind err.E.typ in
   let extra_extra =
     match err.E.typ with
     | _ -> []
   in
   J.Object [
      "check_id", J.String check_id;
      "path", J.String file;
      "start", startp;
      "end", endp;
      "extra", J.Object ([
         "message", J.String message;
         "line", J.String (try lines.(line - 1) with _ -> "NO LINE");
          ] @ extra_extra);
   ]

let string_of_errors errs =
  let arr = J.Array (errs |> List.map (error_to_json)) in
  let json = J.Object ["results", arr] in
  Json_io.string_of_json json

