(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

module V = Visitor_js
module FT = File_type

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

(* copy paste of php code, not sure this is relevant for Javascript *)
let is_js_script file =
  Common.with_open_infile file (fun chan ->
    try
      let l = input_line chan in
      l =~ "#!/usr/.*/js" ||
      l =~ "#!/bin/env js" ||
      l =~ "#!/usr/bin/env js"

    with End_of_file -> false
  )

let find_source_files_of_dir_or_files ?(include_scripts=true)xs =
  Common.files_of_dir_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
    match FT.file_type_of_file filename with
    (* less: could also consider Typescript files *)
    | FT.PL (FT.Web FT.Js) -> true
    | _ ->
        if include_scripts then is_js_script filename else false
  ) |> Common.sort

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

let extract_info_visitor recursor =
  let globals = ref [] in
  let hooks = { V.default_visitor with
                V.kinfo = (fun (_k, _) i -> Common.push i globals)
              } in
  begin
    let vout = V.mk_visitor hooks in
    recursor vout;
    List.rev !globals
  end

let ii_of_any any =
  extract_info_visitor (fun visitor -> visitor any)
