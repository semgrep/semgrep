(*s: pfff/lang_python/parsing/Lib_parsing_python.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module V = Visitor_python

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

(*s: function [[Lib_parsing_python.find_source_files_of_dir_or_files]] *)
let find_source_files_of_dir_or_files xs = 
  Common.files_of_dir_or_files_no_vcs_nofilter xs 
  |> List.filter (fun filename ->
    let ftype = File_type.file_type_of_file filename in
    match ftype with
    | File_type.PL (File_type.Python) -> true
    | _ -> false
  ) |> Common.sort
(*e: function [[Lib_parsing_python.find_source_files_of_dir_or_files]] *)

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

(*s: function [[Lib_parsing_python.extract_info_visitor]] *)
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
(*e: function [[Lib_parsing_python.extract_info_visitor]] *)

(*s: function [[Lib_parsing_python.ii_of_any]] *)
let ii_of_any any = 
  extract_info_visitor (fun visitor -> visitor any)
(*e: function [[Lib_parsing_python.ii_of_any]] *)
(*e: pfff/lang_python/parsing/Lib_parsing_python.ml *)
