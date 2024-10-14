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
open Common
open Fpath_.Operators

(*module V = Visitor_ml*)

(*****************************************************************************)
(* Filemames *)
(*****************************************************************************)

let find_source_files_of_dir_or_files xs =
  UFile.files_of_dirs_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
         match File_type.file_type_of_file filename with
         | File_type.PL (File_type.OCaml ("ml" | "mli")) -> true
         | _ -> false)
  |> List_.sort

let find_ml_files_of_dir_or_files xs =
  UFile.files_of_dirs_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
         match File_type.file_type_of_file filename with
         | File_type.PL (File_type.OCaml "ml") -> true
         | _ -> false)
  |> List_.sort

let find_cmt_files_of_dir_or_files (xs : Fpath.t list) : Fpath.t list =
  UFile.files_of_dirs_or_files_no_vcs_nofilter xs
  |> List.filter (fun filename ->
         match File_type.file_type_of_file filename with
         | File_type.Obj ("cmt" | "cmti") -> true
         | _ -> false)
  (* ocaml 4.07 stdlib now has those .p.cmt files that cause dupe errors *)
  |> List_.exclude (fun filename -> !!filename =~ ".*\\.p\\.cmt")
  (* sometimes there is just a .cmti and no corresponding .cmt because
   * people put the information only in a .mli
   *)
  |> (fun xs ->
       let hfiles = Hashtbl.create 101 in
       xs
       |> List.iter (fun file ->
              let d, b, e = Filename_.dbe_of_filename !!file in
              Hashtbl_.push hfiles (d, b) e);
       Common2.hkeys hfiles
       |> List_.map (fun (d, b) ->
              let xs = Hashtbl_.get_stack hfiles (d, b) in
              match xs with
              | [ "cmt"; "cmti" ]
              | [ "cmti"; "cmt" ]
              | [ "cmt" ] ->
                  Filename_.filename_of_dbe (d, b, "cmt")
              | [ "cmti" ] -> Filename_.filename_of_dbe (d, b, "cmti")
              | _ -> raise Impossible))
  |> Fpath_.of_strings |> List_.sort

(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

(* convert to generic AST if you need to get tokens!

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
*)
