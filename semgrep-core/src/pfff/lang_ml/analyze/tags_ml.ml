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

open Highlight_code
module Flag = Flag_parsing
module Tags = Tags_file
module E = Entity_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * (ab)Using the code highlighter to extract tags.
 *
 * Alternatives:
 *  - use graph_code_tags.ml now that .cmt contains more precise information
 *  - otags, but does not work very well cos it stops everything
 *    when it encounters an unparable file.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*
let tag_of_name filelines name =
  let info = Ast.info_of_name name in
  tag_of_info filelines info
*)

let entity_of_highlight_category_opt x =
  match x with
  | Highlight_code.Entity (kind, (Def2 _)) -> Some kind
  | FunctionDecl _ -> Some E.Function
  | _ -> None

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let defs_of_files_or_dirs ?(verbose=false) xs =
  let files = Lib_parsing_ml.find_source_files_of_dir_or_files xs in

  files |> Console.progress ~show:verbose (fun k ->
    List.map (fun file ->
      k();
      let (_ast, toks) =
        try
          Common.save_excursion Flag.show_parsing_error false(fun()->
            let res = Parse_ml.parse file in
            res.PI.ast, res.PI.tokens
          )
        with Parse_info.Parsing_error pos ->
          pr2 (spf "PARSING error in %s" (Parse_info.string_of_info pos));
          [], []
      in
      let filelines = Common2.cat_array file in
      let defs = ref [] in
      let h = Hashtbl.create 101 in

      (* computing the token attributes *)
      let _prefs = Highlight_code.default_highlighter_preferences in

      (* TODO highlighter not anymore in pfff
            Highlight_ml.visit_program
              ~lexer_based_tagger:true (* !! *)
              ~tag_hook:(fun info categ -> Hashtbl.add h info categ)
              prefs
              file
              (ast, toks)
            ;
      *)

      (* processing the tokens in order *)
      toks |> List.iter (fun tok ->

        let info = Token_helpers_ml.info_of_tok tok in
        let s = Parse_info.str_of_info info in

        let categ = Common2.hfind_option info h in

        categ |> Option.iter (fun x ->
          entity_of_highlight_category_opt x |> Option.iter (fun kind ->

            Common.push (Tags.tag_of_info filelines info kind) defs;

            let (d,b,e) = Common2.dbe_of_filename file in
            let module_name = String.capitalize_ascii b in

            let info' = Parse_info.rewrap_str (module_name ^ "." ^ s) info in

            (* I prefer my tags to led me to the .ml file rather than
             * the .mli because the .mli is usually small and
             * I have a key to go from the .ml to .mli anyway.
            *)

            if e = "ml" ||
               (e = "mli" && not (Sys.file_exists
                                    (Common2.filename_of_dbe (d,b, "ml"))))
            then
              Common.push (Tags.tag_of_info filelines info' kind) defs;
          )
        )
      );
      let defs = List.rev (!defs) in
      (file, defs)
    ))
