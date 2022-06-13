(* Yoann Padioleau
 *
 * Copyright (C) 2021-2022 r2c
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
open Xpattern_matcher
module PI = Parse_info

let logger = Logging.get_logger [ __MODULE__ ]

let lexing_pos_to_loc file x str =
  (* almost like Spacegrep.Semgrep.semgrep_pos() *)
  let line = x.Lexing.pos_lnum in
  let charpos = x.Lexing.pos_cnum in
  (* bugfix: not +1 here, Parse_info.column is 0-based.
   * JSON_report.json_range does the adjust_column + 1.
   *)
  let column = x.Lexing.pos_cnum - x.Lexing.pos_bol in
  { PI.str; charpos; file; line; column }

let spacegrep_matcher config (doc, src) file pat =
  let (config : Config_semgrep.t), _equivalences = config in
  let search_param =
    Spacegrep.Match.create_search_param
      ~ellipsis_max_span:config.generic_ellipsis_max_span ()
  in
  let matches = Spacegrep.Match.search search_param src pat doc in
  matches
  |> Common.map (fun m ->
         let (pos1, _), (_, pos2) = m.Spacegrep.Match.region in
         let { Spacegrep.Match.value = str; _ } = m.Spacegrep.Match.capture in
         let env =
           m.Spacegrep.Match.named_captures
           |> Common.map (fun (s, capture) ->
                  let mvar = "$" ^ s in
                  let { Spacegrep.Match.value = str; loc = pos, _ } = capture in
                  let loc = lexing_pos_to_loc file pos str in
                  let t = info_of_token_location loc in
                  let mval = mval_of_string str t in
                  (mvar, mval))
         in
         let loc1 = lexing_pos_to_loc file pos1 str in
         let loc2 = lexing_pos_to_loc file pos2 "" in
         ((loc1, loc2), env))

(* Preprocess spacegrep pattern or target to remove comments *)
let preprocess_spacegrep ((config : Config_semgrep.t), _equivalences) src =
  match config.generic_comment_style with
  | None -> src
  | Some style ->
      let style =
        match style with
        | `C -> Spacegrep.Comment.c_style
        | `Cpp -> Spacegrep.Comment.cpp_style
        | `Shell -> Spacegrep.Comment.shell_style
      in
      Spacegrep.Comment.remove_comments_from_src style src

let matches_of_spacegrep config spacegreps file =
  matches_of_matcher spacegreps
    {
      init =
        (fun _ ->
          (* coupling: mostly copypaste of Spacegrep_main.run_all *)
          (*
           We inspect the first 4096 bytes to guess whether the file type.
           This saves time on large files, by reading typically just one
           block from the file system.
          *)
          let peek_length = 4096 in
          let partial_doc_src =
            Spacegrep.Src_file.of_file ~max_len:peek_length file
          in
          let doc_type = Spacegrep.File_type.classify partial_doc_src in
          match doc_type with
          | Minified
          | Binary ->
              logger#info "ignoring gibberish file: %s\n%!" file;
              None
          | _ ->
              let src =
                if
                  Spacegrep.Src_file.length partial_doc_src < peek_length
                  (* it's actually complete, no need to re-input the file *)
                then partial_doc_src
                else Spacegrep.Src_file.of_file file
              in
              let src = preprocess_spacegrep config src in
              (* pr (Spacegrep.Doc_AST.show doc); *)
              Some (Spacegrep.Parse_doc.of_src src, src));
      matcher = spacegrep_matcher config;
    }
    file
  [@@profiling]
