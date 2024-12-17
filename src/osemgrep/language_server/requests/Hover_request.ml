(* Brandon Wu
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Lsp
open Types
open RPC_server
module CN = Client_notification
module CR = Client_request
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let on_request server ({ position; textDocument; _ } : HoverParams.t) =
  if not server.session.user_settings.do_hover then
    (* If `do_hover` is not enabled from the user's settings, then
        return a null response, indicating that we do not show any
        content.
        If you return `None` instead, the client will still be
        expecting a response!
    *)
    (server, Some `Null)
  else
    let file = Uri.to_path textDocument.uri |> Fpath.v in
    (* Add 1 to each list for the newline! *)
    let base_charpos =
      let contents = UFile.cat file in
      let lines =
        contents |> List_.index_list
        |> List.filter (fun (_, idx) -> idx < position.line)
      in
      lines
      |> List_.map (fun (l, _) -> String.length l + 1)
      |> List.fold_left ( + ) 0
    in
    let charpos = base_charpos + position.character in
    let lang = Lang.lang_of_filename_exn file in
    (* copied from -dump_ast *)
    let { Parsing_result2.ast; _ } =
      Parse_target.parse_and_resolve_name lang file
      (* else Parse_target.just_parse_with_lang lang file
        *)
    in
    let res = AST_generic_helpers.nearest_any_of_pos ast charpos in
    match res with
    | None -> (server, Some `Null)
    | Some (any, (t1, t2)) ->
        let v = Meta_AST.vof_any any in
        (* 80 columns is too little *)
        Format.set_margin 120;
        let s = OCaml.string_of_v v in
        let end_line, end_col, _ = Tok.end_pos_of_loc t2 in
        let hover =
          Hover.
            {
              contents = `MarkedString { language = Some "OCaml"; value = s };
              range =
                Some
                  {
                    (* Subtract one for each line, because we want to switch to
                        0-indexing
                    *)
                    start =
                      { character = t1.pos.column; line = t1.pos.line - 1 };
                    end_ = { character = end_col; line = end_line - 1 };
                  };
            }
        in
        (server, Some (Hover.yojson_of_t hover))
