(* Brandon Wu
 *
 * Copyright (C) 2019-2023 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Custom LSP request for a command which will show the Semgrep
   Generic AST of the given file.

   This file largely follows the structure of ocaml-lsp, such as:
   https://github.com/ocaml/ocaml-lsp/blob/master/ocaml-lsp-server/src/custom_requests/req_wrapping_ast_node.ml
*)

(*****************************************************************************)
(* Request parameters *)
(*****************************************************************************)

module Request_params = struct
  type t = { document_uri : Uri.t; named : bool }

  let _params_schema =
    `Assoc [ ("uri", `String "<DocumentUri>"); ("named", `Bool false) ]

  let of_jsonrpc_params params : t option =
    match params with
    | Some (`Assoc [ ("uri", uri); ("named", `Bool named) ]) ->
        let document_uri = Uri.t_of_yojson uri in
        Some { document_uri; named }
    | Some (`Assoc [ ("named", `Bool named); ("uri", uri) ]) ->
        let document_uri = Uri.t_of_yojson uri in
        Some { document_uri; named }
    | __else__ -> None

  let of_jsonrpc_params_exn params : t =
    match of_jsonrpc_params params with
    | None -> failwith "expected jsonrpc schema matching showast"
    | Some res -> res
end

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let meth = "semgrep/showAst"

let on_request session id (params : Jsonrpc.Structured.t option) =
  let { Request_params.document_uri; named } =
    Request_params.of_jsonrpc_params_exn params
  in
  let file = Uri.to_path document_uri |> Fpath.v in
  let lang = Lang.lang_of_filename_exn file in
  (* copied from -dump_ast *)
  let { Parsing_result2.ast; _ } =
    if named then Parse_target.parse_and_resolve_name lang file
    else Parse_target.just_parse_with_lang lang file
  in
  let v = Meta_AST.vof_any (AST_generic.Pr ast) in
  (* 80 columns is too little *)
  Format.set_margin 120;
  let s = OCaml.string_of_v v in
  (session, Lsp_.Reply.now (Lsp_.respond_json id (`String s)))
