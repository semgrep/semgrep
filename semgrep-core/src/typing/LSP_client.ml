(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module J = JSON
module C = Lsp.Client_request
open Lsp
open Types
module PI = Parse_info
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)
type env = { io : Io.t option; last_uri : string }

let global = ref { io = None; last_uri = "" }

let debug = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let server =
  match 1 with
  | 1 -> "/home/pad/.opam/4.09.1/bin/ocamllsp"
  | 2 -> "/home/pad/go/bin/go-langserver"
  | _ -> raise Impossible

(*****************************************************************************)
(* LSP library helpers *)
(*****************************************************************************)
let init_request path =
  let capabilities = ClientCapabilities.create () in
  let params =
    InitializeParams.create ~capabilities ~rootUri:path ~trace:`Verbose ()
  in
  let req = Client_request.Initialize params in
  req

let counter = ref 0

(* copy paste of Client_request filled with new cases *)
module Client_request2 = struct
  open Import
  open Client_request
  open Types
  open Extension

  let method_ (type a) (t : a t) =
    match t with
    | Initialize _ -> "initialize"
    | Shutdown -> "shutdown"
    | ExecuteCommand _ -> "workspace/executeCommand"
    | DebugEcho _ -> "debug/echo"
    | TextDocumentHover _ -> "textDocument/hover"
    | _ -> assert false

  let params (type a) (t : a t) =
    match t with
    | Initialize params -> InitializeParams.yojson_of_t params
    | ExecuteCommand params -> ExecuteCommandParams.yojson_of_t params
    | DebugEcho params -> DebugEcho.Params.yojson_of_t params
    | TextDocumentHover params -> HoverParams.yojson_of_t params
    | Shutdown -> `Null
    | _ -> assert false

  let to_jsonrpc_request t ~id =
    let method_ = method_ t in
    let params = params t in
    Jsonrpc.Message.create ~id ~method_ ~params ()

  let response_of_json (type a) (t : a t) (json : Json.t) : a =
    match t with
    | Initialize _ -> InitializeResult.t_of_yojson json
    | ExecuteCommand _ -> json
    | DebugEcho _ -> DebugEcho.Result.t_of_yojson json
    | TextDocumentHover _ -> Json.Option.t_of_yojson Hover.t_of_yojson json
    | x ->
        pr2_gen ("Response TODO", x);
        failwith "TODO"
end

let send_request req io =
  incr counter;
  let id = `Int !counter in
  let json_rpc = Client_request2.to_jsonrpc_request req ~id in
  let json = Jsonrpc.Message.yojson_of_request json_rpc in
  let either = Jsonrpc.Message.either_of_yojson json in
  let packet = Jsonrpc.Message either in
  Io.send io packet;
  id

let send_notif notif io =
  let json_rpc = Client_notification.to_jsonrpc notif in
  let json_rpc = { json_rpc with Jsonrpc.Message.id = None } in
  let packet = Jsonrpc.Message json_rpc in
  Io.send io packet

let rec read_response (id, req) io =
  let res = Io.read io in
  match res with
  | None -> failwith "no answer"
  | Some res -> (
      match res with
      | Jsonrpc.Message
          { Jsonrpc.Message.method_ = "textDocument/publishDiagnostics"; _ } ->
          read_response (id, req) io
      | Jsonrpc.Message _ ->
          pr2_gen ("Message TODO", res);
          failwith "got a message, not a Response"
      | Jsonrpc.Response { Jsonrpc.Response.id = id2; result } -> (
          if id2 <> id then
            failwith (spf "ids are different: %s" (Common.dump (id, id2)));
          match result with
          | Ok json ->
              let response = Client_request2.response_of_json req json in
              response
          | Error err ->
              let json = Jsonrpc.Response.Error.yojson_of_t err in
              let s = Import.Json.to_pretty_string json in
              failwith (spf "error: %s" s) ) )

(*****************************************************************************)
(* OCaml LSP get type *)
(*****************************************************************************)
let final_type_string s =
  (* remove type explanation for list, option, etc part 1 *)
  let s = Str.global_replace (Str.regexp "\n\n") "XXX" s in

  (* no need \n, easier to have one line per type *)
  let s = Str.global_replace (Str.regexp "\n") " " s in

  (* remove type explanation for list, option, etc part 2 *)
  let s = Str.global_replace (Str.regexp "XXX.*") " " s in

  let s =
    match s with
    | _ when s =~ "^type \\([^=]+\\)=.*" -> Common.matched1 s
    | _ when s =~ "^type \\(.+\\)" -> Common.matched1 s
    | _ when s =~ "^sig .*" -> "sig_TODO"
    | _ -> s
  in
  s

let type_at_tok tk uri io =
  let line = PI.line_of_info tk in
  let col = PI.col_of_info tk in
  if !debug then pr2_gen (line, col);
  (* LSP is using 0-based lines and offset (column) *)
  let line = line - 1 in

  let req =
    Client_request.TextDocumentHover
      (HoverParams.create
         ~textDocument:(TextDocumentIdentifier.create ~uri)
         ~position:(Position.create ~line ~character:col))
  in
  let id = send_request req io in
  let res = read_response (id, req) io in
  if !debug then pr2_gen res;
  match res with
  | None ->
      if !debug then pr2 (spf "NO TYPE INFO for %s" (PI.string_of_info tk));
      None
  | Some { Hover.contents = x; _ } -> (
      match x with
      | `MarkupContent { MarkupContent.value = s; kind = _ } ->
          if !debug then pr2_gen x;
          let s = final_type_string s in
          let ty =
            try
              let ty = Parse_ml.type_of_string s in
              match Ml_to_generic.any (Ast_ml.T ty) with
              | G.T ty -> ty
              | _ -> raise Impossible
            with exn ->
              pr2_gen ("Exn Parse_ml.type_of_string TODO", s);
              raise exn
          in
          if !debug then pr2_gen ty;
          Some ty
      | _ -> failwith "not a `MarkedContent`" )

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let connect_server () =
  (* the PWD of the server process is used to look for the .cmt so
   * run this program from the project you want to analyze *)
  let inc, outc = Unix.open_process server in
  let io = Io.make inc outc in
  let req = init_request "file:///" in
  let id = send_request req io in
  let res = read_response (id, req) io in
  if !debug then pr2_gen res;
  let notif = Client_notification.Initialized in
  send_notif notif io;
  io

let rec get_type id =
  let tok = snd id in
  let file = PI.file_of_info tok in
  let uri = "file://" ^ file in
  match !global with
  | { io = Some io; last_uri } when last_uri = uri -> (
      try type_at_tok tok uri io with _exn -> None )
  | { io = Some io; last_uri } when last_uri <> uri ->
      ( if last_uri <> "" then
        let notif =
          Client_notification.TextDocumentDidClose
            (DidCloseTextDocumentParams.create
               ~textDocument:(TextDocumentIdentifier.create ~uri))
        in
        send_notif notif io );

      let notif =
        Client_notification.TextDocumentDidOpen
          (DidOpenTextDocumentParams.create
             ~textDocument:
               (TextDocumentItem.create ~uri ~text:(Common.read_file file)
                  ~version:1 ~languageId:"ocaml"))
      in
      send_notif notif io;
      global := { !global with last_uri = uri };
      (* try again *)
      get_type id
  | _ -> None

let init () =
  if !debug then pr2 "LSP INIT";
  let io = connect_server () in
  global := { io = Some io; last_uri = "" };
  Hooks.get_type := get_type;
  Hooks.exit
  |> Common.push (fun () ->
         if !debug then pr2 "LSP CLOSING";
         send_request Client_request.Shutdown io |> ignore;
         Io.close io);
  ()
