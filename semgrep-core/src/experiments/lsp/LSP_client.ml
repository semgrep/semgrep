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

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* LSP client to extract type information for Semgrep (via semgrep -lsp).
 *
 * This currently only works for ocaml-lsp-server.
 *
 * If ocamllsp does not return the right information, you can debug things
 * by compiling ocaml-lsp from source and modifying the code to display
 * more debug information (it would be great if ocamllsp had a -verbose flag).
 * Clone the ocaml-lsp repo, and then modify
 * ocaml-lsp-server/src/vendor/../logger.ml with
 *   let destination = ref (Some stderr)
 * and use this server (target 2 below in the server global).
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)
type env = { io : Io.t option; last_uri : Uri.t }

let global = ref { io = None; last_uri = Uri.of_path "" }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let server =
  match 0 with
  | 0 -> "ocamllsp"
  | 1 -> "/home/pad/.opam/4.12.0/bin/ocamllsp"
  (* when instrumenting ocamllsp to debug things server-side *)
  | 2 ->
      "/home/pad/work/lang-ocaml/ocaml-lsp/_build/default/ocaml-lsp-server/src/main.exe"
  | 10 -> "/home/pad/go/bin/go-langserver"
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
    | TextDocumentDefinition _ -> "textDocument/definition"
    | _ -> assert false

  let params (type a) (t : a t) =
    Jsonrpc.Message.Structured.of_json
      (match t with
      | Initialize params -> InitializeParams.yojson_of_t params
      | ExecuteCommand params -> ExecuteCommandParams.yojson_of_t params
      | DebugEcho params -> DebugEcho.Params.yojson_of_t params
      | TextDocumentHover params -> HoverParams.yojson_of_t params
      | TextDocumentDefinition params -> DefinitionParams.yojson_of_t params
      | Shutdown -> raise Impossible
      | _ -> assert false)

  let to_jsonrpc_request t ~id =
    let method_ = method_ t in
    if method_ = "shutdown" then Jsonrpc.Message.create ~id ~method_ ()
    else
      let params = params t in
      Jsonrpc.Message.create ~id ~method_ ~params ()

  let response_of_json (type a) (t : a t) (json : Json.t) : a =
    match t with
    | Initialize _ -> InitializeResult.t_of_yojson json
    | ExecuteCommand _ -> json
    | DebugEcho _ -> DebugEcho.Result.t_of_yojson json
    | TextDocumentHover _ -> Json.Option.t_of_yojson Hover.t_of_yojson json
    | TextDocumentDefinition _ ->
        Json.Option.t_of_yojson Locations.t_of_yojson json
    | x ->
        logger#error "Response TODO: %s" (Common.dump x);
        failwith "TODO"
end

let send_request req io =
  incr counter;
  logger#info "send_request %d" !counter;
  let id = `Int !counter in
  let json_rpc = Client_request2.to_jsonrpc_request req ~id in
  let json = Jsonrpc.Message.yojson_of_request json_rpc in
  let either = Jsonrpc.Message.either_of_yojson json in
  let packet = Jsonrpc.Message either in
  Io.send io packet;
  Io.flush io;
  id

let send_notif notif io =
  let json_rpc = Client_notification.to_jsonrpc notif in
  let json_rpc = { json_rpc with Jsonrpc.Message.id = None } in
  let packet = Jsonrpc.Message json_rpc in
  Io.send io packet;
  Io.flush io

let rec read_response (id, req) io =
  logger#info "read_response %s" (Common.dump id);
  let res = Io.read io in
  match res with
  | None -> failwith "no answer"
  | Some res -> (
      match res with
      | Jsonrpc.Message
          { Jsonrpc.Message.method_ = "textDocument/publishDiagnostics"; _ } ->
          read_response (id, req) io
      | Jsonrpc.Message _ ->
          logger#error "Message TODO: %s" (Common.dump res);
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
              failwith (spf "error: %s" s)))

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

let def_at_tok tk uri io =
  let line = PI.line_of_info tk in
  let col = PI.col_of_info tk in
  logger#debug "def_at_tok: %d, %d" line col;
  (* LSP is using 0-based lines and offset (column) *)
  let line = line - 1 in

  let req =
    Client_request.TextDocumentDefinition
      (DefinitionParams.create
         ~textDocument:(TextDocumentIdentifier.create ~uri)
         ~position:(Position.create ~line ~character:col)
         ())
  in
  let id = send_request req io in
  let res = read_response (id, req) io in
  logger#info "%s" (Common.dump res);
  match res with
  | None ->
      logger#error "NO TYPE INFO for %s" (PI.string_of_info tk);
      None
  | Some (`Location [ x ]) ->
      let uri = x.Location.uri in
      let path = Uri.to_path uri in
      (* less: could also extract the range info in x.range *)
      Some path
  | Some (`LocationLink _ | `Location _) ->
      logger#error "too many location for %s" (PI.string_of_info tk);
      None

let type_at_tok tk uri io =
  let line = PI.line_of_info tk in
  let col = PI.col_of_info tk in
  logger#debug "type_at_tok: %d, %d" line col;
  (* LSP is using 0-based lines and offset (column) *)
  let line = line - 1 in

  let req =
    Client_request.TextDocumentHover
      (HoverParams.create
         ~textDocument:(TextDocumentIdentifier.create ~uri)
         ~position:(Position.create ~line ~character:col)
         ())
  in
  let id = send_request req io in
  let res = read_response (id, req) io in
  logger#info "%s" (Common.dump res);
  match res with
  | None ->
      logger#error "NO TYPE INFO for %s" (PI.string_of_info tk);
      None
  | Some { Hover.contents = x; _ } -> (
      match x with
      | `MarkupContent { MarkupContent.value = s; kind = _ } ->
          logger#info "%s" (Common.dump x);
          let s = final_type_string s in
          let ty =
            try
              let ty = Parse_ml.type_of_string s in
              match Ml_to_generic.any (Ast_ml.T ty) with
              | G.T ty -> ty
              | _ -> raise Impossible
            with
            | exn ->
                let e = Exception.catch exn in
                logger#error "Exn Parse_ml.type_of_string TODO: %s" s;
                Exception.reraise e
          in
          (* logger#info "type = %s" (G.show_type_ ty); *)
          Some ty
      | _ -> failwith "not a `MarkedContent`")

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let connect_server () =
  (* the PWD of the server process is used to look for the .cmt so
   * run this program from the project you want to analyze *)
  let inc, outc = Unix.open_process server in
  let io = Io.make inc outc in
  let req = init_request (Uri.of_path "") in
  let id = send_request req io in
  let res = read_response (id, req) io in
  logger#info "connect_server: %s" (Common.dump res);
  let notif = Client_notification.Initialized in
  send_notif notif io;
  io

let rec get_type_or_def f id =
  let tok = snd id in
  let file = PI.file_of_info tok in
  (* bugfix: ocamllsp use URIs to designate files, but it's impossible
   * to use relative paths in URIs, so you need to use the absolute path,
   * otherwise ocamlmerlin code (used internally by ocamllsp) will not
   * find the .cmt corresponding to the file.
   *)
  let fullpath = Common.fullpath file in
  let uri = Uri.of_path fullpath in
  match !global with
  | { io = Some io; last_uri } when Uri.equal last_uri uri -> (
      try f tok uri io with
      | exn ->
          logger#info "exn: %s" (Common.exn_to_s exn);
          None)
  | { io = Some io; last_uri } when not (Uri.equal last_uri uri) ->
      (if not (Uri.equal last_uri (Uri.of_path "")) then
       let notif =
         Client_notification.TextDocumentDidClose
           (DidCloseTextDocumentParams.create
              ~textDocument:(TextDocumentIdentifier.create ~uri))
       in
       send_notif notif io);

      let notif =
        Client_notification.TextDocumentDidOpen
          (DidOpenTextDocumentParams.create
             ~textDocument:
               (TextDocumentItem.create ~uri
                  ~text:(Common.read_file fullpath)
                  ~version:1 ~languageId:"ocaml"))
      in
      send_notif notif io;
      global := { !global with last_uri = uri };
      logger#info "TextDocumentDidOpen for uri %s" (Uri.to_string uri);
      (* try again *)
      get_type_or_def f id
  | _ -> None

let init () =
  logger#info "init";
  let io = connect_server () in
  global := { io = Some io; last_uri = Uri.of_path "" };
  Hooks.get_type := get_type_or_def type_at_tok;
  Hooks.get_def := get_type_or_def def_at_tok;
  Hooks.exit
  |> Common.push (fun () ->
         logger#info "closing";
         send_request Client_request.Shutdown io |> ignore;
         Io.close io);
  ()
