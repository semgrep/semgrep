(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.
*)
(* Read external rule test expectations, keeping paths independent of the
   invocation directory. Each entry replaces all annotated expectations for
   exactly one target, rule, and engine; other entries retain their annotations. *)

module A = Test_annotation

let ( let* ) = Result.bind

module Json = struct
  type entry = {
    path : Fpath.t; [@to_yojson Fpath_.to_yojson] [@of_yojson Fpath_.of_yojson]
    rule_id : string;
    engine : string;
    lines : int list;
  }
  [@@deriving yojson]

  type t = entry list [@@deriving yojson]
end

type entry = {
  path : Fpath.t;
  rule_id : Rule_ID.t;
  engine : A.engine;
  lines : int list;
}

type t = entry list

let empty = []

let load file =
  let invalid message = Error (`Msg message) in
  let validate_entry (entry : Json.entry) =
    let path = Fpath.(parent file // entry.path) in
    (* Lexical normalization also works for entries outside the selected
       test subset, whose submodule may not be checked out. *)
    let path =
      if Fpath.is_abs path then path else Fpath.(v (Sys.getcwd ()) // path)
    in
    let path = Fpath.normalize path in
    let* rule_id =
      match Rule_ID.of_string_opt entry.rule_id with
      | Some id -> Ok id
      | None -> invalid "invalid rule_id"
    in
    let* engine =
      match entry.engine with
      | "OSS" -> Ok A.OSS
      | "Pro" -> Ok A.Pro
      | "Deep" -> Ok A.Deep
      | _ -> invalid "engine must be OSS, Pro, or Deep"
    in
    if List.exists (fun line -> line <= 0) entry.lines then
      invalid "lines must contain positive integers"
    else
      let lines = List.sort_uniq Int.compare entry.lines in
      Ok { path; rule_id; engine; lines }
  in
  let read () =
    let* entries =
      try
        Yojson.Safe.from_file (Fpath.to_string file)
        |> Json.of_yojson
        |> Result.map_error (fun message -> `Msg message)
      with
      | Yojson.Json_error message
      | Sys_error message ->
          invalid message
    in
    let* entries = Result_.list_map validate_entry entries in
    let seen = Base.Hashtbl.Poly.create () in
    Result_.list_map
      (fun ({ path; rule_id; engine; _ } as entry) ->
        let key = (path, rule_id, engine) in
        if Base.Hashtbl.mem seen key then
          invalid "duplicate target/rule/engine entry"
        else (
          Base.Hashtbl.set seen ~key ~data:();
          Ok entry))
      entries
  in
  read ()
  |> Result.map_error (fun (`Msg message) ->
      `Msg
        (Printf.sprintf "Invalid test expectations in %s: %s"
           (Fpath.to_string file) message))

let for_rules entries ids =
  List.filter (fun entry -> List.mem entry.rule_id ids) entries

let for_file entries engine file =
  let path =
    if Fpath.is_abs file then file else Fpath.(v (Sys.getcwd ()) // file)
  in
  let path = Fpath.normalize path in
  List.filter_map
    (fun entry ->
      if entry.engine = engine && Fpath.equal entry.path path then
        Some (entry.rule_id, entry.lines)
      else None)
    entries
