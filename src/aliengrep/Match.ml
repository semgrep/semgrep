(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Match a compiled pattern against a target string.
*)
module Log = Log_aliengrep.Log

type loc = { start : int; length : int; substring : string } [@@deriving show]

type match_ = {
  match_loc : loc;
  captures : (Pat_compile.metavariable * loc) list;
}
[@@deriving show]

type matches = match_ list [@@deriving show]

let loc_of_substring target_str captures capture_id =
  let start, end_ =
    match Pcre2_.match_of_captures captures capture_id with
    | Some m ->
        let Pcre2_.{ start; end_ } = Pcre2_.range_of_match m in
        (start, end_)
    | None ->
        (* bug! Did you introduce capturing groups by accident by inserting
           plain parentheses (XX) instead of (?:XX) ? *)
        (* "corresponding subpattern did not capture a substring" *)
        Log.err (fun m ->
            m "failed to extract capture %i. Captures are [%s]" capture_id
              (List.init (Pcre2_.captures_length captures) (fun i ->
                   Pcre2_.match_of_captures captures i
                   |> Option.map Pcre2_.substring_of_match
                   |> Option.value ~default:"")
                 (* nosemgrep: ocaml.lang.best-practice.string.ocamllint-useless-sprintf *)
              |> List.map (Printf.sprintf "%S")
              |> String.concat ";"));
        assert false
  in
  let length = end_ - start in
  assert (start >= 0);
  assert (length >= 0);
  assert (end_ <= String.length target_str);
  { start; length; substring = String.sub target_str start length }

let convert_match (pat : Pat_compile.t) target_str (captures : Pcre2_.captures)
    =
  let match_loc = loc_of_substring target_str captures 0 in
  let captures =
    List.map
      (fun (capture_id, mv) ->
        let loc = loc_of_substring target_str captures capture_id in
        Log.debug (fun m ->
            m "captured metavariable %s = %S"
              (Pat_compile.show_metavariable mv)
              loc.substring);
        (mv, loc))
      pat.metavariable_groups
  in
  { match_loc; captures }

let search (pat : Pat_compile.t) target_str : match_ list =
  Pcre2_.captures_iter pat.pcre target_str
  |> Seq.filter_map (function
    | Ok captures -> Some captures
    | Error err ->
        Log.err (fun m ->
            m "PCRE2 error while matching aliengrep pattern: %a"
              Pcre2.pp_match_error err);
        None)
  |> Seq.map (convert_match pat target_str)
  |> List.of_seq
