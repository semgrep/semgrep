(*
   Match a compiled pattern against a target string.
*)
module Log = Log_aliengrep.Log

(* Suppresses warnings about PCRE; there appear to be some issues when moving
   this to use PCRE2 on some versions, e.g., 10.34 (Debian stable as of
   2024-04-18). *)
[@@@alert "-deprecated"]

type loc = { start : int; length : int; substring : string } [@@deriving show]

type match_ = {
  match_loc : loc;
  captures : (Pat_compile.metavariable * loc) list;
}
[@@deriving show]

type matches = match_ list [@@deriving show]

let loc_of_substring target_str substrings capture_id =
  let start, end_ =
    try Pcre.get_substring_ofs substrings capture_id with
    | Not_found ->
        (* bug! Did you introduce capturing groups by accident by inserting
           plain parentheses (XX) instead of (?:XX) ? *)
        (* "corresponding subpattern did not capture a substring" *)
        Log.err (fun m ->
            m "failed to extract capture %i. Captures are [%s]" capture_id
              (Pcre.get_substrings substrings
              |> Array.to_list
              |> List_.map (Printf.sprintf "%S")
              |> String.concat ";"));
        assert false
  in
  let length = end_ - start in
  assert (start >= 0);
  assert (length >= 0);
  assert (end_ <= String.length target_str);
  { start; length; substring = String.sub target_str start length }

let convert_match (pat : Pat_compile.t) target_str
    (substrings : Pcre.substrings) =
  let match_loc = loc_of_substring target_str substrings 0 in
  let captures =
    List_.map
      (fun (capture_id, mv) ->
        let loc = loc_of_substring target_str substrings capture_id in
        Log.debug (fun m ->
            m "captured metavariable %s = %S"
              (Pat_compile.show_metavariable mv)
              loc.substring);
        (mv, loc))
      pat.metavariable_groups
  in
  { match_loc; captures }

let search (pat : Pat_compile.t) target_str : match_ list =
  Pcre_.exec_all_noerr ~rex:pat.pcre target_str
  |> Array.to_list
  |> List_.map (convert_match pat target_str)
