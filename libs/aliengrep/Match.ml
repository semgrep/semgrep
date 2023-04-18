(*
   Match a compiled pattern against a target string.
*)

type loc = { start : int; length : int; substring : string }

type match_ = {
  match_loc : loc;
  captures : (Pat_compile.metavariable * loc) list;
}

(* 0: whole match, i >= 1: captures *)
let loc_of_substring target_str substrings i =
  let start, end_ = Pcre.get_substring_ofs substrings i in
  let length = end_ - start in
  assert (start >= 0);
  assert (length >= 0);
  assert (end_ <= String.length target_str);
  { start; length; substring = String.sub target_str start length }

let convert_match (pat : Pat_compile.t) target_str
    (substrings : Pcre.substrings) =
  let match_loc = loc_of_substring target_str substrings 0 in
  let captures =
    Array.mapi
      (fun i mv ->
        let loc = loc_of_substring target_str substrings (i + 1) in
        (mv, loc))
      pat.metavariable_groups
    |> Array.to_list
  in
  { match_loc; captures }

let search (pat : Pat_compile.t) target_str : match_ list =
  SPcre.exec_all_noerr ~rex:pat.pcre target_str
  |> Array.to_list
  |> Common.map (convert_match pat target_str)
