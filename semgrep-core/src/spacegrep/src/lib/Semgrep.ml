(*
   Convert matches to the format expected by the semgrep wrapper,
   similar to what semgrep-core produces.
*)

open Match
open Semgrep_t

let semgrep_pos (x : Lexing.position) : Semgrep_t.position =
  {
    (* both 'line' and 'pos_lnum' start from 1. *)
    line = x.pos_lnum;
    (* 'col' starts from 1, pos_cnum/pos_bol start from 0. *)
    col = x.pos_cnum - x.pos_bol + 1;
    offset = x.pos_cnum;
  }

let unique_id_of_capture ~captured_string : unique_id =
  let md5sum = captured_string |> Digest.string |> Digest.to_hex in
  { type_ = `AST; md5sum = Some md5sum; sid = None }

let convert_capture (name, x) =
  assert (name <> "" && name.[0] <> '$');
  let pos1, pos2 = x.loc in
  ( "$" ^ name,
    {
      start = semgrep_pos pos1;
      end_ = semgrep_pos pos2;
      abstract_content = x.value;
      unique_id = unique_id_of_capture ~captured_string:x.value;
    } )

let make_target_time (src, parse_time, match_time, run_time) :
    Semgrep_t.target_time =
  { path = Src_file.source_string src; parse_time; match_time; run_time }

(*
   Convert match results to the format expected by semgrep.
*)
let make_semgrep_json ~with_time doc_matches pat_errors :
    Semgrep_t.match_results =
  let matches, match_times =
    List.map
      (fun (src, pat_matches, parse_time, run_time) ->
        let path = Src_file.source_string src in
        let matches, match_time =
          List.fold_right
            (fun (pat_id, matches, match_time) (matches_acc, match_time_acc) ->
              let check_id = Some (string_of_int pat_id) in
              let matches_out =
                List.map
                  (fun match_ ->
                    let (pos1, _), (_, pos2) = match_.region in
                    let metavars =
                      List.map convert_capture match_.named_captures
                    in
                    let lines =
                      Src_file.list_lines_of_pos_range src pos1 pos2
                    in
                    let extra = { message = None; metavars; lines } in
                    ( {
                        check_id;
                        path;
                        start = semgrep_pos pos1;
                        end_ = semgrep_pos pos2;
                        extra;
                      }
                      : match_ ))
                  matches
              in
              (matches_out @ matches_acc, match_time +. match_time_acc))
            pat_matches ([], 0.0)
        in
        (matches, (src, parse_time, match_time, run_time)))
      doc_matches
    |> List.split
  in
  let matches = List.flatten matches in
  let errors =
    pat_errors
    |> List.map (fun (src, error) ->
           let path = Src_file.source_string src in
           let pos1, pos2 = error.Parse_pattern.loc in
           let line =
             Src_file.list_lines_of_pos_range src pos1 pos2 |> List.hd
           in
           let extra = { message = error.Parse_pattern.msg; line } in
           {
             check_id = None;
             path;
             start = semgrep_pos pos1;
             end_ = semgrep_pos pos2;
             extra;
           })
  in
  let time =
    if with_time then
      Some
        {
          targets = List.map make_target_time match_times;
          rule_parse_time = None;
        }
    else None
  in
  {
    matches;
    errors;
    stats = { okfiles = List.length doc_matches; errorfiles = 0 };
    time;
  }

let print_semgrep_json ~with_time doc_matches errors =
  make_semgrep_json ~with_time doc_matches errors
  |> Semgrep_j.string_of_match_results |> print_endline
