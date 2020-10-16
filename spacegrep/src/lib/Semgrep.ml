(*
   Convert matches to the format expected by the semgrep wrapper,
   similar to what semgrep-core produces.
*)

open Match
open Semgrep_t

let semgrep_pos (x : Lexing.position) : Semgrep_t.position =
  {
    line = x.pos_lnum + 1;
    col = x.pos_cnum - x.pos_bol + 1;
    offset = x.pos_cnum;
  }

let unique_id_of_loc (loc : Loc.t) : unique_id =
  let md5sum =
    Marshal.to_string loc []
    |> Digest.string
    |> Digest.to_hex
  in
  {
    type_ = `AST;
    md5sum;
  }

let convert_capture x =
  let pos1, pos2 = x.loc in
  x.name, {
    start = semgrep_pos pos1;
    end_ = semgrep_pos pos2;
    abstract_content = x.value;
    unique_id = unique_id_of_loc x.loc;
  }

(*
   Convert match results to the format expected by semgrep.
*)
let make_semgrep_json doc_matches : Semgrep_t.match_results =
  let matches =
    List.map (fun (src, pat_matches) ->
      let path = Src_file.source_string src in
      List.map (fun (pat_id, matches) ->
        let check_id = Some (string_of_int pat_id) in
        List.map (fun match_ ->
          let ((pos1, _), (_, pos2)) = match_.region in
          let metavars = List.map convert_capture match_.captures in
          let extra = {
            message = None;
            metavars;
          } in
          ({
            check_id;
            path;
            start = semgrep_pos pos1;
            end_ = semgrep_pos pos2;
            extra;
          } : match_)
        ) matches
      ) pat_matches
      |> List.flatten
    ) doc_matches
    |> List.flatten
  in
  {
    matches;
    errors = [];
    stats = {
      okfiles = List.length doc_matches;
      errorfiles = 0;
    };
  }

let print_semgrep_json doc_matches =
  make_semgrep_json doc_matches
  |> Semgrep_j.string_of_match_results
  |> Yojson.Safe.prettify
  |> print_endline
