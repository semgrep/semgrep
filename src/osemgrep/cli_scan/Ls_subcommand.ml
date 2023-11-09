(*
   List files that semgrep treats as targets before rule-specific
   or language-specific filtering.
*)

open Common
module Out = Semgrep_output_v1_j

let string_of_json_string json =
  match Yojson.Safe.from_string json with
  | `String str -> str
  | _ -> (* should not happen *) json

let run ~target_roots ~targeting_conf:conf () =
  let selected, skipped = Find_targets.get_targets conf target_roots in
  selected |> List.sort Fpath.compare
  |> List.iter (fun (path : Fpath.t) -> pr (spf "+ %s" (Fpath.to_string path)));
  skipped
  |> List.sort (fun (a : Out.skipped_target) (b : Out.skipped_target) ->
         Fpath.compare a.path b.path)
  |> List.iter (fun (x : Out.skipped_target) ->
         pr
           (spf "- [%s] %s"
              (x.reason |> Out.string_of_skip_reason |> string_of_json_string)
              (Fpath.to_string x.path)));
  Exit_code.ok
