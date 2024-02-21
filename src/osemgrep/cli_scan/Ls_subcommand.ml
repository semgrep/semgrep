(*
   List files that semgrep treats as targets before rule-specific
   or language-specific filtering.
*)

open Common
module OutJ = Semgrep_output_v1_j

let run ~target_roots ~targeting_conf:conf () =
  let selected, skipped = Find_targets.get_targets conf target_roots in
  selected |> List.sort Fppath.compare
  |> List.iter (fun (x : Fppath.t) ->
         Out.put (spf "selected %s" (Fpath.to_string x.fpath)));
  skipped
  |> List.sort (fun (a : OutJ.skipped_target) (b : OutJ.skipped_target) ->
         Fpath.compare a.path b.path)
  |> List.iter (fun (x : OutJ.skipped_target) ->
         Out.put
           (spf "ignored %s [%s]" (Fpath.to_string x.path)
              (x.reason |> OutJ.string_of_skip_reason
             |> JSON.remove_enclosing_quotes_of_jstring)));
  Exit_code.ok ~__LOC__
