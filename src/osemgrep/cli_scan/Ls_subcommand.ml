(*
   List files that semgrep treats as targets before rule-specific
   or language-specific filtering.
*)

open Common
module OutJ = Semgrep_output_v1_j

(*
   Print just the paths (like 'ls') or print a bunch of details (like 'ls -l')
*)
type format = Paths_only | Long [@@deriving show]

let default_format = Paths_only

let run (caps : < Cap.readdir ; .. >) ~target_roots ~targeting_conf:conf ~format
    =
  let selected, errors, skipped =
    Find_targets.get_target_fpaths caps conf target_roots
  in
  selected |> List.sort Fpath.compare
  |> List.iter (fun (x : Fpath.t) ->
         match format with
         | Paths_only -> UConsole.print (Fpath.to_string x)
         | Long -> UConsole.print (spf "selected %s" (x |> Fpath.to_string)));
  (match format with
  | Paths_only -> ()
  | Long ->
      skipped
      |> List.sort (fun (a : OutJ.skipped_target) (b : OutJ.skipped_target) ->
             Fpath.compare a.path b.path)
      |> List.iter (fun (x : OutJ.skipped_target) ->
             UConsole.print
               (spf "ignored %s [%s]" (Fpath.to_string x.path)
                  (x.reason |> OutJ.string_of_skip_reason
                 |> JSON.remove_enclosing_quotes_of_jstring))));
  match errors with
  | [] -> Exit_code.ok ~__LOC__
  | _ -> Exit_code.fatal ~__LOC__
