(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Filter target candidates.

   TODO: Handles the paths: include/exclude

   Filtering each (rule, target) pair can become problematic since the number
   of such pairs is O(number of targets * number of rules).
   TODO? This is why we should cache the results of this step.
   This allows reducing the number of rules to the number of different
   languages and patterns used by the rules.
   update: there used to be such an opti, but I removed it, because
   it was not clear that was the actual bottleneck. Gitignore
   seems currently to be the thing to optimize.

   Partially translated from target_manager.py
*)

(*************************************************************************)
(* Types *)
(*************************************************************************)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(*************************************************************************)
(* Entry point *)
(*************************************************************************)

(* Used by Core_runner.split_jobs_by_language *)
let filter_target_for_xlang (xlang : Xlang.t) (path : Fpath.t) : bool =
  match xlang with
  | L (lang, langs) ->
      (* ok if the file appears to be in one of rule's languages *)
      lang :: langs
      |> List.exists (fun lang -> Guess_lang.inspect_file_p lang path)
  | LRegex
  | LGeneric ->
      true
