(*
   Target discovery can now be done in semgrep-core using scanning roots
   passed by pysemgrep.

   This module takes care of splitting targets by language as requested
   by the legacy semgrep-core input interface.
*)

open Common

(*************************************************************************)
(* Extract mode *)
(*************************************************************************)

(* TODO? move to Analyzer.ml? *)
module AnalyzerSet = Set.Make (struct
  let compare
      (* This only compares the first language in the case of `L (lang :: _)`.
         That should be fine because for the use of Analyzer in this file,
         `L _` should always be flattened out *)
        a b =
    String.compare (Analyzer.to_string a) (Analyzer.to_string b)

  type t = Analyzer.t
end)

(* Extract mode: we need to make sure to include rules that will apply
   to targets that have been extracted. To do that, we'll detect the languages
   that targets might be extracted to and include them in the jobs *)
(* TODO it would be nicer to just extract the targets before splitting jobs,
   but that would cause us to change things for the Core_scan shared path *)
let detect_extract_languages all_rules =
  all_rules
  |> List.fold_left
       (fun acc { Rule.mode; _ } ->
         match mode with
         | `Extract { Rule.dst_lang; _ } -> AnalyzerSet.add dst_lang acc
         | _ -> acc)
       AnalyzerSet.empty

(* The same rule may appear under multiple target languages because
   some patterns can be interpreted in multiple languages.
*)
let group_rules_by_target_language (rules : Rule.t list) :
    (Analyzer.t * Rule.t list) list =
  (* target language -> rules *)
  (* TODO: use Assoc.group_by *)
  let tbl = Hashtbl.create 100 in
  rules
  |> List.iter (fun (rule : Rule.t) ->
         let pattern_lang = rule.target_analyzer in
         let target_langs = Analyzer.flatten pattern_lang in
         target_langs
         |> List.iter (fun lang ->
                let rules =
                  match Hashtbl.find_opt tbl lang with
                  | None -> []
                  | Some rules -> rules
                in
                Hashtbl.replace tbl lang (rule :: rules)));
  Hashtbl.fold (fun lang rules acc -> (lang, rules) :: acc) tbl []

(* If Javascript is one of the rule languages, we should also run on
   Typescript files. This implementation mimics the hack in `rule.py`.
   We could alternatively set this by changing lang.json, but we should
   be careful to do that without affecting other things like the docs
*)
let add_typescript_to_javascript_rules_hack (rules : Rule.t list) : Rule.t list
    =
  rules
  |> List_.map (fun r ->
         match r.Rule.target_analyzer with
         | LRegex
         | LSpacegrep
         | LAliengrep ->
             r
         | L (l, ls) ->
             let lset = Set_.of_list ls in
             let lset =
               if l =*= Language.Js || Set_.mem Language.Js lset then
                 Set_.add Language.Ts lset
               else lset
             in
             { r with Rule.target_analyzer = L (l, lset |> Set_.elements) })

let split_jobs_by_language (conf : Find_targets.conf) (rules : Rule.t list)
    (targets : Fpath.t list) : Lang_job.t list =
  let rules = add_typescript_to_javascript_rules_hack rules in
  let extract_languages = detect_extract_languages rules in
  rules |> group_rules_by_target_language
  |> List_.filter_map (fun (analyzer, rules) ->
         let targets =
           targets
           |> List.filter (fun path ->
                  (* bypass normal analyzer detection for explicit targets with
                     '--scan-unknown-extensions' *)
                  let bypass_language_detection =
                    conf.always_select_explicit_targets
                    && Find_targets.Explicit_targets.mem conf.explicit_targets
                         path
                  in
                  bypass_language_detection
                  || Filter_target.filter_target_for_analyzer analyzer path)
         in
         if
           List_.null targets
           && not (AnalyzerSet.mem analyzer extract_languages)
         then None
         else Some ({ analyzer; targets; rules } : Lang_job.t))

let targets_of_lang_job (x : Lang_job.t) : Target.t list =
  x.targets
  |> List_.map (fun (path : Fpath.t) : Target.t ->
         Target.mk_target_fpath x.analyzer path)

let targets_and_rules_of_lang_jobs (lang_jobs : Lang_job.t list) :
    Target.t list * Rule.t list =
  let targets, rules =
    List_.fold_right
      (fun lang_job (acc_targets, acc_rules) ->
        let targets = targets_of_lang_job lang_job in
        let rules = lang_job.rules in
        (List_.append targets acc_targets, List_.append rules acc_rules))
      lang_jobs ([], [])
  in
  (* TODO: deduplicate rules? *)
  (targets, rules)

let targets_for_files_and_rules (files : Fpath.t list) (rules : Rule.t list) :
    Target.t list =
  let conf = Find_targets.default_conf in
  let lang_jobs = split_jobs_by_language conf rules files in
  lang_jobs |> List.concat_map targets_of_lang_job
