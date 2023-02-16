(* Brandon Wu
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

module R = Rule
module PI = Parse_info
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A separate code path for running many taint rules on the same target,
 * simultaneously.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let modify_requires s ast =
  let mapper =
    Map_AST.mk_visitor
      {
        Map_AST.default_visitor with
        kname =
          (fun (_k, _vout) name ->
            match name with
            | G.Id ((label, tok), idinfo) -> G.Id ((s ^ label, tok), idinfo)
            | other -> other);
      }
  in
  mapper.vexpr ast

(* Given a taint specification, we have to ensure that it doesn't overlap with any
   of the other taint specifications that we are trying to combine it with.

   For instance, we might have two taint rules, both of which using the label
   "tainted". Then, if we combine them into a single rule, their taints will
   overlap with each other, and interfere.

   To fix this, we change all labels (and uses of those labels in `requires`) to
   simply be prepended with the ID of the rule in question.
*)
let mk_unique_spec r =
  let (`Taint spec) = r.R.mode in
  let rule_id = r.R.id |> fst in
  let modify_sources source =
    {
      source with
      R.label = rule_id ^ source.R.label;
      source_requires = modify_requires rule_id source.R.source_requires;
    }
  in
  let modify_sinks sink =
    { sink with R.sink_requires = modify_requires rule_id sink.R.sink_requires }
  in
  let sources = spec.R.sources |> snd |> Common.map modify_sources in
  let sinks = spec.R.sinks |> snd |> Common.map modify_sinks in
  (sources, spec.sanitizers, sinks, spec.propagators)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run_simultaneous_taint ~match_hook:_ (rules : R.taint_mode R.rule_info list)
    _xconf _target =
  (* First, group the rules by language. We can't run multiple rules of different
     languages simultaneously anyways.
  *)
  let rules_by_lang =
    let rules_by_lang_hashtbl =
      rules |> Common.map (fun r -> (r.R.languages, r)) |> Common.hash_of_list
    in
    Hashtbl.to_seq_keys rules_by_lang_hashtbl
    |> List.of_seq
    |> Common.map (fun lang ->
           (lang, Hashtbl.find_all rules_by_lang_hashtbl lang))
  in
  (* For all the groupings of rules, create a "combined rule" which essentially just
     attaches taint labels to each source and sink, separately.

     We should produce one
  *)
  rules_by_lang
  |> List.filter_map (fun (lang, rules) ->
         match rules with
         | [] -> None (* shouldn't be empty, but let's just check *)
         | __else__ ->
             let rule_string =
               Common.map (fun r -> fst r.R.id) rules |> String.concat "-"
             in
             (* We combine all the sources, sinks, and propagators.
       *)
             let sources, sanitizers, sinks, propagators =
               rules |> Common.map mk_unique_spec |> Common2.unzip4
               |> fun (sources, sanitizers, sinks, propagators) ->
               ( List.concat sources,
                 List.concat sanitizers,
                 List.concat sinks,
                 List.concat propagators )
             in
             let combined_spec =
               {
                 R.sources = (PI.unsafe_fake_info "source", sources);
                 sanitizers;
                 sinks = (PI.unsafe_fake_info "sinks", sinks);
                 propagators;
               }
             in
             let combined_rule =
               {
                 R.id = (rule_string, PI.unsafe_fake_info "fakerule");
                 mode = `Taint combined_spec;
                 message = "This is a combined rule!";
                 severity = R.Error;
                 (* TODO *)
                 languages = lang;
                 options = None;
                 (* TODO *)
                 equivalences = None;
                 (* TODO *)
                 fix = None;
                 (* TODO *)
                 fix_regexp = None;
                 (* TODO *)
                 paths = None;
                 (* TODO *)
                 metadata = None (* TODO *);
               }
             in
             Some combined_rule)
