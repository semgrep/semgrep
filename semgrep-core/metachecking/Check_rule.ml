(*s: semgrep/metachecking/Check_rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
(*e: pad/r2c copyright *)
open Common

open Rule
module R = Rule
module E = Error_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Checking the checker (metachecking).
 *
 * The goal of this module is to detect bugs, performance issues, or
 * feature suggestions in semgrep rules.
 *
 * TODO:
 *  - classic boolean checks for satisfaisability? A & !A => not good
 *  - use spacegrep or semgrep itself? but need sometimes to express
 *    rules on the yaml structure and sometimes on the pattern itself
 *    (a bit like in templating languages)
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type env = Rule.t

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error (env: env) s =
  let loc = Parse_info.first_loc_of_file (env.file) in
  let s = spf "%s (in ruleid: %s)" s env.id in
  let check_id = "semgrep-metacheck-rule" in
  let err = E.mk_error_loc loc (E.SemgrepMatchFound (check_id, s)) in
  pr2 (E.string_of_error err)

let show_formula_old pf =
  match pf with
  | Pat x | PatNot x | PatInside x | PatNotInside x ->
      x.pstr
  | _ -> R.show_formula_old pf

(*****************************************************************************)
(* Subparts checker *)
(*****************************************************************************)

let check_old_formula env lang f =
  (* check duplicated patterns, essentially:
   *  $K: $PAT
   *  ...
   *  $K2: $PAT
   * but at the same level!
  *)
  let rec find_dupe f =
    match f with
    | Pat _ | PatNot _ | PatInside _ | PatNotInside _ -> ()
    | PatExtra _ -> ()
    | PatEither xs | Patterns xs ->
        let rec aux xs =
          match xs with
          | [] -> ()
          | x::xs ->
              (* todo: for Pat, we could also check if exist PatNot
               * in which case intersection will always be empty
              *)
              if xs |> List.exists (R.equal_formula_old x)
              then error env (spf "Duplicate pattern %s" (show_formula_old x));
              aux xs
        in
        (* breadth *)
        aux xs;
        (* depth *)
        xs |> List.iter find_dupe
  in
  find_dupe f;

  (* call Check_pattern subchecker *)
  f |> visit_old_formula (fun { pat; pstr = _pat_str; pid = _ } ->
    match pat, lang with
    | Sem semgrep_pat, L (lang, _rest)  ->
        Check_pattern.check lang semgrep_pat
    | Spacegrep _spacegrep_pat, LGeneric -> ()
    | Regexp _, _ -> ()
    | _ -> raise Impossible
  );
  ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let check r =
  (match r.formula with
   | Old f -> check_old_formula r r.languages f;
   | New _ -> ()
  );
  ()
(*e: semgrep/metachecking/Check_rule.ml *)
