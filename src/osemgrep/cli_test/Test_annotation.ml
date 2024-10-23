(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep Inc.
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
open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Module to represent and parse semgrep test annotations.
 * See https://semgrep.dev/docs/writing-rules/testing-rules/ for more info.
 *
 * Note that our docs currently mention just the OSS test annotations but not
 * the pro one (e.g., 'proruleid:', 'deepruleid:').
 * See tests/intrafile/README for more info on the pro/deep annotations.
 *
 * Note that if a finding is expected in OSS but not in Pro, you may need to
 * combine two annotations, e.g. `ruleid: prook: test` would mean that the
 * finding is expected in OSS but in Pro we expect no finding, which could be
 * due to inter-procedural analysis being able to spot sanitization via a
 * third-function.
 *
 * TODO: update https://semgrep.dev/docs/writing-rules/testing-rules and add
 * doc for the pro/deep test annotations and remove tests/intrafile/README.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type kind =
  (* The good one, should be reported (TP) *)
  | Ruleid
  (* Should be reported but are not because of current engine limitations (FN) *)
  | Todoruleid
  (* Are reported but should not (FP) *)
  | Todook
  (* Those should *not* be reported (TN)
   * The 'ok:' is not that useful (it's mostly a comment) and actually
   * complicates some code during parsing (see the _no_ok regexps below), but
   * the `prook:` and `deepok:` are useful to "negate" a preceding ruleid: when
   * a legitimate finding in semgrep OSS is actually considered a FP for the
   * pro engine and should not be reported.
   *)
  | Ok
[@@deriving show]

(* Here we follow the conventions used in the annotations themselves with
 * proruleid: and deepruleid: (see tests/intrafile/README).
 * alt: CoreScan | ProScan | DeepScan
 * less: factorize with the other engine types
 *)
type engine = OSS | Pro | Deep [@@deriving show]

(* ex: "#ruleid: lang.ocaml.do-not-use-lisp-map"
 * but also "ruleid: prook: lang.ocaml.do-not-use-lisp-map".
 *
 * Note that 'ruleid:' implies 'proruleid:' and 'deepruleid:' so you don't need
 * to repeat those annotations. You usually need multiple kind/engine
 * prefix when one engine TP would be another engine TN (e.g., 'ruleid: prook:')
 * See Test_subcommand.filter_annots_for_engine comment for more about those
 * implications.
 *)
type t = {
  kind : kind;
  engine : engine;
  (* e.g., a ruleid: prook: x
   * alt: call the field negators?
   *)
  others : (kind * engine) list;
  (* alt: ids: Rule_ID.t list; (instead we return a list of annots) *)
  id : Rule_ID.t;
}
[@@deriving show]

(* starts at 1 *)
type linenb = int
type annotations = (t * linenb) list

let prefilter_annotation_regexp = ".*\\(ruleid\\|ok\\|todoruleid\\|todook\\):.*"

(* removing ok as it could be valid code (as in `ok: foo` in JS)
 * alt: choose an annotation for ok: that would be less ambiguous
 * alt: get rid of ok:, just care about prook: and deepok:
 *)
let prefilter_annotation_regexp_no_ok =
  ".*\\(ruleid\\|todoruleid\\|todook\\):.*"

let annotation_regexp = "^\\(ruleid\\|ok\\|todoruleid\\|todook\\):\\(.*\\)"

let (comment_syntaxes : (string * string option) list) =
  [
    ("#", None);
    ("//", None);
    ("<!--", Some "-->");
    ("(*", Some "*)");
    ("/*", Some "*/");
    (* in jsx as in
     *       <div>
     *         {/* ruleid: href-semgrep-app */}
     * where you can't use comment
     *)
    ("{/*", Some "*/}");
  ]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let kind_of_string (str : string) : kind =
  match str with
  | "ruleid" -> Ruleid
  | "ok" -> Ok
  | "todoruleid" -> Todoruleid
  | "todook" -> Todook
  | s -> failwith (spf "not a valid annotation: %s" s)

let remove_enclosing_comment_opt (str : string) : string option =
  comment_syntaxes
  |> List.find_map (fun (prefix, suffixopt) ->
         (* stricter: pysemgrep allows code before the comment, but this
          * was used only once in semgrep-rules/ and it can be ambiguous as
          * <some code> # ruleid: xxx might make you think the finding is
          * on this line instead of the line after. Forcing the annotation
          * to be alone on its line before the finding is clearer.
          *)
         if String.starts_with ~prefix str then
           let str = Str.string_after str (String.length prefix) in
           match suffixopt with
           | None -> Some str
           | Some suffix ->
               if String.ends_with ~suffix str then
                 let before = String.length str - String.length suffix in
                 Some (Str.string_before str before)
               else (
                 Logs.warn (fun m ->
                     m "could not find end comment %s in %s" suffix str);
                 Some str)
         else None)

let () =
  Testo.test "Test_subcommand.remove_enclosing_comment_opt" (fun () ->
      let test_remove (str : string) (expected : string option) =
        let res = remove_enclosing_comment_opt str in
        if not (res =*= expected) then
          failwith
            (spf "didn't match, got %s, expected %s" (Dumper.dump res)
               (Dumper.dump expected))
      in
      test_remove "# foobar" (Some " foobar");
      test_remove "// foobar" (Some " foobar");
      test_remove "<!-- foobar -->" (Some " foobar ");
      ())

(* returns the rest of the string too *)
let parse_kind_and_engine_opt (s : string) : (kind * engine * string) option =
  let engine, s =
    match s with
    | _ when s =~ "^pro\\(.*\\)" -> (Pro, Common.matched1 s)
    | _ when s =~ "^deep\\(.*\\)" -> (Deep, Common.matched1 s)
    | _ -> (OSS, s)
  in
  if s =~ annotation_regexp then
    let kind_str, s = Common.matched2 s in
    let kind = kind_of_string kind_str in
    let s = String.trim s in
    Some (kind, engine, s)
  else None

(* TODO: could check for bad combinations such as
 * - ruleid: deepruleid: proruleid:  (redundant)
 * - todoruleid: deepruleid: (probably just need deepruleid:)
 *)
let parse_kinds_and_engines_opt (s : string) :
    (kind * engine * (kind * engine) list * string) option =
  let* kind, engine, s = parse_kind_and_engine_opt s in
  let rec aux annots s =
    match parse_kind_and_engine_opt s with
    | None -> Some (kind, engine, List.rev annots, s)
    | Some (kind2, engine2, s) -> aux ((kind2, engine2) :: annots) s
  in
  aux [] s

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(* This does a few things:
 *  - check comments: #, //, ( *, <--
 *  - support pro/deep annotations
 *  - support possible deepok: or prook: following the ruleid: (to negate
 *    the ruleid when running a ProScan or DeepScan)
 *  - support multiple ruleids separated by commas
 * alt: use parser combinators instead of those regexps/trims/Str.string_xxx
 *)
let annotations_of_string (orig_str : string) (file : Fpath.t) (idx : linenb) :
    annotations =
  let s = orig_str in
  let error_context = spf "in %s line %d" !!file idx in
  if s =~ prefilter_annotation_regexp then
    (* " <!-- ruleid: foo.bar --> " *)
    let s = String.trim s in
    (* "<!-- ruleid: foo.bar -->" *)
    let res = remove_enclosing_comment_opt s in
    match res with
    | None ->
        (* some Javascript code has valid code such as { ok: true } that is not
         * a semgrep annotation hence the use of a no_ok prefilter below
         *)
        if s =~ prefilter_annotation_regexp_no_ok then
          Logs.err (fun m ->
              m "annotation without leading comment: %s" orig_str)
        else
          Logs.debug (fun m ->
              m "skipping %s, actually not an annotation" orig_str);
        []
    | Some s -> (
        (* " ruleid: foo.bar " *)
        let s = String.trim s in
        (* "ruleid: foo.bar" *)
        match parse_kinds_and_engines_opt s with
        | Some (kind, engine, others, s) ->
            let xs =
              Str.split_delim (Str.regexp "[ \t]*,[ \t]*") s
              |> List_.map String.trim
            in
            xs
            |> List_.filter_map (fun id_str ->
                   match Rule_ID.of_string_opt id_str with
                   | Some id -> Some ({ kind; engine; others; id }, idx)
                   | None ->
                       Logs.warn (fun m ->
                           m
                             "malformed rule ID '%s' (%s) skipping this \
                              annotation"
                             id_str error_context);
                       None)
        | None ->
            Logs.warn (fun m ->
                m "could not parse annotation: %s (%s)" orig_str error_context);
            [])
  else []

(* Note that this returns the line of the annotation itself. In practice,
 * you must then add +1 to it if you want to compare it to where semgrep
 * report matches.
 *
 * alt: use Core_error.expected_error_lines_of_files but it does not
 * allow to extract the ruleID after the annotation_kind
 *)
let annotations (file : Fpath.t) : annotations =
  UFile.cat file |> List_.index_list_1
  |> List.concat_map (fun (s, idx) -> annotations_of_string s file idx)

let () =
  Testo.test "Test_subcommand.annotations" (fun () ->
      let test (str : string) (expected : t list) =
        let xs =
          annotations_of_string str (Fpath.v "foo") 0
          |> List_.map (fun (annot, _idx) -> annot)
        in
        if not (xs =*= expected) then
          failwith
            (spf "Annotations didn't match, got %s, expected %s"
               (Dumper.dump xs) (Dumper.dump expected))
      in
      let rule_id s = Rule_ID.of_string_exn s in
      test "// ruleid: foo"
        [ { kind = Ruleid; engine = OSS; others = []; id = rule_id "foo" } ];
      test "// ruleid: foo, bar"
        [
          { kind = Ruleid; engine = OSS; others = []; id = rule_id "foo" };
          { kind = Ruleid; engine = OSS; others = []; id = rule_id "bar" };
        ];
      test "<!-- ruleid: foo-bar -->"
        [ { kind = Ruleid; engine = OSS; others = []; id = rule_id "foo-bar" } ];
      (* the ok: does not mean it's an annot; it's regular (JS) code *)
      test "return res.send({ok: true})" [];
      test "// ruleid: deepok: foo"
        [
          {
            kind = Ruleid;
            engine = OSS;
            others = [ (Ok, Deep) ];
            id = rule_id "foo";
          };
        ];
      ())

(*****************************************************************************)
(* Annotations grouping and filtering *)
(*****************************************************************************)

(* group them by rule id, and adjust the linenb + 1 so it can be used to
 * compare actual matches.
 *)
let group_by_rule_id (annots : annotations) : (Rule_ID.t, linenb list) Assoc.t =
  annots
  |> Assoc.group_by (fun ({ id; _ }, _) -> id)
  |> List_.map (fun (id, xs) ->
         ( id,
           xs
           |> List_.map (fun (_, line) -> line + 1)
           (* should not be needed given how annotations work but safer *)
           |> List.sort_uniq Int.compare ))

let filter_todook (annots : annotations) (xs : linenb list) : linenb list =
  let (todooks : linenb Set_.t) =
    annots
    |> List_.filter_map (fun ({ kind; _ }, line) ->
           match kind with
           (* + 1 because the expected/reported is the line after the annotation *)
           | Todook -> Some (line + 1)
           | Ruleid
           | Ok
           | Todoruleid ->
               None)
    |> Set_.of_list
  in
  xs |> List_.exclude (fun line -> Set_.mem line todooks)
