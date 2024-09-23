open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Module to represent and parse test annotations.
 * See https://semgrep.dev/docs/writing-rules/testing-rules/
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type kind =
  (* The good one, should be reported (TP) *)
  | Ruleid
  (* Those should *not* be reported (TN) *)
  | Ok
  (* Should be reported but are not because of current engine limitations (FN) *)
  | Todoruleid
  (* Are reported but should not (FP) *)
  | Todook
[@@deriving show]

(* following the conventions used in the annotations themselves with
 * proruleid: and deepruleid: (see tests/intrafile/README).
 * alt: CoreScan | ProScan | DeepScan
 * less: factorize with the other engine types
 *)
type engine = OSS | Pro | Deep [@@deriving show]

(* ex: "#ruleid: lang.ocaml.do-not-use-lisp-map" *)
type t = kind * engine * Rule_ID.t [@@deriving show]

(* starts at 1 *)
type linenb = int
type annotations = (t * linenb) list

let prefilter_annotation_regexp = ".*\\(ruleid\\|ok\\|todoruleid\\|todook\\):.*"

(* removing ok as it could be valid code (as in `ok: foo` in JS)
 * alt: choose an annotation for ok: that would be less ambiguous
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

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

let annotation_kind_of_string (str : string) : kind =
  match str with
  | "ruleid" -> Ruleid
  | "ok" -> Ok
  | "todoruleid" -> Todoruleid
  | "todook" -> Todook
  | s -> failwith (spf "not a valid annotation: %s" s)

(* This does a few things:
 *  - check comments: #, //, ( *, <--
 *  - support multiple ruleids separated by commas
 *  - support possible leading deepok:
 *  - TODO? support pro/deep annotations?
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
        if s =~ prefilter_annotation_regexp_no_ok then
          Logs.err (fun m ->
              m "annotation without leading comment: %s" orig_str)
          (* some Javascript code has valid code such as { ok: true } that is not
           * a semgrep annotation *)
        else
          Logs.debug (fun m ->
              m "skipping %s, actually not an annotation" orig_str);
        []
    | Some s ->
        (* " ruleid: foo.bar " *)
        let s = String.trim s in
        (* "ruleid: foo.bar" *)
        let engine, s =
          match s with
          | _ when s =~ "^pro\\(.*\\)" -> (Pro, Common.matched1 s)
          | _ when s =~ "^deep\\(.*\\)" -> (Deep, Common.matched1 s)
          | _ -> (OSS, s)
        in
        if s =~ annotation_regexp then
          let kind_str, ids_str = Common.matched2 s in
          let kind = annotation_kind_of_string kind_str in
          let s = String.trim ids_str in
          let s =
            (* indicate that no finding is expected in interfile analysis *)
            let prefix = "deepok:" in
            if String.starts_with ~prefix s then
              Str.string_after s (String.length prefix)
            else ids_str
          in
          let xs =
            Str.split_delim (Str.regexp "[ \t]*,[ \t]*") s
            |> List_.map String.trim
          in
          xs
          |> List_.filter_map (fun id_str ->
                 match Rule_ID.of_string_opt id_str with
                 | Some id -> Some ((kind, engine, id), idx)
                 | None ->
                     Logs.warn (fun m ->
                         m
                           "malformed rule ID '%s' (%s) skipping this \
                            annotation"
                           id_str error_context);
                     None)
        else (
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
      test "// ruleid: foo.bar"
        [ (Ruleid, OSS, Rule_ID.of_string_exn "foo.bar") ];
      test "// ruleid: foo, bar"
        [
          (Ruleid, OSS, Rule_ID.of_string_exn "foo");
          (Ruleid, OSS, Rule_ID.of_string_exn "bar");
        ];
      test "<!-- ruleid: foo-bar -->"
        [ (Ruleid, OSS, Rule_ID.of_string_exn "foo-bar") ];
      (* the ok: does not mean it's an annot; it's regular (JS) code *)
      test "return res.send({ok: true})" [];
      test "// ruleid: deepok: foo.deep"
        [ (Ruleid, OSS, Rule_ID.of_string_exn "foo.deep") ];
      ())

(*****************************************************************************)
(* Annotations grouping and filtering *)
(*****************************************************************************)

(* Keep only the Ruleid and Todook, group them by rule id, and adjust
 * the linenb + 1 so it can be used to compare actual matches.
 *)
let group_positive_annotations (annots : annotations) :
    (Rule_ID.t, linenb list) Assoc.t =
  annots
  |> List_.filter_map (fun ((kind, engine, id), line) ->
         match kind with
         | Ruleid
         | Todook ->
             Some (id, engine, line)
         | Ok
         | Todoruleid ->
             None)
  |> Assoc.group_by (fun (id, _engine, _line) -> id)
  |> List_.map (fun (id, xs) ->
         ( id,
           xs
           |> List_.map (fun (_id, _engine, line) -> line + 1)
           (* should not be needed given how annotations work but safer *)
           |> List.sort_uniq Int.compare ))

let filter_todook (annots : annotations) (xs : linenb list) : linenb list =
  let (todooks : linenb Set_.t) =
    annots
    |> List_.filter_map (fun ((kind, _engine, _id), line) ->
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
