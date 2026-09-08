(* Cooper Pierce
 *
 * Copyright (C) Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License version 2.1 as
 * published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the file LICENSE for more details.
 *)

(* Extracting required literal substrings from a regexp, for prefiltering.

   Given a PCRE-compatible regexp such as

     (?<KEY>\b((AKIA|ABIA|ACCA)[0-9A-Z]{16})\b)

   any string it matches must contain one of the literals "AKIA", "ABIA", or
   "ACCA", so a file containing none of them can be skipped without running
   the (comparatively expensive) regexp. This module computes such a necessary
   condition as a [Predicate.t Formula.t] of literal [String] predicates.

   We parse the regexp into an AST (see [Parser_regexp]) and fold it bottom-up
   into a [frag], a summary of the guaranteed-literal structure of a
   subexpression:

   - [exact]: the fixed string, if the fragment matches exactly one;
   - [prefix]/[suffix]: the literal runs guaranteed at the start/end of any
     match, kept open so that adjacent literals merge across a concatenation
     into a single longer (more selective) substring;
   - [req]: a formula of substrings guaranteed to occur somewhere in any
     match (runs closed off in the interior, alternations).

   Concatenation merges boundary runs and closes off interior ones;
   alternation disjoins its branches' requirements; a repetition with a
   positive lower bound keeps its content's requirements. Constructs we cannot
   reason about soundly (arbitrary character classes, back-references, [.],
   ...) contribute no constraint. An inline [(?i)] makes literals in its scope
   case-insensitive; runs of differing case-sensitivity are never merged.

   Extraction functions return [None] ("give up") for constructs that could
   make the result unsound, notably the [(?x)] option, which changes how the
   pattern lexes. The extracted formula is always a *necessary* condition for
   the regexp to match, so it is sound to use as a prefilter; it may of course
   be weaker than the regexp itself.

   Note that the formula is evaluated against whole file contents, not against
   match text; this is what makes it sound to treat zero-width assertions
   (including [\K], which excludes the preceding text from the reported match)
   as transparent to literal runs. *)

open Common
module P = Predicate
module F = Formula
module R = Parser_regexp

(* Minimum length for an extracted literal to be worth using. Shorter literals
   match too often to filter effectively; when a required run falls below this
   we treat it as no constraint, which (in an alternation) makes us give up on
   that regexp and fall back to running it in full. *)
let min_substring_length = 3

(* Cap on how many copies of a fixed string we materialize for a bounded
   repeat like [(ab){1000}]. Fewer copies only ever weaken the (still sound)
   prefilter. *)
let max_repeat_expansion = 8

(* A literal run: a guaranteed-present substring, tagged with whether it is
   matched case-insensitively (i.e., it occurred within a [(?i)] scope). *)
type run = { str : string; ci : bool }

let no_run = { str = ""; ci = false }

let equal_run (r1 : run) (r2 : run) : bool =
  String.equal r1.str r2.str && Bool.equal r1.ci r2.ci

(* A summary of the guaranteed-literal structure of a regexp fragment.

   Invariants:
   - if [exact = Some r], the fragment matches exactly the fixed run [r], and
     [prefix = suffix = r] and [req = None];
   - [prefix]/[suffix] are literal runs guaranteed at the very start/end of
     any match ([no_run] if none);
   - [req] is a formula of substrings guaranteed to occur somewhere in any
     match; [None] means "no constraint" (trivially true). *)
type frag = {
  exact : run option;
  prefix : run;
  suffix : run;
  req : P.t F.t option;
}

(* A fragment about which we know nothing: matches anything, imposes no
   constraint, and breaks any surrounding literal run. *)
let top = { exact = None; prefix = no_run; suffix = no_run; req = None }

(* A fragment matching exactly the empty string. Transparent to literal runs:
   used for zero-width assertions like [\b] or [^]. *)
let empty_str =
  { exact = Some no_run; prefix = no_run; suffix = no_run; req = None }

let run_pred (r : run) : P.t F.t option =
  if String.length r.str >= min_substring_length then
    Some (F.pred (P.String { needle = r.str; case_sensitive = not r.ci }))
  else None

let and_opt (xs : P.t F.t option list) : P.t F.t option =
  List_.filter_some xs |> F.and_

(* Disjoin the branch formulas of an alternation, flattening nested [Or]s that
   arise from the AST's binary [Alt] nodes. *)
let flatten_or (fs : P.t F.t list) : P.t F.t option =
  List.concat_map
    (function
      | F.Or xs -> xs
      | f -> [ f ])
    fs
  |> F.or_

(* Close a fragment into a single required-substring formula: every guaranteed
   run AND-ed with the interior requirements. *)
let close (fr : frag) : P.t F.t option =
  let runs =
    match fr.exact with
    | Some r -> [ r ]
    | None when equal_run fr.prefix fr.suffix -> [ fr.prefix ]
    | None -> [ fr.prefix; fr.suffix ]
  in
  and_opt (fr.req :: List.map run_pred runs)

(* Concatenate two adjacent runs; [None] if they cannot form a single run
   because their case-sensitivity differs. *)
let cat_runs (r1 : run) (r2 : run) : run option =
  if String_.empty r1.str then Some r2
  else if String_.empty r2.str then Some r1
  else if Bool.equal r1.ci r2.ci then Some { str = r1.str ^ r2.str; ci = r1.ci }
  else None

let repeat_run (r : run) n =
  let n = Int.min n max_repeat_expansion in
  { r with str = String.concat "" (List.init n (fun _ -> r.str)) }

(*****************************************************************************)
(* Folding the AST into a frag *)
(*****************************************************************************)

(* Concatenation: merge the boundary runs (a's suffix meeting b's prefix) and
   close off any run that ends up interior. An exact side extends the merged
   run all the way to its outer end, so the run stays a prefix/suffix. *)
let seq (a : frag) (b : frag) : frag =
  let reqs = [ a.req; b.req ] in
  match (cat_runs a.suffix b.prefix, a.exact, b.exact) with
  | Some j, Some _, Some _ ->
      { exact = Some j; prefix = j; suffix = j; req = and_opt reqs }
  | Some j, Some _, None ->
      { exact = None; prefix = j; suffix = b.suffix; req = and_opt reqs }
  | Some j, None, Some _ ->
      { exact = None; prefix = a.prefix; suffix = j; req = and_opt reqs }
  | Some j, None, None ->
      (* the merged run is interior: at neither end of a match *)
      {
        exact = None;
        prefix = a.prefix;
        suffix = b.suffix;
        req = and_opt (run_pred j :: reqs);
      }
  | None, _, _ ->
      (* Boundary runs of differing case-sensitivity: keep them separate. A
         non-exact side's boundary run becomes interior and is closed off; an
         exact side's run is still that side's outer prefix/suffix. *)
      let interior exact r =
        if Option.is_some exact then None else run_pred r
      in
      {
        exact = None;
        prefix = a.prefix;
        suffix = b.suffix;
        req =
          and_opt
            (interior a.exact a.suffix :: interior b.exact b.prefix :: reqs);
      }

let repeat (fr : frag) ((mn, mx) : R.AST.repeat_range) : frag =
  if mn =|= 0 then (* the content may not appear at all *)
    top
  else
    match fr.exact with
    | None ->
        (* the content appears at least once, so its runs and requirements
           carry over unchanged *)
        fr
    | Some r ->
        let r' = repeat_run r mn in
        (* [r'] is capped at [max_repeat_expansion] copies, which makes it a
           prefix and a suffix of any actual repetition, but the exact match
           only for a fixed count we materialized in full. *)
        let exact =
          match mx with
          | Some m when m =|= mn && mn <= max_repeat_expansion -> Some r'
          | _ -> None
        in
        { exact; prefix = r'; suffix = r'; req = None }

let is_ascii_alnum (c : char) : bool =
  match c with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9' ->
      true
  | _ -> false

(* The regexp lexer does not decode every PCRE escape faithfully: e.g. it
   turns [\N], [\R], [\X], [\C], and [\E] (all non-literal in PCRE) into the
   literal letter, and decodes [\f] and [\cX] to the wrong byte. Rather than
   depend on the lexer being complete, only trust a [Singleton] whose source
   text is the character itself, or a backslash escape of that same character
   where it cannot be an escape-sequence letter (e.g. [\.] or [\+]). *)
let trusted_literal ((tok, _) : R.AST.loc) (c : char) : bool =
  match Tok.content_of_tok_opt tok with
  | None -> false
  | Some src ->
      String.equal src (String.make 1 c)
      || String.length src =|= 2
         && Char.equal src.[0] '\\'
         && Char.equal src.[1] c
         && not (is_ascii_alnum c)

(* [None] (here and below) means "give up on the whole regexp"; see
   [frag_of_node]. *)
let frag_of_char_class ~caseless (loc : R.AST.loc) (cc : R.AST.char_class) :
    frag option =
  match cc with
  | Singleton code when code =|= Char.code '[' || code =|= Char.code ']' ->
      (* A literal '[' or ']' is a strong sign that [Parser_regexp] failed to
         parse a bracket expression and emitted its characters as separate
         literals (e.g. a class containing whitespace, such as [ =:]). Those
         "literals" do not reflect the real PCRE semantics, so trusting them
         would be unsound. *)
      None
  | Singleton code
    when code >= 0x1 && code <= 0x7f && trusted_literal loc (Char.chr code) ->
      (* A single concrete ASCII byte is a literal. We stay within [0x1, 0x7f]
         so the byte stands alone in UTF-8 (and never the NUL terminator). *)
      let r = { str = String.make 1 (Char.chr code); ci = caseless } in
      Some { exact = Some r; prefix = r; suffix = r; req = None }
  | _ -> Some top

let frag_of_special (sp : R.AST.special) : frag option =
  match sp with
  | Beginning_of_line
  | End_of_line
  | Beginning_of_input
  | End_of_last_line
  | End_of_input
  | Beginning_of_match
  | Word_boundary
  | Not_word_boundary
  | Match_point_reset ->
      (* zero-width assertions: transparent to literal runs *)
      Some empty_str
  | Set_option Ignore_whitespace
  | Clear_option Ignore_whitespace ->
      (* [(?x)] changes how the pattern lexes; we cannot reason about it *)
      None
  | Set_option _
  | Clear_option _ ->
      (* [Caseless] is handled in the [Seq] fold, where it can affect the
         following siblings; other options ([m], [s], ...) do not change
         literals. *)
      Some empty_str
  | Numeric_back_reference _
  | Named_back_reference _
  | Callout _
  | Recurse_pattern _
  | Call_subpattern_by_abs_number _
  | Call_subpattern_by_rel_number _
  | Call_subpattern_by_name _ ->
      (* matches non-literal text; contributes no constraint *)
      Some top

let rec flatten_alt (node : R.AST.t) : R.AST.t list =
  match node with
  | Alt (_, a, b) -> flatten_alt a @ flatten_alt b
  | other -> [ other ]

let rec flatten_seq (node : R.AST.t) : R.AST.t list =
  match node with
  | Seq (_, a, b) -> flatten_seq a @ flatten_seq b
  | other -> [ other ]

(* [None] means the regexp contains a construct that would make extraction
   unsound; the caller must give up and use the full regexp. *)
let rec frag_of_node ~caseless (node : R.AST.t) : frag option =
  match node with
  | Empty _ -> Some empty_str
  | Char (loc, cc) -> frag_of_char_class ~caseless loc cc
  | Special (_, sp) -> frag_of_special sp
  | Seq _ ->
      (* Fold left-to-right, tracking the case-insensitivity scope: an inline
         [(?i)]/[(?-i)] affects the following siblings and ends with the
         enclosing group (which is a separate [frag_of_node] call). *)
      let* _caseless, fr =
        List.fold_left
          (fun acc item ->
            let* cl, fr = acc in
            match item with
            | R.AST.Special (_, Set_option Caseless) -> Some (true, fr)
            | R.AST.Special (_, Clear_option Caseless) -> Some (false, fr)
            | _ ->
                let* fr' = frag_of_node ~caseless:cl item in
                Some (cl, seq fr fr'))
          (Some (caseless, empty_str))
          (flatten_seq node)
      in
      Some fr
  | Alt _ -> (
      (* Folding each branch with the same [caseless] is sound because the
         parser hoists an inline [(?i)] above the rest of its alternation: PCRE
         scopes it over the *following branches too*, and correspondingly
         [Parser_regexp] parses [(?i)a|b] as [Seq ((?i), Alt (a, b))] (and
         rejects a mid-sequence [(?i)] as a syntax error). *)
      let* branches =
        flatten_alt node |> List.map (frag_of_node ~caseless) |> Base.Option.all
      in
      (* Each branch must be satisfied on its own, so [close] each and disjoin
         them. All-or-nothing: if any branch imposes no constraint, neither
         does the alternation (see NOTE "AND vs OR and filter_map" in
         Analyze_rule.ml). *)
      match List.map close branches |> Base.Option.all with
      | None -> Some top
      | Some fs -> Some { top with req = flatten_or fs })
  | Repeat (_, x, range, _pref) ->
      let* fr = frag_of_node ~caseless x in
      Some (repeat fr range)
  | Group (_, kind, x) -> (
      match kind with
      | Non_capturing
      | Non_capturing_reset
      | Capturing
      | Named_capture _
      | Atomic ->
          frag_of_node ~caseless x
      | Lookahead
      | Neg_lookahead
      | Lookbehind
      | Neg_lookbehind ->
          (* zero-width; a positive lookaround does imply its content, but we
             stay simple (and sound) by treating all as transparent *)
          Some empty_str
      | Other _ -> Some top)
  | Conditional _ -> Some top

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let required_substrings (re : string) : P.t F.t option =
  (* The parser rejects regexps it does not support with various exceptions
     ([Parsing_error.Syntax_error], lexer [Failure]s, ...). This is a
     best-effort optimization, so any of them just means "no prefilter". *)
  match R.Parse.string re with
  | exception exn ->
      Log.debug (fun m ->
          m "no prefilter for unparseable regexp /%s/: %s" re
            (Printexc.to_string exn));
      None
  | ast ->
      let* fr = frag_of_node ~caseless:false ast in
      close fr
