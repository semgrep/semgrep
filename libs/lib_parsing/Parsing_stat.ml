(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 * Copyright (C) 2020 r2c
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
open Common

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Some helpers for the different lexers and parsers in pfff *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type ast_stat = { total_node_count : int; untranslated_node_count : int }

type t = {
  filename : Common.filename;
  total_line_count : int;
  mutable error_line_count : int;
  mutable have_timeout : bool;
  (* used by our cpp commentizer *)
  mutable commentized : int;
  (* if want to know exactly what was passed through, uncomment:
   *
   * mutable passing_through_lines: int;
   *
   * it differs from bad by starting from the error to
   * the synchro point instead of starting from start of
   * function to end of function.
   *)
  (* for instance to report most problematic macros when parse c/c++ *)
  mutable problematic_lines :
    (string list (* ident in error line *) * int (* line_error *)) list;
  ast_stat : ast_stat option;
}

(* deprecated *)
type parsing_stat = t

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let summary_of_stat (x : parsing_stat) =
  spf "%s lines=%i error_lines=%i timeout=%B" x.filename x.total_line_count
    x.error_line_count x.have_timeout

let default_stat file =
  let n = Common2.nblines_eff file in
  {
    filename = file;
    total_line_count = n;
    error_line_count = 0;
    have_timeout = false;
    commentized = 0;
    problematic_lines = [];
    ast_stat = None;
  }

let bad_stat file =
  let stat = default_stat file in
  stat.error_line_count <- stat.total_line_count;
  stat

let correct_stat file = default_stat file

(*****************************************************************************)
(* Parsing statistics *)
(*****************************************************************************)

let aggregate_stats statxs =
  let total_lines =
    statxs |> List.fold_left (fun acc { total_line_count = x; _ } -> acc + x) 0
  in
  let bad =
    statxs |> List.fold_left (fun acc { error_line_count = x; _ } -> acc + x) 0
  in
  (total_lines, bad)

(* todo: stat per dir ?  give in terms of func_or_decl numbers:
 * nbfunc_or_decl pbs / nbfunc_or_decl total ?/
 *
 * note: cela dit si y'a des fichiers avec des #ifdef dont on connait pas les
 * valeurs alors on parsera correctement tout le fichier et pourtant y'aura
 * aucune def  et donc aucune couverture en fait.
 * ==> TODO evaluer les parties non parsé ?
 *)

let print_parsing_stat_list ?(verbose = false) statxs =
  let total = List.length statxs in
  let perfect =
    statxs
    |> List.filter (function
         | { have_timeout = false; error_line_count = 0; _ } -> true
         | _ -> false)
    |> List.length
  in

  if verbose then (
    pr "\n\n\n---------------------------------------------------------------";
    pr "pbs with files:";
    statxs
    |> List.filter (function
         | { have_timeout = true; _ } -> true
         | { error_line_count = n; _ } when n > 0 -> true
         | _ -> false)
    |> List.iter (function
           | {
               filename = file;
               have_timeout = timeout;
               error_line_count = n;
               _;
             }
           -> pr (file ^ "  " ^ if timeout then "TIMEOUT" else i_to_s n));

    pr "\n\n\n";
    pr "files with lots of tokens passed/commentized:";
    let threshold_passed = 100 in
    statxs
    |> List.filter (function
         | { commentized = n; _ } when n > threshold_passed -> true
         | _ -> false)
    |> List.iter (function { filename = file; commentized = n; _ } ->
           pr (file ^ "  " ^ i_to_s n));

    pr "\n\n\n");

  let total_lines =
    statxs |> List.fold_left (fun acc { total_line_count = x; _ } -> acc + x) 0
  in
  let bad =
    statxs |> List.fold_left (fun acc { error_line_count = x; _ } -> acc + x) 0
  in
  let passed =
    statxs |> List.fold_left (fun acc { commentized = x; _ } -> acc + x) 0
  in
  let good = total_lines - bad in

  pr "---------------------------------------------------------------";
  pr
    (spf "NB total files = %d; " total
    ^ spf "NB total lines = %d; " total_lines
    ^ spf "perfect = %d; " perfect
    ^ spf "pbs = %d; "
        (statxs
        |> List.filter (function
             | { error_line_count = n; _ } when n > 0 -> true
             | _ -> false)
        |> List.length)
    ^ spf "timeout = %d; "
        (statxs
        |> List.filter (function
             | { have_timeout = true; _ } -> true
             | _ -> false)
        |> List.length)
    ^ spf "=========> %d" (100 * perfect / total)
    ^ "%");
  let gf, badf = (float_of_int good, float_of_int bad) in
  let passedf = float_of_int passed in
  pr
    (spf "nb good = %d,  nb passed = %d " good passed
    ^ spf "=========> %f" (100.0 *. (passedf /. gf))
    ^ "%");
  pr
    (spf "nb good = %d,  nb bad = %d " good bad
    ^ spf "=========> %f" (100.0 *. (gf /. (gf +. badf)))
    ^ "%")

(*****************************************************************************)
(* Regression stats *)
(*****************************************************************************)

let print_regression_information ~ext xs newscore =
  let xs = File.Path.to_strings xs in
  let dirname_opt =
    match xs with
    | [ x ] when Common2.is_directory x -> Some (Common.fullpath x)
    | _ -> None
  in
  (* nosemgrep *)
  let score_path = (* Config_pfff.regression_data_dir *) "/tmp/pfff" in
  dirname_opt
  |> Option.iter (fun dirname ->
         pr2 "------------------------------";
         pr2 "regression testing information";
         pr2 "------------------------------";
         let str = Str.global_replace (Str.regexp "/") "__" dirname in
         let file =
           Filename.concat score_path
             ("score_parsing__" ^ str ^ ext ^ ".marshalled")
         in
         logger#debug "saving regression info in %s" file;
         Common2.regression_testing newscore file);
  ()

(*****************************************************************************)
(* Most problematic tokens *)
(*****************************************************************************)

(* inspired by a comment by a reviewer of my CC'09 paper *)
let lines_around_error_line ~context (file, line) =
  let arr = Common2.cat_array file in

  let startl = max 0 (line - context) in
  let endl = min (Array.length arr) (line + context) in
  let res = ref [] in

  for i = startl to endl - 1 do
    Common.push arr.(i) res
  done;
  List.rev !res

let print_recurring_problematic_tokens xs =
  let h = Hashtbl.create 101 in
  xs
  |> List.iter (fun x ->
         let file = x.filename in
         x.problematic_lines
         |> List.iter (fun (xs, line_error) ->
                xs
                |> List.iter (fun s ->
                       Common2.hupdate_default s
                         (fun (old, example) -> (old + 1, example))
                         (fun () -> (0, (file, line_error)))
                         h)));
  Common2.pr2_xxxxxxxxxxxxxxxxx ();
  pr2 "maybe 10 most problematic tokens";
  Common2.pr2_xxxxxxxxxxxxxxxxx ();
  Common.hash_to_list h
  |> List.sort (fun (_k1, (v1, _)) (_k2, (v2, _)) -> compare v2 v1)
  |> Common.take_safe 10
  |> List.iter (fun (k, (i, (file_ex, line_ex))) ->
         pr2 (spf "%s: present in %d parsing errors" k i);
         pr2 "example: ";
         let lines = lines_around_error_line ~context:2 (file_ex, line_ex) in
         lines |> List.iter (fun s -> pr2 ("       " ^ s)));
  Common2.pr2_xxxxxxxxxxxxxxxxx ();
  ()
