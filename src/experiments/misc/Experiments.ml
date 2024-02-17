open Common
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A pot-pourri of experiments.
*)

(*****************************************************************************)
(* Experiments *)
(*****************************************************************************)

(* We now log the files who have too many matches, but this action below
 * can still be useful for deeper debugging.
 *)
let stat_matches file =
  let (matches : Pattern_match.t list) = Common2.get_value file in
  UCommon.pr2 (spf "matched: %d" (List.length matches));
  let per_files =
    matches
    |> List_.map (fun (m : Pattern_match.t) ->
           (m.path.internal_path_to_content, m))
    |> Assoc.group_assoc_bykey_eff
    |> List_.map (fun (file, xs) -> (file, List.length xs))
    |> Assoc.sort_by_val_highfirst |> List_.take_safe 10
  in
  UCommon.pr2 "biggest file offenders";
  per_files
  |> List.iter (fun (file, n) -> UCommon.pr2 (spf " %60s: %d" !!file n));
  ()

module T = Genlex

let ebnf_to_menhir file =
  let xs = UFile.Legacy.cat file in
  let hkwd = Hashtbl.create 11 in
  let htokens = Hashtbl.create 11 in
  let lower = String.uncapitalize_ascii in
  let upper = String.capitalize_ascii in
  let lexer = Genlex.make_lexer [ "("; ")"; "*"; "?" ] in
  let rec insert_space_when_needed = function
    | [] -> []
    | (T.Kwd (")" | "*" | "?") as x) :: ((T.Ident _ | T.String _) as y) :: xs ->
        x :: T.Kwd " " :: insert_space_when_needed (y :: xs)
    | ((T.Ident _ | T.String _) as x)
      :: ((T.Ident _ | T.String _ | T.Kwd "(") as y)
      :: xs ->
        x :: T.Kwd " " :: insert_space_when_needed (y :: xs)
    | x :: xs -> x :: insert_space_when_needed xs
  in
  let process s =
    let chars = Stream.of_string s in
    let tokens = lexer chars in
    let xs = Stream.npeek 100 tokens in
    xs |> insert_space_when_needed
    |> List_.map (function
         | T.Kwd s -> spf "%s" s
         | T.Ident s ->
             if s =~ "^[A-Z]" then lower s
             else (
               Hashtbl.replace htokens (upper s) true;
               upper s)
         | T.Int _
         | T.Float _
         | T.Char _ ->
             raise Impossible
         | T.String s ->
             Hashtbl.replace hkwd s true;
             spf "\"%s\"" s)
    |> String.concat ""
  in
  let ys =
    xs
    |> List_.map (fun s ->
           match s with
           | _ when s =~ "^ *\\([A-Z][a-zA-Z0-9]*\\) +::= \\(.*\\)$" ->
               let s1, s2 = Common.matched2 s in
               let s2 = process s2 in
               let s1 = lower s1 in
               spf "%s: %s { }" s1 s2
           | _ when s =~ "^ *| \\(.*\\)$" ->
               let s2 = Common.matched1 s in
               let s2 = process s2 in
               spf " | %s { }" s2
           | _ when s =~ "^[ \t]*$" -> ""
           | _ -> failwith (spf "not handled: %s" s))
  in
  UCommon.pr2 "%{";
  UCommon.pr2 "%}";
  UCommon.pr2 "";

  htokens |> Hashtbl_.hashset_to_list
  |> List.iter (fun s -> UCommon.pr2 (spf "%%token <unit> %s" s));

  let i = ref 0 in
  hkwd |> Hashtbl_.hashset_to_list
  |> List.iter (fun s ->
         incr i;
         UCommon.pr2 (spf "%%token <unit> X%d \"%s\"" !i s));
  UCommon.pr2 "%start <unit> compilationUnit";
  UCommon.pr2 "%%";
  UCommon.pr2 "";

  ys |> List.iter (fun s -> UCommon.pr2 s)
