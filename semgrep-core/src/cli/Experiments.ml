open Common
module PI = Parse_info

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
  pr2 (spf "matched: %d" (List.length matches));
  let per_files =
    matches
    |> List.map (fun m -> (m.Pattern_match.file, m))
    |> Common.group_assoc_bykey_eff
    |> List.map (fun (file, xs) -> (file, List.length xs))
    |> Common.sort_by_val_highfirst |> Common.take_safe 10
  in
  pr2 "biggest file offenders";
  per_files |> List.iter (fun (file, n) -> pr2 (spf " %60s: %d" file n));
  ()

module T = Genlex

let ebnf_to_menhir file =
  let xs = Common.cat file in
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
      :: ((T.Ident _ | T.String _ | T.Kwd "(") as y) :: xs ->
        x :: T.Kwd " " :: insert_space_when_needed (y :: xs)
    | x :: xs -> x :: insert_space_when_needed xs
  in
  let process s =
    let chars = Stream.of_string s in
    let tokens = lexer chars in
    let xs = Stream.npeek 100 tokens in
    xs |> insert_space_when_needed
    |> List.map (function
         | T.Kwd s -> spf "%s" s
         | T.Ident s ->
             if s =~ "^[A-Z]" then lower s
             else (
               Hashtbl.replace htokens (upper s) true;
               upper s )
         | T.Int _ | T.Float _ | T.Char _ -> raise Impossible
         | T.String s ->
             Hashtbl.replace hkwd s true;
             spf "\"%s\"" s)
    |> String.concat ""
  in
  let ys =
    xs
    |> List.map (fun s ->
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
  pr "%{";
  pr "%}";
  pr "";

  htokens |> Common.hashset_to_list
  |> List.iter (fun s -> pr (spf "%%token <unit> %s" s));

  let i = ref 0 in
  hkwd |> Common.hashset_to_list
  |> List.iter (fun s ->
         incr i;
         pr (spf "%%token <unit> X%d \"%s\"" !i s));
  pr "%start <unit> compilationUnit";
  pr "%%";
  pr "";

  ys |> List.iter (fun s -> pr s)

let layer_file = ref (None : filename option)

(* a layer need readable path, hence the ~root argument *)
let gen_layer ~root ~query _matching_tokens file =
  ignore query;
  pr2 ("generating layer in " ^ file);

  let root = Common2.relative_to_absolute root in

  let toks = !_matching_tokens in
  let kinds = [ ("m" (* match *), "red") ] in

  (* todo: could now use Layer_code.simple_layer_of_parse_infos *)
  let files_and_lines =
    toks
    |> List.map (fun tok ->
           let file = PI.file_of_info tok in
           let line = PI.line_of_info tok in
           let file = Common2.relative_to_absolute file in
           (Common.readable root file, line))
  in
  let group = Common.group_assoc_bykey_eff files_and_lines in
  let layer =
    {
      Layer_code.title = "Sgrep";
      description = "output of sgrep";
      kinds;
      files =
        group
        |> List.map (fun (file, lines) ->
               let lines = Common2.uniq lines in
               ( file,
                 {
                   Layer_code.micro_level =
                     lines |> List.map (fun l -> (l, "m"));
                   macro_level = (if null lines then [] else [ ("m", 1.) ]);
                 } ));
    }
  in
  Layer_code.save_layer layer file;
  ()

let gen_layer_maybe _matching_tokens pattern_string xs =
  !layer_file
  |> Common.do_option (fun file ->
         let root = Common2.common_prefix_of_files_or_dirs xs in
         gen_layer ~root ~query:pattern_string _matching_tokens file)
