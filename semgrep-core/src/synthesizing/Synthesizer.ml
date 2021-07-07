open Common
module J = JSON

let range_to_ast file lang s =
  let r = Range.range_of_linecol_spec s file in
  let { Parse_target.ast; errors; _ } =
    Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
  in
  if errors <> [] then failwith (spf "problem parsing %s" file);
  let a_opt = Range_to_AST.many_at_range r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow ast;
  match a_opt with
  | [] -> failwith (spf "could not find an expr at range %s in %s" s file)
  | _ -> a_opt

let synthesize_patterns config s file =
  let lang = Lang.langs_of_filename file |> List.hd in
  (* range_to_ast returns a list of subtrees to be used by Pattern_from_Targets.
   * Pattern_from_Code only handles a single expression, so we take
   * the first element here. This matches previous behavior when range_to_ast
   * returned a single subtree.
   *)
  let a = List.hd (range_to_ast file lang s) in
  let patterns = Pattern_from_Code.from_any config a in
  List.map
    (fun (k, v) -> (k, Pretty_print_generic.pattern_to_string lang v))
    patterns

let print_target lang target =
  let print_pattern_line p = Pretty_print_generic.pattern_to_string lang p in
  let target_str = List.map print_pattern_line target |> String.concat "\n" in
  "target: [\n" ^ target_str ^ "\n]\n"

let parse_range_args s =
  let rec read_input xs =
    match xs with
    | [] -> raise WrongNumberOfArguments
    | [ x ] -> ([], x)
    | x :: xs ->
        let ranges, file = read_input xs in
        (x :: ranges, file)
  in
  read_input s

let parse_targets (args : string list) : Pattern.t list list * Lang.t =
  let ranges, file = parse_range_args args in
  let lang = Lang.langs_of_filename file |> List.hd in
  let targets = List.map (range_to_ast file lang) ranges in
  (targets, lang)

let generate_pattern_choices config s =
  let targets, lang = parse_targets s in
  List.map (print_target lang) targets
  @ List.map
      (Pretty_print_generic.pattern_to_string lang)
      (Pattern_from_Targets.generate_patterns config targets lang)
