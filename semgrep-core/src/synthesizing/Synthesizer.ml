open Common
module J = JSON

let range_to_ast file lang s =
  let r = Range.range_of_linecol_spec s file in
  let { Parse_target.ast; errors; _ } =
    Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
  in
  if errors <> [] then failwith (spf "problem parsing %s" file);
  let a_opt = Range_to_AST.any_at_range r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow ast;
  match a_opt with
  | Some a -> a
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)

let synthesize_patterns config s file =
  let lang = Lang.langs_of_filename file |> List.hd in
  let a = range_to_ast file lang s in
  let patterns = Pattern_from_Code.from_any config a in
  List.map
    (fun (k, v) -> (k, Pretty_print_generic.pattern_to_string lang v))
    patterns

let generate_pattern_choices config s =
  let rec read_input xs =
    match xs with
    | [] -> raise WrongNumberOfArguments
    | [ x ] -> ([], x)
    | x :: xs ->
        let ranges, file = read_input xs in
        (x :: ranges, file)
  in
  let ranges, file = read_input s in
  let lang = Lang.langs_of_filename file |> List.hd in
  let targets = List.map (range_to_ast file lang) ranges in
  List.map
    (fun target ->
      "target: " ^ Pretty_print_generic.pattern_to_string lang target)
    targets
  @ List.map
      (Pretty_print_generic.pattern_to_string lang)
      (Pattern_from_Targets.generate_patterns config targets lang)
