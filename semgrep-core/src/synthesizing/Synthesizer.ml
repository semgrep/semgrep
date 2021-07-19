open Common
module J = JSON

let range_to_ast file lang s =
  let r = Range.range_of_linecol_spec s file in
  let { Parse_target.ast; errors; _ } =
    Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
  in
  if errors <> [] then failwith (spf "problem parsing %s" file);
  let a_opt = Range_to_AST.any_at_range_all r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow ast;
  match a_opt with
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)
  | Some a -> a

let synthesize_patterns config s file =
  let lang = Lang.langs_of_filename file |> List.hd in
  let a = range_to_ast file lang s in
  let patterns = Pattern_from_Code.from_any config a in
  List.map
    (fun (k, v) -> (k, Pretty_print_generic.pattern_to_string lang v))
    patterns

let target_to_string lang target =
  "target:\n" ^ Pretty_print_generic.pattern_to_string lang target ^ "\n"

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

let parse_targets (args : string list) : Pattern.t list * Lang.t =
  let ranges, file = parse_range_args args in
  let lang = Lang.langs_of_filename file |> List.hd in
  let targets = List.map (range_to_ast file lang) ranges in
  (targets, lang)

let print_pattern lang targets pattern =
  List.map (target_to_string lang) targets
  @ [ Pretty_print_generic.pattern_to_string lang pattern ]

let generate_pattern_from_targets config s =
  let targets, lang = parse_targets s in
  let pattern = Pattern_from_Targets.generate_patterns config targets lang in
  match pattern with
  | None -> failwith "Unable to infer a pattern from these targets."
  | Some p -> (lang, targets, p)

let print_pattern_from_targets config s =
  let lang, targets, pattern = generate_pattern_from_targets config s in
  print_pattern lang targets pattern
