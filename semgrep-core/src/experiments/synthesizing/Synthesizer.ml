open Common
module J = JSON
module In = Input_to_core_j

let range_to_ast file lang s =
  let r = Range.range_of_linecol_spec s file in
  let { Parse_target.ast; errors; _ } =
    Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
  in
  if errors <> [] then failwith (spf "problem parsing %s" file);
  let a_opt = Range_to_AST.any_at_range_all r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow lang ast;
  match a_opt with
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)
  | Some a -> a

let synthesize_patterns config s file =
  let lang = Lang.langs_of_filename file |> List.hd in
  let a = range_to_ast file lang s in
  let patterns = Pattern_from_Code.from_any config a in
  List.map
    (fun (k, v) -> (k, Pretty_print_pattern.pattern_to_string lang v))
    patterns

let range_of_ast ast = Range.range_of_tokens (Visitor_AST.ii_of_any ast)

let function_from_diff f =
  let f = Common.read_file f in

  let diff_files = In.diff_files_of_string f in
  let functions_from_file f =
    let file = f.In.filename in
    let function_from_range range =
      let r = Range.range_of_line_spec range file in
      let file_ast = Parse_target.parse_program file in
      let func = Range_to_AST.function_at_range r file_ast in
      match func with
      | None -> ()
      | Some func ->
          let func_r =
            let r2_opt = range_of_ast func in
            match r2_opt with
            (* NoTokenLocation issue for the expression, should fix! *)
            | None -> failwith "No range found"
            | Some r2 -> r2
          in
          let func_str = Range.content_at_range file func_r in
          pr2 func_str
    in
    List.iter function_from_range f.In.diffs
  in
  List.iter functions_from_file diff_files

let target_to_string lang target =
  "target:\n" ^ Pretty_print_pattern.pattern_to_string lang target ^ "\n"

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
  @ [ Pretty_print_pattern.pattern_to_string lang pattern ]

let generate_pattern_from_targets config s =
  let targets, lang = parse_targets s in
  let pattern = Pattern_from_Targets.generate_patterns config targets lang in
  match pattern with
  | None -> failwith "Unable to infer a pattern from these targets."
  | Some p -> (lang, targets, p)

let print_pattern_from_targets config s =
  let lang, targets, pattern = generate_pattern_from_targets config s in
  print_pattern lang targets pattern
