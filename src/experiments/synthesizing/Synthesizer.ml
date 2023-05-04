open Common
open File.Operators
module J = JSON
module In = Input_to_core_j
module Out = Output_from_core_j

let range_to_ast file lang s =
  let r = Range.range_of_linecol_spec s !!file in
  let ast = Parse_target.parse_and_resolve_name_fail_if_partial lang !!file in
  let a_opt = Range_to_AST.any_at_range_all r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow lang ast;
  match a_opt with
  | None -> failwith (spf "could not find an expr at range %s in %s" s !!file)
  | Some a -> a

let synthesize_patterns config s file =
  let lang = Lang.lang_of_filename_exn file in
  let a = range_to_ast file lang s in
  let patterns = Pattern_from_Code.from_any config a in
  Common.map
    (fun (k, v) -> (k, Pretty_print_pattern.pattern_to_string lang v))
    patterns

let locate_patched_functions f =
  let f = File.read_file f in

  let d = In.diff_files_of_string f in
  let diff_files = d.In.cve_diffs in
  let diffs = Common.map Pattern_from_diff.pattern_from_diff diff_files in
  Out.string_of_cve_results diffs

let target_to_string lang target =
  "target:\n" ^ Pretty_print_pattern.pattern_to_string lang target ^ "\n"

let parse_range_args xs =
  let rec read_input xs =
    match xs with
    | [] -> raise Arg_helpers.WrongNumberOfArguments
    | [ file ] -> ([], Fpath.v file)
    | x :: xs ->
        let ranges, file = read_input xs in
        (x :: ranges, file)
  in
  read_input xs

let parse_targets (args : string list) : Pattern.t list * Lang.t =
  let ranges, file = parse_range_args args in
  let lang = Lang.lang_of_filename_exn file in
  let targets = Common.map (range_to_ast file lang) ranges in
  (targets, lang)

let print_pattern lang targets pattern =
  Common.map (target_to_string lang) targets
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
