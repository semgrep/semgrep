open Common

module J = JSON

let synthesize_patterns s file =
  let r = Range.range_of_linecol_spec s file in
  let lang = Lang.langs_of_filename file |> List.hd in
  let {Parse_target. ast; errors; _ } =
    Parse_target.parse_and_resolve_name_use_pfff_or_treesitter lang file
  in
  if errors <> [] then failwith (spf "problem parsing %s" file);
  let a_opt = Range_to_AST.any_at_range r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate_basic lang ast;
  Constant_propagation.propagate_dataflow ast;
  match a_opt with
  | Some a ->
      let patterns = Pattern_from_Code.from_any a in
      List.map (fun (k, v) -> (k, Pretty_print_generic.pattern_to_string lang v)) patterns
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)
