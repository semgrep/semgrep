open Common

module J = JSON

let synthesize_patterns s file =
  let r = Range.range_of_linecol_spec s file in
  let lang = Lang.langs_of_filename file |> List.hd in
  let ast = Parse_code.parse_and_resolve_name_use_pfff_or_treesitter lang file in
  let a_opt = Range_to_AST.any_at_range r ast in
  Naming_AST.resolve lang ast;
  Constant_propagation.propagate lang ast;
  match a_opt with
  | Some a ->
       let patterns = Pattern_from_Code.from_any a in
       List.map (fun (k, v) -> (k, Pretty_print_generic.pattern_to_string lang v)) patterns
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)
