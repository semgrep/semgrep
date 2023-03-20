let init () =
  (* Those refs are set because the engine is now mostly
   * language independent (so that we can generate small
   * JS files for the playground)
   *)
  Parse_target.just_parse_with_lang_ref := Parse_target_bis.just_parse_with_lang;
  Parse_pattern.parse_pattern_ref := Parse_pattern_bis.parse_pattern;
  ()
