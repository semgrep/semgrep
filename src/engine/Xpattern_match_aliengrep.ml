(*
   Wrapper around the aliengrep matcher (a generic mode variant)
*)

let convert_pos ~file (loc : Aliengrep.Match.loc) =
  (* single "token" spanning the whole match *)
  let bytepos = loc.start in
  let line, column = Xpattern_matcher.line_col_of_charpos file bytepos in
  let pos = Pos.make file ~line ~column bytepos in
  { Tok.str = loc.substring; pos }

let convert_loc ~file (loc : Aliengrep.Match.loc) =
  (* single "token" spanning the whole match *)
  let start_pos = convert_pos ~file loc in
  (* a location is a pair of positions/tokens so we create an empty token
     at the end of the match *)
  let end_pos =
    let bytepos = loc.start + loc.length in
    let line, column = Xpattern_matcher.line_col_of_charpos file bytepos in
    let pos = Pos.make file ~line ~column bytepos in
    { Tok.str = ""; pos }
  in
  (start_pos, end_pos)

let convert_capture ~file
    ((mv : Aliengrep.Pat_compile.metavariable), (loc : Aliengrep.Match.loc)) =
  let str = loc.substring in
  let pos = convert_pos ~file loc in
  let tok = Tok.tok_of_loc pos in
  let name_with_dollar = Aliengrep.Pat_compile.string_of_metavariable mv in
  (name_with_dollar, Xpattern_matcher.mval_of_string str tok)

(* Convert locations to the file/line/column format etc. *)
let convert_match ~file (match_ : Aliengrep.Match.match_) =
  let loc = convert_loc ~file match_.match_loc in
  let env = List_.map (convert_capture ~file) match_.captures in
  (loc, env)

let aliengrep_matcher target_str file pat =
  Aliengrep.Match.search pat target_str |> List_.map (convert_match ~file)

let matches_of_aliengrep patterns lazy_contents (file : Fpath.t) origin =
  let init _ =
    (* TODO: ignore binary files like spacegrep? *)
    (* TODO: preprocess and remove comments like spacegrep does *)
    Some (Lazy.force lazy_contents)
  in
  Xpattern_matcher.matches_of_matcher patterns
    { init; matcher = aliengrep_matcher }
    file origin
