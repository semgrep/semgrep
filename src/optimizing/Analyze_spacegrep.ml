(* Yoann Padioleau
 *
 * Copyright (C) 2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
module S = Spacegrep.Pattern_AST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Analyzing a spacegrep pattern for optimization purpose.
 *
 * See Analyze_rule.ml and Analyze_pattern.ml for more information
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Spacegrep removes the leading '$' from metavars in Pattern_AST
 * but the rest of the codebase (including Metavariable.mvar) assumes
 * the '$' is part of the string.
 *)
let mvar_of_str (str : string) : Metavariable.mvar = "$" ^ str

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let extract_strings_and_mvars_spacegrep (pat : Spacegrep.Pattern_AST.t) :
    string list * Metavariable.mvar list =
  let strings = ref [] in
  let mvars = ref [] in

  (* alt: we could define a visitor in spacegrep and even deriving it
   * automatically from the type like we do for AST_generic. However,
   * the situation is different here because there are only a few constructs
   * in the AST so explicitely recursing is simpler and we also get
   * the exhaustive check which is nice.
   *)
  let rec visit xs = xs |> List.iter node
  and node n =
    match n with
    | S.Atom (_loc, a) -> atom a
    | S.List xs -> visit xs
    | S.Dots (_loc, None) -> ()
    | S.Dots (_loc, Some dots_metavar) ->
        Stack_.push (mvar_of_str dots_metavar) mvars
    | S.End -> ()
  and atom a =
    match a with
    (* TODO? filter word more than 4 characters long? *)
    | S.Word str -> Stack_.push str strings
    | S.Punct _char -> ()
    | S.Byte _char -> ()
    | S.Metavar mvar -> Stack_.push (mvar_of_str mvar) mvars
  in
  visit pat;
  (Common2.uniq !strings, Common2.uniq !mvars)
