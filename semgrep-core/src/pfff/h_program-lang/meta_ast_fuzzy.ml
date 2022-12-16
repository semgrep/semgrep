open Ast_fuzzy

let vof_token t =
  OCaml.VString (Parse_info.str_of_info t)
(* Parse_info.vof_token t*)

let rec vof_multi_grouped =
  function
  | Braces (v1, v2, v3) ->
      let v1 = vof_token v1
      and v2 = OCaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in OCaml.VSum ("Braces", [ v1; v2; v3 ])
  | Parens (v1, v2, v3) ->
      let v1 = vof_token v1
      and v2 = OCaml.vof_list (OCaml.vof_either vof_trees vof_token) v2
      and v3 = vof_token v3
      in OCaml.VSum ("Parens", [ v1; v2; v3 ])
  | Angle (v1, v2, v3) ->
      let v1 = vof_token v1
      and v2 = OCaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in OCaml.VSum ("Angle", [ v1; v2; v3 ])
  | Bracket (v1, v2, v3) ->
      let v1 = vof_token v1
      and v2 = OCaml.vof_list vof_multi_grouped v2
      and v3 = vof_token v3
      in OCaml.VSum ("Bracket", [ v1; v2; v3 ])
  | Metavar v1 -> let v1 = vof_wrap v1 in OCaml.VSum ("Metavar", [ v1 ])
  | Dots v1 -> let v1 = vof_token v1 in OCaml.VSum ("Dots", [ v1 ])
  | Tok v1 -> let v1 = vof_wrap v1 in OCaml.VSum ("Tok", [ v1 ])
and vof_wrap (s, _x) = OCaml.VString s
and vof_trees xs =
  OCaml.VList (xs |> List.map vof_multi_grouped)
