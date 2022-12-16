open Ast_html

let rec vof_info v =
  Meta_parse_info.vof_info_adjustable_precision v

and vof_wrap _of_a (v1, v2) =
  let v1 = _of_a v1 and v2 = vof_info v2 in OCaml.VTuple [ v1; v2 ]

let rec vof_html_tree =
  function
  | Element (v1, v2, v3) ->
      let v1 = vof_tag v1
      and v2 =
        OCaml.vof_list
          (fun (v1, v2) ->
             let v1 = vof_attr_name v1
             and v2 = vof_attr_value v2
             in OCaml.VTuple [ v1; v2 ])
          v2
      and v3 = OCaml.vof_list vof_html_tree v3
      in OCaml.VSum ("Element", [ v1; v2; v3 ])
  | Data v1 ->
      let v1 = vof_wrap OCaml.vof_string v1 in OCaml.VSum ("Data", [ v1 ])
and vof_tag =
  function
  | Tag v1 ->
      let v1 = vof_wrap OCaml.vof_string v1 in OCaml.VSum ("Tag", [ v1 ])
and vof_attr_name =
  function
  | Attr v1 ->
      let v1 = vof_wrap OCaml.vof_string v1 in OCaml.VSum ("Attr", [ v1 ])
and vof_attr_value =
  function
  | Val v1 ->
      let v1 = vof_wrap OCaml.vof_string v1 in OCaml.VSum ("Val", [ v1 ])
