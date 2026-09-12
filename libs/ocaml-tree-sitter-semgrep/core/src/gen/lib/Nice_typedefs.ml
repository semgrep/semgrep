(*
   Improve OCaml type definitions and boilerplate destined for human
   consumption:

   1. Undo the inlining of grammar rules due to the 'inline' field
      of the grammar.
   2. Factor out and assign a name to large nodes that occur multiple times.
   3. Inline small definitions that are used only once.
*)

let rearrange_rules grammar =
  grammar
  |> Deinline.deinline_rules
  |> Factorize.factorize_rules
  |> Inline.inline_rules
