open Rule

(* currently used in Check_rule.ml metachecker *)

(* A more generic formula visitor than the specialized one for xpatterns below *)
let visit_formula f (formula : formula) : unit =
  let rec aux formula =
    f formula;
    (match formula.f with
    | P _ -> ()
    | Anywhere (_, formula)
    | Inside (_, formula)
    | Not (_, formula) ->
        aux formula
    | Or (_, xs)
    | And (_, xs) ->
        xs |> List.iter aux);
    formula.conditions
    |> List.iter (fun (_, cond) ->
           match cond with
           | CondNestedFormula (_, _, formula) -> aux formula
           | CondEval _
           | CondName _
           | CondType _
           | CondRegexp _
           | CondAnalysis _ ->
               ())
  in
  aux formula

(* OK, this is only a little disgusting, but...
   Evaluation order means that we will only visit children after parents.
   So we keep a reference cell around, and set it to true whenever we descend
   under an inside.
   That way, pattern leaves underneath an Inside/Anywhere will properly be
   paired with a true boolean.
*)
let visit_xpatterns (func : Xpattern.t -> inside:bool -> unit)
    (formula : formula) : unit =
  let bref = ref false in
  let rec aux func formula =
    match formula.f with
    | P p -> func p ~inside:!bref
    | Anywhere (_, formula)
    | Inside (_, formula) ->
        Common.save_excursion bref true (fun () -> aux func formula)
    | Not (_, x) -> aux func x
    | Or (_, xs)
    | And (_, xs) ->
        xs |> List.iter (aux func)
  in
  aux func formula

let xpatterns_of_rule (rule : Rule.t) : Xpattern.t list =
  let xs = formulas_of_mode rule.mode in
  let xpat_store = ref [] in
  let visit xpat ~inside:_ = xpat_store := xpat :: !xpat_store in
  List.iter (visit_xpatterns visit) xs;
  !xpat_store
