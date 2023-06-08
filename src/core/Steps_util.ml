module R = Rule

module ResultsMap = Map.Make (struct
  type t = R.rule_id

  let compare a b = compare (R.ID.to_string a) (R.ID.to_string b)
end)

(* Specifically, Metavariable.mvar, but it doesn't derive compare *)
module MvarBindings = Map.Make (String)

type mvars_for_match = Metavariable.mvalue MvarBindings.t
type mvars_for_step = mvars_for_match list
