module R = Rule
module RP = Report
module C = Common2
module P = Pattern_match
module M = Metavariable
module JoinRuleMap = Map.Make (String)
module MvarMap = Map.Make (String)

(* Saving some functions that will be useful in pro *)

module MvarSet = Set.Make (struct
  type t = Metavariable.mvalue

  let compare a b = if M.Syntactic.equal_mvalue a b then 0 else compare a b
end)
