
type element_class =
  | Inline
  | Block
  | Essential_block

  | None
  | Everywhere

type model_constraint =
  | Inline2
  | Block2
  | Flow         (* = `Inline or `Block *)
  | Empty
  | Any
  | Special
  | Elements of string list  (* Enumeration of allowed elements *)

  | Or of (model_constraint * model_constraint)
  | Except of (model_constraint * model_constraint)
  | Sub_exclusions of (string list * model_constraint)

type simplified_dtd =
  (string * (element_class * model_constraint)) list

val html40_dtd: simplified_dtd
