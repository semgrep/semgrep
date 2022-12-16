
(* many refactorings can be done by spatch! used this code only as
 * last resort
*)
type refactoring_kind =
  | AddInterface of string option (* specific class *)
                    * string (* the interface to add *)
  | RemoveInterface of string option * string

  | SplitMembers

  (* type related *)
  | AddReturnType of string
  | AddTypeHintParameter of string
  | OptionizeTypeParameter
  | AddTypeMember of string

type position = {
  file: Common.filename;
  line: int;
  col: int;
}

type refactoring = refactoring_kind * position option

val load: Common.filename -> refactoring list
