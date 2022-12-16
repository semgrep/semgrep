(* very similar to lang_GENERIC/parsing/lang.ml *)

module FT = File_type

type t =
  | PHP
  | OCaml
  | Java
  | Skip
  | Javascript
  | Go
  | Cpp

let lang_of_string_opt = function
  | "phpfuzzy"            -> Some PHP
  | "cfuzzy" | "cppfuzzy" -> Some Cpp
  | "mlfuzzy"             -> Some OCaml
  | "javafuzzy"           -> Some Java
  | "jsfuzzy"             -> Some Javascript
  | "skipfuzzy"           -> Some Skip
  | "fofuzzy"           -> Some Go
  | _ -> None

let lang_of_filename_opt filename =
  let typ = File_type.file_type_of_file filename in
  match typ with
  | FT.PL (FT.Web (FT.Js)) -> Some Javascript
  | FT.PL (FT.Web (FT.Php _)) -> Some PHP
  | FT.PL (FT.C ("c" | "h" )) -> Some Cpp
  | FT.PL (FT.OCaml _) -> Some OCaml
  | FT.PL (FT.Java) -> Some Java
  | FT.PL (FT.Skip) -> Some Skip
  | FT.PL (FT.Go) -> Some Go
  | _ -> None
