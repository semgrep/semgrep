open AST_generic
open Lang

(* should use TyBuiltin instead? *)
let make_type type_string tok =
  Some (TyN (Id ((type_string, tok), empty_id_info ())))

let get_resolved_type lang (vinit, vtype) =
  match vtype with
  | Some _ -> vtype
  | None -> (
      (* Should never be reached by languages where the type is in the declaration *)
      (* e.g. Java, C *)
      let string_str =
        match lang with
        | Go -> "str"
        | Javascript | Typescript -> "string"
        | _ -> "string"
      in
      (* Currently these vary between languages *)
      (* Alternative is to define a TyInt, TyBool, etc in the generic AST *)
      (* so this is more portable across langauges *)
      match vinit with
      | Some (L (Bool (_, tok))) -> make_type "bool" tok
      | Some (L (Int (_, tok))) -> make_type "int" tok
      | Some (L (Float (_, tok))) -> make_type "float" tok
      | Some (L (Char (_, tok))) -> make_type "char" tok
      | Some (L (String (_, tok))) -> make_type string_str tok
      | Some (L (Regexp ((_, (_, tok), _), _))) -> make_type "regexp" tok
      | Some (L (Unit tok)) -> make_type "unit" tok
      | Some (L (Null tok)) -> make_type "null" tok
      | Some (L (Imag (_, tok))) -> make_type "imag" tok
      | Some (N (Id (_, { id_type; _ }))) -> !id_type
      | _ -> None )
