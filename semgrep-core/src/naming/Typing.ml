open AST_generic
open Lang
module G = AST_generic

(* should use TyBuiltin instead? *)
let make_type type_string tok =
  Some (TyN (Id ((type_string, tok), empty_id_info ())) |> G.t)

let get_resolved_type lang (vinit, vtype) =
  match vtype with
  | Some _ -> vtype
  | None -> (
      (* Should never be reached by languages where the type is in the declaration *)
      (* e.g. Java, C *)
      let string_str =
        match lang with
        | Go -> "str"
        | Js
        | Ts ->
            "string"
        | _ -> "string"
      in
      (* Currently these vary between languages *)
      (* Alternative is to define a TyInt, TyBool, etc in the generic AST *)
      (* so this is more portable across languages *)
      match vinit with
      | Some { e = L (Bool (_, tok)); _ } -> make_type "bool" tok
      | Some { e = L (Int (_, tok)); _ } -> make_type "int" tok
      | Some { e = L (Float (_, tok)); _ } -> make_type "float" tok
      | Some { e = L (Char (_, tok)); _ } -> make_type "char" tok
      | Some { e = L (String (_, tok)); _ } -> make_type string_str tok
      | Some { e = L (Regexp ((_, (_, tok), _), _)); _ } ->
          make_type "regexp" tok
      | Some { e = L (Unit tok); _ } -> make_type "unit" tok
      | Some { e = L (Null tok); _ } -> make_type "null" tok
      | Some { e = L (Imag (_, tok)); _ } -> make_type "imag" tok
      | Some { e = N (Id (_, { id_type; _ })); _ } -> !id_type
      | Some
          {
            e = Call ({ e = IdSpecial (New, _); _ }, (_, ArgType tp :: _, _));
            _;
          } ->
          Some tp
      | _ -> None)
