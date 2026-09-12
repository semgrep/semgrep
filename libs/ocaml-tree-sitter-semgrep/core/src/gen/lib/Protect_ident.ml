(*
   Change the name of identifiers that would make unsuitable OCaml
   identifiers.
*)

open Printf

let ocaml_keywords = [
  "and";
  "as";
  "assert";
  "asr";
  "begin";
  "class";
  "constraint";
  "do";
  "done";
  "downto";
  "else";
  "end";
  "exception";
  "external";
  "false";
  "for";
  "fun";
  "function";
  "functor";
  "if";
  "in";
  "include";
  "inherit";
  "initializer";
  "";
  "land";
  "lazy";
  "let";
  "lor";
  "lsl";
  "lsr";
  "lxor";
  "match";
  "method";
  "mod";
  "module";
  "mutable";
  "new";
  "nonrec";
  "object";
  "of";
  "open";
  "or";
  "private";
  "rec";
  "sig";
  "struct";
  "then";
  "to";
  "true";
  "try";
  "type";
  "val";
  "virtual";
  "when";
  "while";
  "with";
]

let ocaml_builtin_types = [
  "unit";
  "bool";
  "int";
  "float";
  "string";
  "bytes";
  "list";
  "array";
  "option";
]

let reserved_type_names = [
  "extra";
  "extras";
]

let ocaml_reserved =
  ocaml_keywords @ ocaml_builtin_types @ reserved_type_names

(*
   Map from input identifier to output identifier and vice-versa.
   This map is initialized with the reserved keywords of the output language,
   and then filled with identifiers as they are encountered in the program.
*)
type t = {
  (* Reserved identifiers are the valid source identifiers which
     are invalid destination identifiers. *)
  reserved: (string, unit) Hashtbl.t;

  forward: (string, string) Hashtbl.t;
  backward: (string, string) Hashtbl.t;
}

type translation_status =
  | Available
  | Valid
  | Invalid

let is_reserved map s =
  Hashtbl.mem map.reserved s

let reserve map s =
  if Hashtbl.mem map.backward s then
    invalid_arg (sprintf "Protect_ident.reserve: %S is already in use" s)
  else
    Hashtbl.replace map.reserved s ()

(* Check that src could map to dst and vice-versa. *)
let check map src dst =
  if is_reserved map dst then
    Invalid
  else
    match
      Hashtbl.find_opt map.forward src, Hashtbl.find_opt map.backward dst with
    | None, None -> Available
    | Some dst', Some src' when src' = src && dst' = dst -> Valid
    | _ -> Invalid

let force_add map a b =
  Hashtbl.replace map.forward a b;
  Hashtbl.replace map.backward b a

let create ?(reserved_dst = []) () =
  let map = {
    reserved = Hashtbl.create 100;
    forward = Hashtbl.create 100;
    backward = Hashtbl.create 100;
  } in
  List.iter (reserve map) reserved_dst;
  List.iter (fun src ->
    let dst = src ^ "_" in
    match check map src dst with
    | Available -> force_add map src dst
    | Valid -> ()
    | Invalid -> invalid_arg (sprintf "Protect_ident.create: %s" src)
  ) reserved_dst;
  map

(* Try these suffixes first before falling back to random numbers. *)
let good_suffixes = [
  "";
  "_";
  "2"; "3"; "4"; "5";
]

(*
   Specify a translation from a string src. Honor it if possible, otherwise
   use something as close as possible as preferred_dst.
   Return the actual dst.

   For example, "object" returns "object_" if "object" is reserved.
   "object_" might then return something like "object__" because the
   destination "object_" is already taken.
*)
let add_translation ?preferred_dst map src =
  let preferred_dst =
    match preferred_dst with
    | None -> src
    | Some s -> s
  in
  let rec try_suffix suffixes =
    let candidate_suffix, suffixes =
      match suffixes with
      | [] -> (Random.bits () |> string_of_int), []
      | x :: xs -> x, xs
    in
    let dst = preferred_dst ^ candidate_suffix in
    match check map src dst with
    | Available ->
        force_add map src dst;
        dst
    | Valid ->
        dst
    | Invalid ->
        try_suffix suffixes
  in
  try_suffix good_suffixes

let translate map src =
  Hashtbl.find_opt map.forward src
