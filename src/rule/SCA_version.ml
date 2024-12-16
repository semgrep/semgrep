(* The "core of a version": a dot separated list of numbers, like 4.1.6.2.7
 * alt: we could inline it in V of { ... } which is allowed in modern OCaml
 * but Match_SCA_mode.ml has also functions operating just on this type
 * so simpler to have a separate type with a proper name.
 *)
type core = { major : int; minor : int; incrementals : int list }
[@@deriving show, eq]

(* Used in Rule.ml for sca_dependency_pattern and in SCA_dependency.ml *)
type t =
  | V of core
  (* Versions are sometimes listed as arbitrary strings, like a github URL *)
  | Other of string
[@@deriving show { with_path = false }, eq]

(* pretty printer *)
let to_string (v : t) : string =
  match v with
  | Other s -> s
  | V { major; minor; incrementals } ->
      major :: minor :: incrementals
      |> List_.map string_of_int |> String.concat "."
