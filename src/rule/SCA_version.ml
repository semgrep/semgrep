(* The "core of a version": a dot separated list of numbers, like 4.1.6.2.7 *)
type core = { major : int; minor : int; incrementals : int list }
[@@deriving show, eq]

(* Used in Rule.ml for sca_dependency_pattern and in SCA_dependency.ml *)
type t =
  | V of core
  (* Versions are sometimes listed as arbitrary strings, like a github URL *)
  | Other of string
[@@deriving show, eq]
