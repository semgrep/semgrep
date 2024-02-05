type t = File of Fpath.t [@@deriving show, eq, ord]

(* TODO: should be private *)
module S = struct
  open Sexplib.Std

  type t = File of string [@@deriving sexp]
end

let sexp_of_t source =
  let x =
    match source with
    | File path -> S.File (Fpath.to_string path)
  in
  S.sexp_of_t x

let t_of_sexp sexp =
  match S.t_of_sexp sexp with
  | S.File path -> File (Fpath.v path)

let to_string (s : t) =
  match s with
  | File path -> Fpath.to_string path

let to_string_opt ?(unspecified = "unknown") (s : t option) =
  match s with
  | Some s -> to_string s
  | None -> Printf.sprintf "<%s>" unspecified
