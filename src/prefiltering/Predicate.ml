(* Needed to derive hash *)
let hash_fold_string : Base.Hash.state -> string -> Base.Hash.state =
  Base.hash_fold_string

type t = String of string | Regex of Pcre2_.t [@@deriving show, eq, ord, hash]

let eval (predicate : t) (content : string) : bool =
  match predicate with
  | String id ->
      let re = Pcre2_.matching_exact_string id in
      Pcre2_.unanchored_match ~on_error:true re content
  | Regex re -> Pcre2_.unanchored_match ~on_error:true re content
