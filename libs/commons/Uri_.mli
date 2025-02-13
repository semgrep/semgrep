(* like Uri.of_string but instead of silently returning an
 * empty uri in case of error, we return None here.
 *)
val of_string_opt : string -> Uri.t option

(* rely on Uri.pp *)
val show : Uri.t -> string
val of_fpath : Fpath.t -> Uri.t

(* Checks if the string starts with 'http://' or 'https://'
 * Returns true only for valid HTTP(S) URL prefixes.
 * Note: This only validates the scheme prefix, not the full URL structure.
 *)
val is_url : string -> bool
