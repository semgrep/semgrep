(*
   Type of the AST for an aliengrep pattern.

   This will be converted into a PCRE regexp for matching a target file.
*)

type t = node list

and node =
  | Ellipsis
  | Long_ellipsis
  | Metavar of string (* identifier "FOO" only without "$" *)
  | Metavar_ellipsis of string (* same *)
  | Long_metavar_ellipsis of string (* same *)
  | Seq of t
  | Bracket of char * t * char
  | Other of string
