(*
   A generic tree type meant to accommodate all tree types derived
   from tree-sitter grammars.

   Such generic trees are constructed automatically as part of the
   generated boilerplate (Boilerplate.ml files). They are used
   to represent nodes that are waiting for a partial or full manual
   translation.
*)

(*
   Type of a raw tree.

   Question 1:

   Should we include type names? We don't have type names for all constructs
   but when we do, it could allow us to start a semgrep pattern search
   from a node of a specific type. It's not clear whether it's something
   we want to support.

   Question 2:

   Should we use a simpler tree type like the following?

     type 'a t =
       | Atom of Token.t
       | List of 'a t list
       | Any of 'a

   This may make it harder to modify the generated boilerplate.
   For example, we could decide to support semgrep ellipses within all the
   lists but not in tuples.

   For debugging purposes, a more precise tree is better since it allows
   us to pretty-print more accurately.
*)
type 'a t =
  | Token of Token.t (* keyword, identifier, punctuation, int literal, ... *)
  | List of 'a t list (* sequence of variable length [repeat] *)
  | Tuple of 'a t list (* sequence of fixed length (wrt type) [seq] *)
  | Case of string * 'a t (* tagged value = variant [choice] *)
  | Option of 'a t option (* optional value [optional] *)
  | Any of 'a (* allows integration in a richer tree type *)

val format :
  ?format_any: ('a -> Tree_sitter_gen.Indent.t) ->
  'a t -> Tree_sitter_gen.Indent.t

val to_string :
  ?format_any: ('a -> Tree_sitter_gen.Indent.t) ->
  'a t -> string

val to_channel :
  ?format_any: ('a -> Tree_sitter_gen.Indent.t) ->
  out_channel ->
  'a t -> unit
