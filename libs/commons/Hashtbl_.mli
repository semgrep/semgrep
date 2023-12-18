val hash_of_list : ('a * 'b) list -> ('a, 'b) Hashtbl.t
val hash_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val hkeys : ('a, 'b) Hashtbl.t -> 'a list

type 'a hashset = ('a, bool) Hashtbl.t

val hashset_of_list : 'a list -> 'a hashset
val hashset_to_list : 'a hashset -> 'a list

(* Safe replacement for Hashtbl.find_all for OCaml < 5

   In Ocaml < 5, Hashtbl.find_all is not stack-safe and causes Semgrep
   crashes on some input. The alternative below should be used instead
   at least until we don't support OCaml 4.

   Compared to add/find_all, push/get_stack will keep only one
   copy of each key in the table. It reduces the number of calls to
   the 'equal' function when extracting the stack of values for a given
   key. See the original 'hashtbl.ml' in the OCaml distribution for more
   insight.

   push: add a value to the stack associated with a key.
   get_stack: get the stack associated with a key. Values are returned as
              a list, most recently-added first.

   Feel free to add a 'pop' function if needed.
*)
val push : ('k, 'v list ref) Hashtbl.t
val get_stack : ('k, 'v list ref) Hashtbl.t -> 'k -> 'v list
