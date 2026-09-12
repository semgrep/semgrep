(*
   Simple, functional pretty-printer.

   Code is presented as a tree of lines and blocks, which is then
   printed with the correct indentation.

   This produces a lot of whitespace but it's easy to maintain,
   and the output and be passed through a dedicated reformatter such
   as refmt if needed.

   Usage:

   open Indent.Types
   ...

   let fmt_binding x =
     [
       Line "let x =";
       Block (fmt_body ...);
     ]

   ...

   let formatted_code = Indent.to_string (fmt_bindings ...)
*)


module Types : sig
  type node =
    | Line of string
    (* single line of output. May be collapsed with other lines
       it's a member of a group. *)

    | Block of node list
    (* a list of things to indent *)

    | Inline of node list
    (* a list of things to not indent *)

    | Space
    (* guarantees the presence of at least one whitespace character
       (space or newline) between the previous and the next element
       to be printed. This is useful when collapsing a group. *)

    | Group of node list
    (* single line of output iff contains a single line, otherwise
       an indented block. e.g.

         (hello)

       or

         (
           hello
           world
         )

       or

         ()
    *)
end

open Types

type t = node list

val to_string : t -> string
val to_channel : out_channel -> t -> unit
val to_file : string -> t -> unit
