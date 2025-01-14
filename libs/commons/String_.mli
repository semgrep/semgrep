(* [unit_str cnt prefix] will print "%d %s" cnt prefix
 * but with an additional "s" at the end if the cnt is > 1.
 * ex: unit_str 2 "file" -> "2 files"
 *)
val unit_str : ?pad:bool -> int -> string -> string

val search : term:string -> string -> int option
(** [contains_opt term str] is a {b naive} implementation which tries to find
    [term] into the given [str]. It returns the position where it find the
    {b first} occurrence of [term]. Otherwise, it returns [None]. *)

(* [contains term str] returns true if [term] is inside [str] *)
val contains : term:string -> string -> bool
val empty : string -> bool
val split : sep:string (* regexp *) -> string -> string list

(* Like 'String.sub', extract a substring from a string by specifying
   the start index and the length of the substring.

   This function won't raise out-of-range exceptions:
   - Requesting a substring of negative length is the same as requesting an
     empty substring.
   - The result is the overlap between the string's range and the substring's
     imaginary range.

   For example, 'safe_sub "abc" (-1) 3' is '"ab"'.
*)
val safe_sub : string -> int -> int -> string

(*
   Print an escaped and quoted string, truncating it to show at most max_len
   bytes. The length in bytes is printed if the string has to be truncated.
   The default value for max_len is 200.
   This function is intended for logs.
*)
val show : ?max_len:int -> string -> string

(* Removes a trailing carriage return ('\r') from the given string. For use when
 * reading an input line from an input stream or when splitting a string by the
 * line feed character ('\n'). *)
val trim_cr : string -> string

(* Returns the entirety of all of the lines in the given string that are
 * included in the given range. Strips trailing newline characters from each
 * line. See unit tests for examples. *)
val lines_of_range : int * int -> string -> string list
val is_capitalized : string -> bool

val truncate_with_message : int -> (string -> int -> string) -> string -> string
(** [truncate max_len fmt str] truncates the string [str] to at most [max_len]
    bytes. If the string is truncated, [fmt] is called with the original string
    and the number of characters that were removed. *)
