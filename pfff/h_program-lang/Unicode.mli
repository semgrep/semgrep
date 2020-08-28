(*
   Utilities for dealing with Unicode issues.
*)

val file_contains_non_ascii: Common.filename -> bool

module UTF8 : sig
  (*
     Convert non-ascii whitespace to space characters and
     other characters to 'Z' characters. The number of replacement
     characters is adjusted to as to match the number of bytes of the original
     character.

     This is meant to be used as a hack to tolerate non-ascii input in
     lexers that only support ascii.
  *)
  val asciify : in_channel -> string
end
