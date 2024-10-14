(*
   Predefined character classes.

   Uses the definitions of PCRE unless otherwise noted.
   https://www.pcre.org/original/doc/html/pcrepattern.html

   Many different Unicode classes are defined and supported by PCRE via
   \p{Foo} and \P{Foo} (complement). These are recognized by our parser
   as being character classes but we don't expand them.
*)

open AST

(**************************************************************************)
(* Helpers *)
(**************************************************************************)

(*
   Convert a list of unicode code points into a set.
*)
let rec of_list chars : char_class =
  match chars with
  | [] -> Empty
  | [ last ] -> Singleton last
  | x :: chars -> Union (Singleton x, of_list chars)

let of_list_pos chars : char_class = List_.map snd chars |> of_list

(*
   Each byte of the list becomes a unicode code point in the set.
   Use with ascii strings only.
*)
let of_string s : char_class = of_list (AST.code_points_of_ascii_string s)

(**************************************************************************)
(* POSIX character classes *)
(**************************************************************************)

let posix_lower = Range (Char.code 'a', Char.code 'z')
let posix_upper = Range (Char.code 'A', Char.code 'Z')
let posix_digit = Range (Char.code '0', Char.code '9')

let posix_xdigit =
  Union
    ( posix_digit,
      Union
        ( Range (Char.code 'A', Char.code 'Z'),
          Range (Char.code 'a', Char.code 'z') ) )

let posix_alpha = Union (posix_lower, posix_upper)
let posix_alnum = Union (posix_alpha, posix_digit)

let posix_punct =
  Union
    ( Range (Char.code '!', Char.code '/'),
      Union
        ( Range (Char.code ':', Char.code '@'),
          Union
            ( Range (Char.code '[', Char.code '`'),
              Range (Char.code '{', Char.code '~') ) ) )

let posix_graph = Union (posix_alnum, posix_punct)
let posix_print = Union (Singleton (Char.code ' '), posix_graph)

let posix_cntrl =
  Union
    (Range (Char.code '\x00', Char.code '\x1F'), Singleton (Char.code '\x7F'))

let posix_space = Range (Char.code '\t', Char.code '\r')
let posix_ascii = Range (0, 127)

(*** Non-POSIX extensions that use the same "POSIX" syntax [: ... :] ***)

(* [:word:] Perl/PCRE, ascii mode *)
let perl_word = Union (posix_alnum, Singleton (Char.code '_'))

(* [:blank:] Perl/GNU/PCRE *)
let perl_blank = of_list [ Char.code '\t'; Char.code ' ' ]

(*** Other ascii ***)

(* \s Perl >= 5.18 or PCRE, ascii mode *)
let perl_whitespace = Range (Char.code '\t', Char.code '\r')

(**************************************************************************)
(* Other character classes *)
(**************************************************************************)

let unicode_horizontal_whitespace =
  of_list
    [
      0x0009;
      (* Horizontal tab (HT) *)
      0x0020;
      (* Space *)
      0x00A0;
      (* Non-break space *)
      0x1680;
      (* Ogham space mark *)
      0x180E;
      (* Mongolian vowel separator *)
      0x2000;
      (* En quad *)
      0x2001;
      (* Em quad *)
      0x2002;
      (* En space *)
      0x2003;
      (* Em space *)
      0x2004;
      (* Three-per-em space *)
      0x2005;
      (* Four-per-em space *)
      0x2006;
      (* Six-per-em space *)
      0x2007;
      (* Figure space *)
      0x2008;
      (* Punctuation space *)
      0x2009;
      (* Thin space *)
      0x200A;
      (* Hair space *)
      0x202F;
      (* Narrow no-break space *)
      0x205F;
      (* Medium mathematical space *)
      0x3000 (* Ideographic space *);
    ]

let unicode_vertical_whitespace =
  of_list
    [
      0x000A;
      (* Linefeed (LF) *)
      0x000B;
      (* Vertical tab (VT) *)
      0x000C;
      (* Form feed (FF) *)
      0x000D;
      (* Carriage return (CR) *)
      0x0085;
      (* Next line (NEL) *)
      0x2028;
      (* Line separator *)
      0x2029 (* Paragraph separator *);
    ]

(* UCP mode: \d  any character that matches \p{Nd} (decimal digit) *)
let unicode_digit = Abstract (Unicode_character_property "Nd")

(* UCP mode: \s  any character that matches \p{Z} or \h or \v *)
let unicode_whitespace =
  Union
    ( Abstract (Unicode_character_property "Z"),
      Union (unicode_horizontal_whitespace, unicode_vertical_whitespace) )

(* UCP mode: \w  any character that matches \p{L} or \p{N}, plus underscore *)
let unicode_word =
  Union
    ( Singleton (Char.code '_'),
      Union
        ( Abstract (Unicode_character_property "L"),
          Abstract (Unicode_character_property "N") ) )
