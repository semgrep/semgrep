(*****************************************************************************)
(* Characters, encoding *)
(*****************************************************************************)

(*
 * On character sets and encodings (src ocamlnet/netstring/netconversion.mli):
 *
 *   A {b character set} is a set of characters where every character is
 *   identified by a {b code point}. An {b encoding} is a way of
 *   representing characters from a set in byte strings. For example,
 *   the Unicode character set has more than 96000 characters, and
 *   the code points have values from 0 to 0x10ffff (not all code points
 *   are assigned yet). The UTF-8 encoding represents the code points
 *   by sequences of 1 to 4 bytes. There are also encodings that
 *   represent code points from several sets, e.g EUC-JP covers four
 *   sets.
 *
 *   pad: so unicode = char set, utf-8 = encoding (char set -> bytes).
 *   A character is a glyph, and a code point is its index in the set.
 *
 * On strings and encodings (src ocamlnet/netstring/netconversion.mli):
 *
 *   In a {b single-byte encoding} every code point is represented by
 *   one byte. This is what many programmers are accustomed at, and
 *   what the O'Caml language specially supports: A [string] is
 *   a sequence of [char]s, where [char] means an 8 bit quantity
 *   interpreted as character.
 *
 *   In a {b multi-byte encoding} there are code points that are represented
 *   by several bytes. As we still represent such text as [string], the
 *   problem arises that a single [char], actually a byte, often represents
 *   only a fraction of a full multi-byte character. There are two solutions:
 *   - Give up the principle that text is represented by [string].
 *     This is, for example, the approach chosen by [Camomile], another O'Caml
 *     library dealing with Unicode. Instead, text is represented as
 *     [int array]. This way, the algorithms processing the text can
 *     remain the same.
 *   - Give up the principle that individual characters can be directly
 *     accessed in a text. This is the primary way chosen by Ocamlnet.
 *     This means that there is not any longer the possibility to read
 *     or write the [n]th character of a text. One can, however, still
 *     compose texts by just concatenating the strings representing
 *     individual characters. Furthermore, it is possible to define
 *     a cursor for a text that moves sequentially along the text.
 *     The consequence is that programmers are restricted to sequential
 *     algorithms. Note that the majority of text processing falls into
 *     this class.
 *
 *  On ASCII compatible  (src ocamlnet/netstring/netconversion.mli):
 *
 *    "ASCII compatible" means: The bytes 1 to 127 represent the ASCII
 *    codes 1 to 127, and no other representation of a character contains
 *    the bytes 1 to 127.
 *
 *    For example, ISO-8859-1 is ASCII-compatible because the byte 1 to
 *    127 mean the same as in ASCII, and all other characters use bytes
 *    greater than 127. UTF-8 is ASCII-compatible for the same reasons,
 *    *it does not matter that there are multi-byte characters*.
 *    EBCDIC is not ASCII-compatible because the bytes 1 to 127 do not mean
 *    the same as in ASCII. UTF-16 is not ASCII-compatible because the bytes
 *    1 to 127 can occur in multi-byte representations of non-ASCII
 *    characters.
 *
 *)
