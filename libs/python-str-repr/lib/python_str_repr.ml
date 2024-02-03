(** From {[Py_UNICODE_ISPRINTABLE]} documentation:
    Return [true] or [false] depending on whether [uchar] is a printable character.
    Nonprintable characters are those characters defined in the Unicode
    character database as "Other" or "Separator", excepting the ASCII space
    (0x20) which is considered printable.  (Note that printable characters in
    this context are those which should not be escaped when Python's {[repr]}
    is invoked on a string.) *)
let py_unicode_isprintable ?unicode_version uchar =
  (* {[    if char == ord(" ") or category[0] not in ("C", "Z"):
           flags |= PRINTABLE_MASK]} *)
  Uchar.equal uchar (Uchar.of_char ' ')
  ||
  let age = Uucp.Age.age uchar in
  (match (age, unicode_version) with
  | `Unassigned, _ -> false
  | `Version _, None -> true
  | `Version (major, minor), Some (major', minor') ->
      major < major' || (major = major' && minor <= minor'))
  &&
  let gc = Uucp.Gc.general_category uchar in
  (* Not those categories starting with 'C' or 'Z' *)
  match gc with
  | `Cc | `Cf | `Cn | `Co | `Cs | `Zl | `Zp | `Zs -> false
  | `Ll | `Lm | `Lo | `Lt | `Lu | `Mc | `Me | `Mn | `Nd | `Nl | `No | `Pc | `Pd
  | `Pe | `Pf | `Pi | `Po | `Ps | `Sc | `Sk | `Sm | `So ->
      true

(** This function is based on {[unicode_repr()]} from
    cpython:Objects/unicodeobject.c. *)
let repr ?unicode_version s =
  (* Compute preliminary output size and count number of quote characters *)
  let osize, squote, dquote =
    let folder (osize, squote, dquote) _pos c =
      let c =
        (* NOTE: we replace bad utf-8 sequences with [Uutf.u_rep]. This is
         * similar to in python calling {[bytes.decode(.., errors='replace')]}.
         * We could implement other strategies... *)
        match c with `Uchar c -> c | `Malformed _string -> Uutf.u_rep
      in
      if Uchar.is_char c && Uchar.to_int c < 128 then
        (* US ASCII is special *)
        match Uchar.to_char c with
        | '\'' -> (succ osize, succ squote, dquote)
        | '"' -> (succ osize, squote, succ dquote)
        | '\\' | '\t' | '\r' | '\n' -> (osize + 2, squote, dquote)
        | '\x00' .. '\x1f' (* c < ' ' *) | '\x7f' -> (osize + 4, squote, dquote)
        | _printable -> (succ osize, squote, dquote)
      else if py_unicode_isprintable ?unicode_version c then
        (osize + Uchar.utf_8_byte_length c, squote, dquote)
      else
        let utf8_byte_len = Uchar.utf_8_byte_length c in
        (osize + 2 + (2 * utf8_byte_len), squote, dquote)
    in
    Uutf.String.fold_utf_8 folder (0, 0, 0) s
  in
  let quote, osize =
    (* We use single quotes unless the string has single quotes but no double
     * quotes.
     * We add the surrounding quotes and the number of extra backslashes
     * escaping the quote character we may need to the output size. *)
    match (squote, dquote) with
    | 0, _ -> ('\'', osize + 2)
    | _non_zero, 0 -> ('"', osize + 2)
    | _non_zero, _non_zero' -> ('\'', osize + squote + 2)
  in
  let uquote = Uchar.of_char quote in
  (* The easy case: nothing needs to be escaped. *)
  if osize = String.length s + 2 then Printf.sprintf "%c%s%c" quote s quote
  else
    let b = Buffer.create osize in
    Buffer.add_char b quote;
    let folder () _index uchar =
      let c =
        (* See the comment earlier. *)
        match uchar with `Uchar c -> c | `Malformed _string -> Uutf.u_rep
      in
      (* If the codepoint is considered printable by Python we use it unescaped
       * except for a few cases in the US ASCII range. *)
      if py_unicode_isprintable ?unicode_version c then
        match Uchar.to_int c with
        | 0x22 (* '\'' *) | 0x27 (* '"' *) ->
            if Uchar.equal c uquote then Buffer.add_char b '\\';
            Buffer.add_utf_8_uchar b c
        | 0x5c (* '\\' *) -> Buffer.add_string b "\\\\"
        | _ -> Buffer.add_utf_8_uchar b c
      else
        let codepoint = Uchar.to_int c in
        match codepoint with
        | 0x09 (* '\t' *) | 0x0a (* '\n' *) | 0x0d (* '\r' *) ->
            (* These US ASCII codepoints are not considered printable, but have
             * special escape sequences. *)
            Buffer.add_string b (Char.escaped (Uchar.to_char c))
        | _ ->
            (* Use the shortest possible escape sequence. '\xHH' is, unlike in
             * OCaml, considered a unicode code point escape sequence. *)
            if codepoint < 0x100 then Printf.bprintf b "\\x%02x" codepoint
            else if codepoint < 0x10000 then
              Printf.bprintf b "\\u%04x" codepoint
            else Printf.bprintf b "\\U%08x" codepoint
    in
    Uutf.String.fold_utf_8 folder () s;
    Buffer.add_char b quote;
    Buffer.contents b
