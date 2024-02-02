(*
static PyObject *
unicode_repr(PyObject *unicode)
{
    PyObject *repr;
    Py_ssize_t isize;
    Py_ssize_t osize, squote, dquote, i, o;
    Py_UCS4 max, quote;
    int ikind, okind, unchanged;
    const void *idata;
    void *odata;

    isize = PyUnicode_GET_LENGTH(unicode);
    idata = PyUnicode_DATA(unicode);

    /* Compute length of output, quote characters, and
       maximum character */
    osize = 0;
    max = 127;
    squote = dquote = 0;
    ikind = PyUnicode_KIND(unicode);
    for (i = 0; i < isize; i++) {
        Py_UCS4 ch = PyUnicode_READ(ikind, idata, i);
        Py_ssize_t incr = 1;
        switch (ch) {
        case '\'': squote++; break;
        case '"':  dquote++; break;
        case '\\': case '\t': case '\r': case '\n':
            incr = 2;
            break;
        default:
            /* Fast-path ASCII */
            if (ch < ' ' || ch == 0x7f)
                incr = 4; /* \xHH */
            else if (ch < 0x7f)
                ;
            else if (Py_UNICODE_ISPRINTABLE(ch))
                max = ch > max ? ch : max;
            else if (ch < 0x100)
                incr = 4; /* \xHH */
            else if (ch < 0x10000)
                incr = 6; /* \uHHHH */
            else
                incr = 10; /* \uHHHHHHHH */
        }
        if (osize > PY_SSIZE_T_MAX - incr) {
            PyErr_SetString(PyExc_OverflowError,
                            "string is too long to generate repr");
            return NULL;
        }
        osize += incr;
    }

    quote = '\'';
    unchanged = (osize == isize);
    if (squote) {
        unchanged = 0;
        if (dquote)
            /* Both squote and dquote present. Use squote,
               and escape them */
            osize += squote;
        else
            quote = '"';
    }
    osize += 2;   /* quotes */

    repr = PyUnicode_New(osize, max);
    if (repr == NULL)
        return NULL;
    okind = PyUnicode_KIND(repr);
    odata = PyUnicode_DATA(repr);

    PyUnicode_WRITE(okind, odata, 0, quote);
    PyUnicode_WRITE(okind, odata, osize-1, quote);
    if (unchanged) {
        _PyUnicode_FastCopyCharacters(repr, 1,
                                      unicode, 0,
                                      isize);
    }
    else {
        for (i = 0, o = 1; i < isize; i++) {
            Py_UCS4 ch = PyUnicode_READ(ikind, idata, i);

            /* Escape quotes and backslashes */
            if ((ch == quote) || (ch == '\\')) {
                PyUnicode_WRITE(okind, odata, o++, '\\');
                PyUnicode_WRITE(okind, odata, o++, ch);
                continue;
            }

            /* Map special whitespace to '\t', \n', '\r' */
            if (ch == '\t') {
                PyUnicode_WRITE(okind, odata, o++, '\\');
                PyUnicode_WRITE(okind, odata, o++, 't');
            }
            else if (ch == '\n') {
                PyUnicode_WRITE(okind, odata, o++, '\\');
                PyUnicode_WRITE(okind, odata, o++, 'n');
            }
            else if (ch == '\r') {
                PyUnicode_WRITE(okind, odata, o++, '\\');
                PyUnicode_WRITE(okind, odata, o++, 'r');
            }

            /* Map non-printable US ASCII to '\xhh' */
            else if (ch < ' ' || ch == 0x7F) {
                PyUnicode_WRITE(okind, odata, o++, '\\');
                PyUnicode_WRITE(okind, odata, o++, 'x');
                PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 4) & 0x000F]);
                PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[ch & 0x000F]);
            }

            /* Copy ASCII characters as-is */
            else if (ch < 0x7F) {
                PyUnicode_WRITE(okind, odata, o++, ch);
            }

            /* Non-ASCII characters */
            else {
                /* Map Unicode whitespace and control characters
                   (categories Z* and C* except ASCII space)
                */
                if (!Py_UNICODE_ISPRINTABLE(ch)) {
                    PyUnicode_WRITE(okind, odata, o++, '\\');
                    /* Map 8-bit characters to '\xhh' */
                    if (ch <= 0xff) {
                        PyUnicode_WRITE(okind, odata, o++, 'x');
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 4) & 0x000F]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[ch & 0x000F]);
                    }
                    /* Map 16-bit characters to '\uxxxx' */
                    else if (ch <= 0xffff) {
                        PyUnicode_WRITE(okind, odata, o++, 'u');
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 12) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 8) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 4) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[ch & 0xF]);
                    }
                    /* Map 21-bit characters to '\U00xxxxxx' */
                    else {
                        PyUnicode_WRITE(okind, odata, o++, 'U');
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 28) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 24) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 20) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 16) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 12) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 8) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[(ch >> 4) & 0xF]);
                        PyUnicode_WRITE(okind, odata, o++, Py_hexdigits[ch & 0xF]);
                    }
                }
                /* Copy characters as-is */
                else {
                    PyUnicode_WRITE(okind, odata, o++, ch);
                }
            }
        }
    }
    /* Closing quote already added at the beginning */
    assert(_PyUnicode_CheckConsistency(repr, 1));
    return repr;
}
   *)

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
  | `Cc | `Cf | `Cn | `Cs | `Co | `Zs | `Zl | `Zp -> false
  | `Po | `Pc | `Sc | `Pd | `Lt | `Pf | `Ps | `Sm | `Nd | `Mc | `Sk | `Lo | `Nl
  | `Ll | `Pi | `So | `Lu | `Pe | `No | `Lm | `Mn | `Me ->
      true

let repr ?unicode_version s =
  let osize, squote, dquote =
    let folder (osize, squote, dquote) _pos c =
      let c = match c with `Uchar c -> c | `Malformed _string -> Uutf.u_rep in
      if Uchar.is_char c && Uchar.to_int c < 128 then
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
    match (squote, dquote) with
    | 0, _ -> ('\'', osize + 2)
    | _non_zero, 0 -> ('"', osize + 2)
    | _non_zero, _non_zero' -> ('\'', osize + squote + 2)
  in
  let uquote = Uchar.of_char quote in
  if osize = String.length s + 2 then Printf.sprintf "%c%s%c" quote s quote
  else
    let b = Buffer.create osize in
    Buffer.add_char b quote;
    let folder () _index uchar =
      let c =
        match uchar with `Uchar c -> c | `Malformed _string -> Uutf.u_rep
      in
      if py_unicode_isprintable ?unicode_version c then
        match Uchar.to_int c with
        | 0x22 (* '\'' *) | 0x27 (* '"' *) ->
            if Uchar.equal c uquote then Buffer.add_char b '\\';
            Buffer.add_utf_8_uchar b c
        | 0x5c (* '\\' *) -> Buffer.add_string b "\\\\"
        | 0x09 (* '\t' *) | 0x0a (* '\n' *) | 0x0d (* '\r' *) ->
            Buffer.add_string b (Char.escaped (Uchar.to_char c))
        | _ -> Buffer.add_utf_8_uchar b c
      else
        let codepoint = Uchar.to_int c in
        match codepoint with
        | 0x09 (* '\t' *) | 0x0a (* '\n' *) | 0x0d (* '\r' *) ->
            Buffer.add_string b (Char.escaped (Uchar.to_char c))
        | _ ->
            if codepoint < 0x100 then Printf.bprintf b "\\x%02x" codepoint
            else if codepoint < 0x10000 then
              Printf.bprintf b "\\u%04x" codepoint
            else Printf.bprintf b "\\U%08x" codepoint
    in
    Uutf.String.fold_utf_8 folder () s;
    Buffer.add_char b quote;
    Buffer.contents b
