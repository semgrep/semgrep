# python-str-repr

Escape strings in OCaml as if you were using Python.

The function `Python_str_repr.repr` replicates the behavior of Python's
`str.__repr__()` - that is, `repr(s)` where `s` is a string.

## FAQ

- **Why not use `String.escaped` or `Printf.sprintf "%S"`?**  
  Good question. If that works for you, great! But sometimes you need to
  closely replicate the behavior of Python. Python will use different quote
  characters (`'`, `"`) depending on the string contents. OCaml also prefers
  octal escapes (`\234`) for non-printable byte values while python uses hex
  escapes (`\x9c`). Finally, Python uses unicode escape sequences for
  non-printable unicode code points while OCaml strings are byte sequences and
  not unicode aware.
- **How about unicode support?**  
  Glad you asked! This library assumes the input is a utf-8 encoded string. So
  called *non-printable* code points are escaped appropriately according to
  what Python considers *non-printable*. Unfortunately, what is considered
  *non-printable* depends on the Unicode version Python was compiled with.
  Luckily, `Python_str_repr` takes an optional argument `?unicode_version :
  (int, int)` which is a pair of major and minor version. The default is to use
  the version Uucp is compiled against.
- **How do I check what version of Unicode my Python deployment uses?**  
  You can check with the following snippet of Python code:
```Python
import unicodedata
unicodedata.unidata_version
```
