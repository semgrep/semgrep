val repr : ?unicode_version: int * int -> string -> string
(** [repr s] is the representation of [s] as a Python escaped string. That is,
    the output of {[str.__repr__(s)]} in Python.
    The unicode version used to decide what code points to escape is decided by
    [unicode_version] (default is [Uucp.unicode_version]).
*)
