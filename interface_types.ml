[@@@ocaml.warning "-27-30-39"]

type position = { line : int32; col : int32; offset : int32 }

type location = {
  path : string;
  start : position option;
  end_ : position option;
  lines : string list;
}

let rec default_position ?(line : int32 = 0l) ?(col : int32 = 0l)
    ?(offset : int32 = 0l) () : position =
  { line; col; offset }

let rec default_location ?(path : string = "") ?(start : position option = None)
    ?(end_ : position option = None) ?(lines : string list = []) () : location =
  { path; start; end_; lines }
