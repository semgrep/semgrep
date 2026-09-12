(*
   Generate unique names with a given prefix.
*)

type scope = {
  names: (string, unit) Hashtbl.t;
  prefixes: (string, int ref) Hashtbl.t;
}

let create_scope () =
  {
    names = Hashtbl.create 100;
    prefixes = Hashtbl.create 100;
  }

let register scope name =
  Hashtbl.add scope.names name ()

let exists scope name =
  Hashtbl.mem scope.names name

let create_name scope prefix =
  let rec try_next counter =
    let suffix =
      match !counter with
      | 0 -> ""
      | 1 -> "_"
      | n -> string_of_int n
    in
    let candidate = prefix ^ suffix in
    incr counter;
    if exists scope candidate then (
      try_next counter
    )
    else (
      register scope candidate;
      candidate
    )
  in
  let counter =
    let prefixes = scope.prefixes in
    try Hashtbl.find prefixes prefix
    with Not_found ->
      let counter = ref 0 in
      Hashtbl.add prefixes prefix counter;
      counter
  in
  try_next counter

let add_unique_names scope names =
  List.fold_left (fun duplicates name ->
    if create_name scope name <> name then
      name :: duplicates
    else
      duplicates
  ) [] names

let init_scope names =
  let scope = create_scope () in
  match add_unique_names scope names with
  | [] -> Ok scope
  | duplicates -> Error duplicates
