(*
   Fast check that tells whether a pattern has any chance of matching the
   document with 'Match.search'.

   In the future, it may be superseded by an equivalent pre-parse
   optimization that operates directly on the source files rather than the
   AST. It would be better since it turns out parsing is usually more
   expensive than matching, and that would allow skipping the parsing step.
   See https://github.com/semgrep/semgrep/issues/3461
*)

(*
   Check if the pattern contains 'Byte' tokens, since they often don't
   and the target may contain many of them. This is meant as an optimization
   to avoid making many useless table lookups when scanning the target.
*)
let pattern_has_bytes (pat : Pattern_AST.t) =
  let open Pattern_AST in
  let rec has_bytes = function
    | Atom (_, atom) -> (
        match atom with
        | Byte _ -> true
        | Word _
        | Punct _
        | Metavar _ ->
            false)
    | List nodes -> List.exists has_bytes nodes
    | Dots _
    | End ->
        false
  in
  List.exists has_bytes pat

(* This is used when case-insensitive matching on words is requested. *)
let lowercase = String.lowercase_ascii

type atom_table = {
  words : (string, unit) Hashtbl.t;
  chars : (char, unit) Hashtbl.t; (* could use an array of 256 booleans *)
}

let build_atom_table ~case_sensitive (pat : Pattern_AST.t) =
  let open Pattern_AST in
  (* Initial sizes for these hash tables reduced based on a memtrace
   * investigation. Increase them only with caution. *)
  let words = Hashtbl.create 64 in
  let chars = Hashtbl.create 32 in
  let rec index nodes = List.iter index_node nodes
  and index_node = function
    | Atom (_, atom) -> index_atom atom
    | List nodes -> index nodes
    | Dots _
    | End ->
        ()
  and index_atom = function
    | Word word ->
        let k = if case_sensitive then word else lowercase word in
        Hashtbl.replace words k ()
    | Punct k
    | Byte k ->
        Hashtbl.replace chars k ()
    | Metavar _ -> ()
  in
  index pat;
  { words; chars }

(*
   Warning: the atom table cannot be reused after this, since we operate
   by removing the elements from the table.
*)
let doc_has_all_required_atoms ~case_sensitive ~check_bytes atom_tbl
    (doc : Doc_AST.t) =
  let open Doc_AST in
  let { words; chars } = atom_tbl in
  let rec check nodes = List.iter check_node nodes
  and check_node = function
    | Atom (_, atom) -> check_atom atom
    | List nodes -> check nodes
  and check_atom = function
    | Byte k -> if check_bytes then Hashtbl.remove chars k
    | Word word ->
        let k = if case_sensitive then word else lowercase word in
        Hashtbl.remove words k
    | Punct k -> Hashtbl.remove chars k
  in
  check doc;
  Hashtbl.length words = 0 && Hashtbl.length chars = 0

let may_match ~case_sensitive pat doc =
  let atom_tbl = build_atom_table ~case_sensitive pat in
  let has_bytes = pattern_has_bytes pat in
  doc_has_all_required_atoms ~case_sensitive ~check_bytes:has_bytes atom_tbl doc
