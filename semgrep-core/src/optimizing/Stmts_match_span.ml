(*
   Track the location of the leftmost and rightmost matched statements,
   when matching a sequence a statements.

   This is not a problem for other types of patterns (expressions, ...),
   which match a single node rather than a sequence.

   Here's the problem:
   We match lists of statements that can either be actual sequences
   of statements in the AST, or a flattened sub-AST. It's for the latter
   that the situation is messy, as some stmts in the list may contain
   other list members, and the rightmost element in the list is not
   necessarily the rightmost element in the source code e.g.
   the source code

     {
       foo();
     }

   is flattened into a Block statement ({ ... }) and an expression statement
   (foo();). If this code matches the pattern, the location of the match
   must include the closing '}'. However, the rightmost token in the source
   is not part of the rightmost statement in the list. Additionally,
   extracting the rightmost location in a given statement is a bit expensive,
   so we want to do this only once we have full match. Until then, we
   simply accumulate the statements into a list whose order doesn't matter,
   and we search for the rightmost location and leftmost location.

   Now there's an issue with caching, in which we cache the result of
   matching the rest of a pattern with the rest of the target AST node.
   When hitting the cache, the result we get back is a full match in which
   the rightmost location is reliable. However, the leftmost location is
   not reliable because the left part of the pattern or source code
   is different from what was seen before (otherwise it would likely
   have been cached and retrieved from the cache). So, the leftmost location
   is given by one set of matching statements and the rightmost location is
   given by another set of matching statements, the one found in the cache.
   This is why we accumulate two sets of statements:
   - left_stmts
   - right_stmts
*)

(* Leftmost and rightmost statements matched by the pattern.
   They're used to determine the matched region when the pattern
   matches a sequence of statements. See details in the big comment above.

   TODO: update the ml grammar in pfff so as to support the
         '| Span of { ... }' syntax in semgrep.
*)

type span = {
  left_stmts : AST_generic.stmt list;
  right_stmts : AST_generic.stmt list;
}

type t = Empty | Span of span

(* Add a statement to the set of statements matched by the current pattern,
   for the sake of eventually determining the match location. *)
let extend stmt span =
  match span with
  | Empty -> Span { left_stmts = [ stmt ]; right_stmts = [ stmt ] }
  | Span { left_stmts; right_stmts } ->
      Span
        { left_stmts = stmt :: left_stmts; right_stmts = stmt :: right_stmts }

let location x =
  match x with
  | Empty -> None
  | Span { left_stmts; right_stmts } ->
      let min_loc, _ = Visitor_AST.range_of_any (AST_generic.Ss left_stmts) in
      let _, max_loc = Visitor_AST.range_of_any (AST_generic.Ss right_stmts) in
      Some (min_loc, max_loc)
  [@@profiling]

let merge_and_deduplicate get_key a b =
  let tbl = Hashtbl.create 100 in
  let acc = ref [] in
  let add values =
    List.iter
      (fun v ->
        let k = get_key v in
        if not (Hashtbl.mem tbl k) then (
          Hashtbl.add tbl k ();
          acc := v :: !acc ))
      values
  in
  add a;
  add b;
  List.rev !acc

let extract_tokens stmts =
  Visitor_AST.ii_of_any (Ss stmts) |> List.filter Parse_info.is_origintok

let is_not_before ~min_loc tok = Parse_info.compare_pos min_loc tok <= 0

let is_not_after ~max_loc tok = Parse_info.compare_pos tok max_loc <= 0

(* Extract a deduplicated list of the original tokens *)
let list_original_tokens x =
  match x with
  | Empty -> []
  | Span { left_stmts; right_stmts } ->
      let left_tokens = extract_tokens left_stmts in
      let right_tokens = extract_tokens right_stmts in
      let min_loc, _ = Visitor_AST.range_of_tokens left_tokens in
      let _, max_loc = Visitor_AST.range_of_tokens right_tokens in
      let left_tokens = List.filter (is_not_after ~max_loc) left_tokens in
      let right_tokens = List.filter (is_not_before ~min_loc) right_tokens in
      (* deduplicate tokens by location *)
      merge_and_deduplicate Parse_info.token_location_of_info left_tokens
        right_tokens
  [@@profiling]
