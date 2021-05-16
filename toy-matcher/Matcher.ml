(*
   A matcher operating on sequences of symbols,
   using the same basic matching algorithm as semgrep-core.

   The goals are:

   - Clarify semgrep-core's matching algorithm by not having to deal with any
     of the real-world complexity.
   - Keep the semgrep-core implementation separate and easy to follow,
     without functors.

   Differences with semgrep:

   - There is no pattern syntax. There is no useful executables, only
     tests with patterns constructed directly in OCaml.

   - As in traditional regexp syntaxes, captures and backreferences
     are made using different syntax e.g.:

                       PCRE         Semgrep
       capture:        (.)          $X
       backreference   \1           $X

     Here, like in regexps, we distinguish the first reference to a
     "metavariable", which requests a capture, from later references.
     This hopefully makes it easier to understand the internals,
     which rely on left-to-right matching.

     The distinction between captures and patterns to be matched allows
     for a bit of experimentation.
*)

open Printf
module Names = Set.Make (String)

type pattern_atom =
  | Any_symbol (* match any single symbol *)
  | Symbol of char (* match a specific symbol *)
  | Ellipsis (* match any sequence of symbols *)
  | Backref of string

(* match the same thing that was previous
   matched by the pattern atom of that name. *)

(* A simple pattern tree, which is in fact a list, meant to match
   input that's a list of symbols. *)
and pattern_value = Cons of pattern_atom * pattern | Nil

(* A pattern node, decorated with generic attributes. *)
and pattern = {
  pat_val : pattern_value;
  (* Optional capture (metavariable). This is a variable to bind to
     a matching AST node. *)
  capture_name : string option;
  (* Set of metavariables accessed in rest of the pattern.
     It's used during matching to determine whether a bound metavariable
     should go into the cache key.

     This is the set of variables dereferenced somewhere in the rest of the
     pattern. In a pattern more complicated than a list, the rest of the
     pattern corresponds to all the branches of the pattern that remain to
     be matched against the input AST; not just the subtree under the current
     node. *)
  backrefs : Names.t;
}

type input = char list

let rec pat_equal a b =
  a.capture_name = b.capture_name && pat_val_equal a.pat_val b.pat_val

and pat_val_equal a b =
  match (a, b) with
  | Cons (a_at, a_pat), Cons (b_at, b_pat) ->
      pat_atom_equal a_at b_at && pat_equal a_pat b_pat
  | Nil, Nil -> true
  | _ -> false

and pat_atom_equal (a : pattern_atom) (b : pattern_atom) = a = b

(* Captured subsequences. *)
module Bindings = Map.Make (String)

type env = {
  (* All captures *)
  full_env : char list Bindings.t;
  (* Only captures that are used in the rest of the pattern *)
  min_env : char list Bindings.t;
}

let empty_env = { full_env = Bindings.empty; min_env = Bindings.empty }

let construct_backref_set pat_val =
  match pat_val with
  | Nil -> Names.empty
  | Cons (atom, pat) -> (
      match atom with
      | Backref name -> Names.add name pat.backrefs
      | Any_symbol
      | Symbol _
      | Ellipsis ->
          pat.backrefs )

let create_pattern pat_val capture_name =
  let backrefs = construct_backref_set pat_val in
  { pat_val; capture_name; backrefs }

let has_backref k pat = Names.mem k pat.backrefs

(*
   To be called each time a new value is captured, i.e. bound to a
   metavariable.
*)
let add_capture pat v env =
  match pat.capture_name with
  | None -> env
  | Some k ->
      let min_env =
        if has_backref k pat then Bindings.add k v env.min_env else env.min_env
      in
      { full_env = Bindings.add k v env.full_env; min_env }

(*
   To be called each time we pass a backreference which may no longer
   be needed when descending down the pattern.
*)
let update_env_after_seeing_backref k env pat =
  if not (has_backref k pat) then
    { env with min_env = Bindings.remove k env.min_env }
  else env

(*
   Accumulator use to store the symbols matched by the current ellipsis.

   In_named_ellipsis (pat, acc) is the accumulator of symbols
   matching the ellipsis, in reverse order, for the ellipsis whose name
   is specified by the 'capture_name' field of the pattern 'pat'.
*)
type ellipsis =
  | Not_in_ellipsis
  | In_anon_ellipsis
  | In_named_ellipsis of pattern * char list

type stat = { mutable match_calls : int }

type cache_when = Always | Optimal

(*
   Create a pattern from an ocaml list.
*)
let rec pat_of_list xs =
  match xs with
  | [] -> create_pattern Nil None
  | (atom, opt_capture_name) :: xs ->
      let pat_val = Cons (atom, pat_of_list xs) in
      create_pattern pat_val opt_capture_name

let sample_pattern : pattern =
  pat_of_list
    [
      (Symbol 'A', None);
      (Any_symbol, Some "thing");
      (Ellipsis, None);
      (Backref "thing", None);
    ]

(*
   Unlike in semgrep syntax, we distinguish metavariable assignments
   from metavariable dereferencing.

   This function checks that variable assignments are unique and that
   backreferences refer to a valid variable name.

   This is a sanity check, not part of the matching algorithm.
*)
let rec check_pattern_ env (pat : pattern) =
  let orig_pat = pat in
  match pat.pat_val with
  | Nil -> (
      match pat.capture_name with
      | None -> ()
      | Some name -> failwith ("invalid capture of Nil node: " ^ name) )
  | Cons (atom, pat) ->
      let new_env =
        match orig_pat.capture_name with
        | None -> env
        | Some name ->
            if List.mem name env then
              failwith ("multiple atoms have the same name: " ^ name)
            else name :: env
      in
      ( match atom with
      | Any_symbol -> ()
      | Symbol _ -> ()
      | Ellipsis -> ()
      | Backref name ->
          if not (List.mem name env) then
            failwith ("backreference to invalid name: " ^ name) );
      check_pattern_ new_env pat

let check_pattern pat = check_pattern_ [] pat

(*
   Initialize the capture of symbols by an ellipsis.
   'init_acc' is a stack, holding symbols in reverse order.
*)
let init_ellipsis pat init_acc =
  match pat.capture_name with
  | None -> In_anon_ellipsis
  | Some _name -> In_named_ellipsis (pat, init_acc)

let extend_ellipsis opt_ellipsis symbol =
  match opt_ellipsis with
  | Not_in_ellipsis -> opt_ellipsis
  | In_anon_ellipsis -> opt_ellipsis
  | In_named_ellipsis (name, acc) -> In_named_ellipsis (name, symbol :: acc)

let close_ellipsis opt_ellipsis env =
  match opt_ellipsis with
  | Not_in_ellipsis -> env
  | In_anon_ellipsis -> env
  | In_named_ellipsis (pat, acc) -> add_capture pat (List.rev acc) env

(*
   Extend the environment by adding captured subsequences:
   - add the named ellipsis that ended just before the current match,
     if applicable
   - add the named atom that was just matched, if applicable
*)
let extend env opt_ellipsis pat captured_sequence =
  let env = close_ellipsis opt_ellipsis env in
  add_capture pat captured_sequence env

(*
   Turn a string into a list of chars.
*)
let parse s =
  let acc = ref [] in
  for i = String.length s - 1 downto 0 do
    acc := s.[i] :: !acc
  done;
  !acc

let unparse l =
  let buf = Buffer.create (List.length l) in
  List.iter (Buffer.add_char buf) l;
  Buffer.contents buf

let print_result oc opt_bindings =
  match opt_bindings with
  | None -> fprintf oc "not a match\n"
  | Some l ->
      fprintf oc "match: {\n";
      List.iter (fun (name, subseq) -> fprintf oc "  %s: %S\n" name subseq) l;
      fprintf oc "}\n"

(* to be appended to existing line *)
let print_ellipsis oc ellipsis =
  match ellipsis with
  | Not_in_ellipsis -> ()
  | In_anon_ellipsis -> fprintf oc " in-ellipsis"
  | In_named_ellipsis (pat, acc) ->
      let name =
        match pat.capture_name with
        | Some name -> name
        | _ -> assert false
      in
      fprintf oc " in-ellipsis:%s:%S" name (unparse (List.rev acc))

(* to be appended to existing line *)
let print_some_env oc some_env =
  fprintf oc " {";
  let is_first = ref true in
  Bindings.bindings some_env
  |> List.iter (fun (name, subseq) ->
         if !is_first then is_first := false else fprintf oc " ";
         fprintf oc "%s:%S" name (unparse subseq));
  fprintf oc "}"

(* to be appended to existing line *)
let print_pat_head oc pat =
  match pat.pat_val with
  | Nil -> fprintf oc " _:''"
  | Cons (atom, _tail) -> (
      ( match pat.capture_name with
      | None -> fprintf oc " _:"
      | Some name -> fprintf oc " %s:" name );
      match atom with
      | Any_symbol -> fprintf oc "_"
      | Symbol c -> fprintf oc "%C" c
      | Ellipsis -> fprintf oc "..."
      | Backref name -> fprintf oc "%s" name )

(* to be appended to existing line *)
let print_input_head oc input =
  match input with
  | [] -> fprintf oc " ''"
  | symbol :: _ -> fprintf oc " %C" symbol

let max_trace_lines = 100

(* print single line *)
let trace_match_call ~trace stat ellipsis env pat input =
  let match_calls = stat.match_calls + 1 in
  stat.match_calls <- match_calls;
  if trace then
    if match_calls <= max_trace_lines then
      printf "match%a%a%a%a%a\n" print_ellipsis ellipsis print_some_env
        env.full_env print_some_env env.min_env print_pat_head pat
        print_input_head input
    else if match_calls = max_trace_lines + 1 then
      printf "[exceeded max trace lines = %i]\n%!" max_trace_lines

module Cache_key = struct
  type t = ellipsis * env * pattern * input

  let phys_eq = ( == )

  let equal (ellipsis1, env1, pat1, input1) (ellipsis2, env2, pat2, input2) =
    pat_equal pat1 pat2 && phys_eq input1 input2 && ellipsis1 = ellipsis2
    && Bindings.equal ( = ) env1.min_env env2.min_env

  let hash_env env =
    Bindings.fold
      (fun k v h ->
        Hashtbl.hash_param 10 100 k + Hashtbl.hash_param 10 100 v + h)
      env.min_env 0

  (*
     We define a custom hash function because the default one doesn't work
     on maps (two equal maps may have different tree structures).

     OCaml's default 'Hashtbl.hash' is the same as 'Hashtbl.hash_param 10 100'.
     First parameter (10): maximum of number of meaningful nodes used
                           by the hashing function.
                           (int-like constants, floats, strings)
     Second parameter (100): maximum total number of nodes used by the hashing
                             function.
     See the documentation for the Hashtbl module for more info.
  *)
  let hash (ellipsis, env, pat, input) =
    Hashtbl.hash_param 10 100 ellipsis
    + Hashtbl.hash_param 10 100 pat
    + Hashtbl.hash_param 10 100 input
    + hash_env env
end

(*
   Memoization consists in caching the results of a function.
   This is the basis of "dynamic programming" algorithms.
   The arguments of the function and anything in the environment that
   determines its result must be used to form the key for a cache entry.

   Using the memoized version of the match function, we're guaranteed to
   not compute the same thing twice (except for cache key creation and
   lookup).

   To see a trace in both cached and uncached versions of the algorithm,
   run the tests with 'make bench' and compare the outputs of the tests
   "cacheable gap" and "cacheable gap cached".
*)
module Memoize = struct
  module Tbl = Hashtbl.Make (Cache_key)

  (*
     Setting cache_every to a value greater than 1, such as 3,
     limits the amount caching, making things faster overall by 1.5x.
  *)
  let should_use_cache cache_every input =
    if cache_every > 1 then Hashtbl.hash_param 5 10 input mod cache_every = 0
    else true

  let get tbl cache_every compute ellipsis env pat input =
    (* only use the cache on some inputs because it's expensive *)
    if should_use_cache cache_every input then
      let key = (ellipsis, env, pat, input) in
      match Tbl.find_opt tbl key with
      | None ->
          let res = compute ellipsis env pat input in
          Tbl.add tbl key res;
          res
      | Some res -> res
    else compute ellipsis env pat input

  let create ?(cache_when = Always) compute =
    (*
       Initial table size impacts performance.
    *)
    let tbl = Tbl.create 8192 in
    let cache_every =
      match cache_when with
      | Always -> 1
      | Optimal -> 3
    in
    fun ellipsis env pat input ->
      get tbl cache_every compute ellipsis env pat input
end

(*
   Main matching function.

   Checks if a pattern matches the entire input sequence. It operates
   by scanning the pattern and the input sequence in parallel. Each call
   to the match function is in charge of matching the head of the pattern
   against the head of the input, and calls itself to match
   the tails recursively.

   Returns the captured symbols or sequences of symbols for which a
   name was specified in the pattern.

   To visualize the steps of the algorithm, run the test program in verbose
   mode with 'make bench'.
*)
let match_input ?(trace = true) ?cache root_pat root_input :
    (string * string) list option * stat =
  let stat = { match_calls = 0 } in
  let get_from_cache = ref (fun _ellipsis _env _pat _input -> assert false) in

  let rec match_ (ellipsis : ellipsis) (env : env) pat input : env option =
    trace_match_call ~trace stat ellipsis env pat input;
    !get_from_cache ellipsis env pat input
  and uncached_match ellipsis env pat input =
    let orig_pat = pat in
    let orig_input = input in
    let in_ellipsis = ellipsis <> Not_in_ellipsis in
    match pat.pat_val with
    | Nil -> (
        match input with
        | [] ->
            let env = close_ellipsis ellipsis env in
            Some env
        | symbol :: input ->
            if in_ellipsis then
              let ellipsis = extend_ellipsis ellipsis symbol in
              match_ ellipsis env orig_pat input
            else None )
    | Cons (pat_atom, pat) -> (
        match input with
        | [] -> (
            (* end of input, only empty sequence can match *)
            match pat_atom with
            | Ellipsis ->
                let env = extend env ellipsis orig_pat [] in
                match_ (init_ellipsis orig_pat []) env pat input
            | Backref name -> (
                match Bindings.find_opt name env.min_env with
                | Some [] ->
                    let env = extend env ellipsis orig_pat [] in
                    match_ Not_in_ellipsis env pat input
                | _ -> None )
            | Any_symbol
            | Symbol _ ->
                None )
        | symbol :: input -> (
            let head_match =
              match pat_atom with
              | Any_symbol ->
                  let env = extend env ellipsis orig_pat [ symbol ] in
                  Some (Not_in_ellipsis, env, input)
              | Symbol x ->
                  if x = symbol then
                    let env = extend env ellipsis orig_pat [ symbol ] in
                    Some (Not_in_ellipsis, env, input)
                  else None
              | Ellipsis ->
                  (* start new ellipsis but don't consume first symbol *)
                  let env = close_ellipsis ellipsis env in
                  Some (init_ellipsis orig_pat [], env, orig_input)
              | Backref name -> (
                  match Bindings.find_opt name env.min_env with
                  | Some [ symbol0 ] when symbol0 = symbol ->
                      let env = extend env ellipsis orig_pat [ symbol ] in
                      Some (Not_in_ellipsis, env, input)
                  | _ -> None )
            in
            let matches_here =
              match head_match with
              | None -> None
              | Some (ellipsis, env, input) ->
                  (* match the rest of the pattern with the rest of the
                     input *)
                  match_ ellipsis env pat input
            in
            match matches_here with
            | Some _ as x -> x
            | None ->
                if in_ellipsis then
                  (* skip input symbol and retry *)
                  let ellipsis = extend_ellipsis ellipsis symbol in
                  match_ ellipsis env orig_pat input
                else None ) )
  in
  ( match cache with
  | Some cache_when ->
      get_from_cache := Memoize.create ~cache_when uncached_match
  | None -> get_from_cache := uncached_match );

  let opt_captures =
    match match_ Not_in_ellipsis empty_env root_pat root_input with
    | None -> None
    | Some env ->
        Some
          ( Bindings.bindings env.full_env
          |> List.map (fun (k, v) -> (k, unparse v)) )
  in
  (opt_captures, stat)

(********** Tests **********)

let print_time name f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  printf "%s: %.6f s\n%!" name (t2 -. t1);
  res

let check_match ?cache ?min_match_calls ?max_match_calls pat input_str
    expected_opt_bindings =
  let sort = Option.map (List.sort compare) in
  check_pattern pat;
  let input = parse input_str in
  let expected = sort expected_opt_bindings in
  let actual, match_calls =
    let res, stat =
      print_time "match function" (fun () -> match_input ?cache pat input)
    in
    printf "input length: %i\n" (String.length input_str);
    printf "number of calls to the match function: %i\n" stat.match_calls;
    (sort res, stat.match_calls)
  in
  print_result stdout actual;
  Alcotest.(check bool) "equal" true (expected = actual);
  ( match min_match_calls with
  | None -> ()
  | Some mini ->
      printf "min number of calls to the match function: %i\n" mini;
      Alcotest.(check bool)
        (sprintf "no more than %i match calls" mini)
        true (match_calls >= mini) );
  match max_match_calls with
  | None -> ()
  | Some maxi ->
      printf "max number of calls to the match function: %i\n" maxi;
      Alcotest.(check bool)
        (sprintf "no more than %i match calls" maxi)
        true (match_calls <= maxi)

let test_simple_symbol () =
  let pat = pat_of_list [ (Symbol 'A', Some "a") ] in
  let input = "A" in
  check_match pat input (Some [ ("a", "A") ])

let test_simple_ellipsis () =
  let pat = pat_of_list [ (Ellipsis, Some "x") ] in
  let input = "ABC" in
  check_match pat input (Some [ ("x", "ABC") ])

let test_any_symbol () =
  let pat = pat_of_list [ (Any_symbol, Some "a") ] in
  let input = "A" in
  check_match pat input (Some [ ("a", "A") ])

let test_floating_symbol () =
  let pat =
    pat_of_list
      [
        (Ellipsis, Some "head"); (Symbol 'A', Some "a"); (Ellipsis, Some "tail");
      ]
  in
  let input = "012345A6789" in
  check_match pat input
    (Some [ ("head", "012345"); ("a", "A"); ("tail", "6789") ])

let test_backref ?cache () =
  let pat =
    pat_of_list
      [
        (Ellipsis, None);
        (Any_symbol, Some "orig");
        (Backref "orig", Some "copy");
        (Ellipsis, None);
      ]
  in
  let input = "ABBC" in
  check_match ?cache pat input (Some [ ("orig", "B"); ("copy", "B") ])

(* Caching won't help due to the capturing ellipsis. *)
let test_named_gap ?cache () =
  let pat =
    pat_of_list
      [ (Symbol 'A', Some "a"); (Ellipsis, Some "gap"); (Symbol 'B', Some "b") ]
  in
  let input = "A12B" in
  check_match ?cache pat input (Some [ ("a", "A"); ("gap", "12"); ("b", "B") ])

let test_cacheable_gap ?cache ?min_match_calls ?max_match_calls () =
  let pat =
    pat_of_list
      [
        (Ellipsis, None);
        (Any_symbol, Some "a");
        (Ellipsis, None);
        (Symbol 'X', Some "x");
        (Ellipsis, None);
      ]
  in
  let input = "ABCDEFG" in
  check_match ?cache ?min_match_calls ?max_match_calls pat input None

let test_backref_backtrack ?cache () =
  let pat =
    pat_of_list
      [
        (Ellipsis, None);
        (Any_symbol, Some "x");
        (Ellipsis, None);
        (Backref "x", None);
        (Symbol 'C', None);
        (Ellipsis, None);
      ]
  in
  let input = "ABBCA" in
  check_match ?cache pat input (Some [ ("x", "B") ])

(*
   Deterministically generate a random-looking string.
*)
let pseudo_random_string len pick_from =
  let n = String.length pick_from in
  assert (n > 0);
  Random.init 0;
  String.init len (fun _i -> pick_from.[Random.int n])

(*
   This is equivalent in semgrep to searching for a pattern like
   '$A; ... foo;', in a file where the statement 'foo;' doesn't exist.

   With the naive match algorithm, the complexity is O(n^2) where n is the
   input length.
*)
let test_quadratic ?cache ?min_match_calls ?max_match_calls () =
  let pat =
    pat_of_list
      [
        (Ellipsis, None);
        (Any_symbol, None);
        (* matches everywhere *)
        (Ellipsis, None);
        (Symbol 'x', None);
        (* doesn't exist in the input *)
        (Ellipsis, None);
      ]
  in
  (*
     With uniform input (e.g. all As), caching performance tanks.
     This is why we use a pseudo-random string. This is also a more realistic
     usage scenario i.e. actual programs don't repeat statements many times.
  *)
  let input = pseudo_random_string 10_000 "AB" in
  check_match ?cache ?min_match_calls ?max_match_calls pat input None

let test_cubic ?cache () =
  let pat =
    pat_of_list
      [
        (Ellipsis, None);
        (Any_symbol, None);
        (* matches everywhere *)
        (Ellipsis, None);
        (Any_symbol, None);
        (* matches everywhere *)
        (Ellipsis, None);
        (Symbol 'x', None);
        (* doesn't exist in the input *)
        (Ellipsis, None);
      ]
  in
  let input = pseudo_random_string 1_000 "AB" in
  check_match ?cache pat input None

let test =
  ( "Matcher",
    [
      ("simple symbol", `Quick, test_simple_symbol);
      ("simple ellipsis", `Quick, test_simple_ellipsis);
      ("any symbol", `Quick, test_any_symbol);
      ("floating symbol", `Quick, test_floating_symbol);
      ("backref", `Quick, test_backref ?cache:None);
      ("backref cached", `Quick, test_backref ~cache:Always);
      ("named gap", `Quick, test_named_gap ?cache:None);
      ("named gap cached", `Quick, test_named_gap ~cache:Always);
      ( "cacheable gap",
        `Quick,
        test_cacheable_gap ?cache:None ~min_match_calls:44 ?max_match_calls:None
      );
      ( "cacheable gap cached",
        `Quick,
        test_cacheable_gap ~cache:Always ?min_match_calls:None
          ~max_match_calls:29 );
      ("backref backtrack", `Quick, test_backref_backtrack ?cache:None);
      ("backref backtrack cached", `Quick, test_backref_backtrack ~cache:Optimal);
      ( "quadratic",
        `Slow,
        test_quadratic ?cache:None ~min_match_calls:50_025_002
          ?max_match_calls:None );
      ( "quadratic cached",
        `Quick,
        test_quadratic ~cache:Optimal ?min_match_calls:None
          ~max_match_calls:54390 );
      ("cubic", `Slow, test_cubic ?cache:None);
      ("cubic cached", `Quick, test_cubic ~cache:Optimal);
    ] )
