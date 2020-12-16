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

type pattern_atom =
  | Any_symbol         (* match any single symbol *)
  | Symbol of char     (* match a specific symbol *)
  | Ellipsis           (* match any sequence of symbols *)
  | Backref of string  (* match the same thing that was previous matched
                          by the pattern atom of that name. *)

(*
   A pattern is a list of pattern atoms with an optional name.
   The presence of a name indicates that the symbol or sequence it matches
   must be captured, i.e. stored in the environment.
*)
type pattern = (pattern_atom * string option) list

type input = char list

(* Captured subsequences. *)
module Env = Map.Make (String)
type env = char list Env.t

(*
   Accumulator use to store the symbols matched by the current ellipsis.

   In_named_ellipsis ("foo", acc) is the accumulator of symbols
   matching the ellipsis, in reverse order, for the ellipsis named "foo".
*)
type ellipsis =
  | Not_in_ellipsis
  | In_anon_ellipsis
  | In_named_ellipsis of string * char list

type stat = {
  mutable match_calls: int;
}

type cache_when =
  | Always
  | Optimal

let sample_pattern : pattern = [
  Symbol 'A', None;
  Any_symbol, Some "thing";
  Ellipsis, None;
  Backref "thing", None;
]

(*
   Unlike in semgrep syntax, we distinguish metavariable assignments
   from metavariable dereferencing.

   This function checks that variable assignments are unique and that
   backreferences refer to a valid variable name.

   This is a sanity check, not part of the matching algorithm.
*)
let rec check_pattern_ env (pat : pattern) =
  match pat with
  | [] -> ()
  | (atom, opt_name) :: pat ->
      let new_env =
        match opt_name with
        | None -> env
        | Some name ->
            if List.mem name env then
              failwith ("multiple atoms have the same name: " ^ name)
            else
              name :: env
      in
      (match atom with
       | Any_symbol -> ()
       | Symbol _ -> ()
       | Ellipsis -> ()
       | Backref name ->
           if not (List.mem name env) then
             failwith ("backreference to invalid name: " ^ name)
      );
      check_pattern_ new_env pat

let check_pattern pat =
  check_pattern_ [] pat

(*
   Initialize the capture of symbols by an ellipsis.
   'init_acc' is a stack, holding symbols in reverse order.
*)
let init_ellipsis opt_name init_acc =
  match opt_name with
  | None -> In_anon_ellipsis
  | Some name -> In_named_ellipsis (name, init_acc)

let extend_ellipsis opt_ellipsis symbol =
  match opt_ellipsis with
  | Not_in_ellipsis -> opt_ellipsis
  | In_anon_ellipsis -> opt_ellipsis
  | In_named_ellipsis (name, acc) -> In_named_ellipsis (name, symbol :: acc)

let close_ellipsis opt_ellipsis env =
  match opt_ellipsis with
  | Not_in_ellipsis -> env
  | In_anon_ellipsis -> env
  | In_named_ellipsis (name, acc) -> Env.add name (List.rev acc) env

(*
   Extend the environment by adding captured subsequences:
   - add the named ellipsis that ended just before the current match,
     if applicable
   - add the named atom that was just matched, if applicable
*)
let extend env opt_ellipsis opt_name captured_sequence =
  let env = close_ellipsis opt_ellipsis env in
  match opt_name with
  | None -> env
  | Some name ->
      Env.add name captured_sequence env

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
  | None ->
      fprintf oc "not a match\n"
  | Some l ->
      fprintf oc "match: {\n";
      List.iter (fun (name, subseq) ->
        fprintf oc "  %s: %S\n"
          name subseq
      ) l;
      fprintf oc "}\n"

(* to be appended to existing line *)
let print_ellipsis oc ellipsis =
  match ellipsis with
  | Not_in_ellipsis -> ()
  | In_anon_ellipsis ->
      fprintf oc " in-ellipsis"
  | In_named_ellipsis (name, acc) ->
      fprintf oc " in-ellipsis:%s:%S" name (unparse (List.rev acc))

(* to be appended to existing line *)
let print_env oc env =
  fprintf oc " {";
  let is_first = ref true in
  Env.bindings env
  |> List.iter (fun (name, subseq) ->
    if !is_first then
      is_first := false
    else
      fprintf oc " ";
    fprintf oc "%s:%S"
      name
      (unparse subseq)
  );
  fprintf oc "}"

(* to be appended to existing line *)
let print_pat_head oc pat =
  match pat with
  | [] ->
      fprintf oc " _:''"
  | (atom, opt_name) :: _ ->
      (match opt_name with
       | None -> fprintf oc " _:"
       | Some name -> fprintf oc " %s:" name
      );
      match atom with
      | Any_symbol -> fprintf oc "_"
      | Symbol c -> fprintf oc "%C" c
      | Ellipsis -> fprintf oc "..."
      | Backref name -> fprintf oc "%s" name

(* to be appended to existing line *)
let print_input_head oc input =
  match input with
  | [] ->
      fprintf oc " ''"
  | symbol :: _ ->
      fprintf oc " %C" symbol

let max_trace_lines = 100

(* print single line *)
let trace_match_call ~trace stat ellipsis env pat input =
  let match_calls = stat.match_calls + 1 in
  stat.match_calls <- match_calls;
  if trace then
    if match_calls <= max_trace_lines then
      printf "match%a%a%a%a\n"
        print_ellipsis ellipsis
        print_env env
        print_pat_head pat
        print_input_head input
    else if match_calls = max_trace_lines + 1 then
      printf "[exceeded max trace lines = %i]\n%!" max_trace_lines

module Cache_key = struct
  type t = ellipsis * env * pattern * input

  let equal (ellipsis1, env1, pat1, input1) (ellipsis2, env2, pat2, input2) =
    (==) pat1 pat2
    && (==) input1 input2
    && ellipsis1 = ellipsis2
    && Env.equal (=) env1 env2

  let hash_env env =
    Env.fold
      (fun k v h ->
         Hashtbl.hash_param 10 100 k
         + Hashtbl.hash_param 10 100 v
         + h)
      env 0

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
    if cache_every > 1 then
      Hashtbl.hash_param 5 10 input mod cache_every = 0
    else
      true

  (* TODO: exclude from env the unused variables, i.e. those that are not
           backreferenced in the pattern.
  *)
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
    else
      compute ellipsis env pat input

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

   Checks if a pattern matches the entire input sequence.
   Returns the captured symbols or sequences of symbols for which a
   name was specified in the pattern.

   To visualize the steps of the algorithm, run the test program in verbose
   mode with 'make bench'.
*)
let match_input ?(trace = true) ?cache root_pat root_input
  : (string * string) list option * stat =

  let stat = { match_calls = 0 } in
  let get_from_cache = ref (fun _ellipsis _env _pat _input -> assert false) in

  let rec match_ (ellipsis : ellipsis) (env : env) pat input : env option =

    trace_match_call ~trace stat ellipsis env pat input;
    !get_from_cache ellipsis env pat input

  and uncached_match ellipsis env pat input =
    let orig_pat = pat in
    let orig_input = input in
    let in_ellipsis = ellipsis <> Not_in_ellipsis in
    match pat with
    | [] ->
        (match input with
         | [] ->
             let env = close_ellipsis ellipsis env in
             Some env
         | symbol :: input ->
             if in_ellipsis then
               let ellipsis = extend_ellipsis ellipsis symbol in
               match_ ellipsis env orig_pat input
             else
               None
        )
    | (pat_atom, opt_name) :: pat ->
        match input with
        | [] ->
            (* end of input, only empty sequence can match *)
            (match pat_atom with
             | Ellipsis ->
                 let env = extend env ellipsis opt_name [] in
                 match_ (init_ellipsis opt_name []) env pat input
             | Backref name ->
                 (match Env.find_opt name env with
                  | Some [] ->
                      let env = extend env ellipsis opt_name [] in
                      match_ Not_in_ellipsis env pat input
                  | _ ->
                      None
                 )
             | _ -> None
            )
        | symbol :: input ->
            let head_match =
              match pat_atom with
              | Any_symbol ->
                  let env = extend env ellipsis opt_name [symbol] in
                  Some (Not_in_ellipsis, env, input)
              | Symbol x ->
                  if x = symbol then
                    let env = extend env ellipsis opt_name [symbol] in
                    Some (Not_in_ellipsis, env, input)
                  else
                    None
              | Ellipsis ->
                  (* start new ellipsis but don't consume first symbol *)
                  let env = close_ellipsis ellipsis env in
                  Some (init_ellipsis opt_name [], env, orig_input)
              | Backref name ->
                  (match Env.find_opt name env with
                   | Some [symbol0] when symbol0 = symbol ->
                       let env = extend env ellipsis opt_name [symbol] in
                       Some (Not_in_ellipsis, env, input)
                   | _ ->
                       None
                  )
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
                else
                  None
  in
  (match cache with
   | Some cache_when ->
       get_from_cache := Memoize.create ~cache_when uncached_match
   | None ->
       get_from_cache := uncached_match
  );

  let opt_captures =
    match match_ Not_in_ellipsis Env.empty root_pat root_input with
    | None -> None
    | Some env ->
        Some (
          Env.bindings env
          |> List.map (fun (k, v) -> (k, unparse v))
        )
  in
  opt_captures, stat

(********** Tests **********)

let print_time name f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  printf "%s: %.6f s\n%!" name (t2 -. t1);
  res

let check_match ?cache pat input_str expected_opt_bindings =
  let sort = Option.map (List.sort compare) in
  check_pattern pat;
  let input = parse input_str in
  let expected = sort expected_opt_bindings in
  let actual =
    let res, stat =
      print_time "match function" (fun () ->
        match_input ?cache pat input
      )
    in
    printf "input length: %i\n" (String.length input_str);
    printf "number of calls to the match function: %i\n" stat.match_calls;
    sort res
  in
  print_result stdout actual;
  Alcotest.(check bool) "equal" true (expected = actual)

let test_simple_symbol () =
  let pat = [Symbol 'A', Some "a"] in
  let input = "A" in
  check_match pat input (Some ["a", "A"])

let test_simple_ellipsis () =
  let pat = [Ellipsis, Some "x"] in
  let input = "ABC" in
  check_match pat input (Some ["x", "ABC"])

let test_any_symbol () =
  let pat = [Any_symbol, Some "a"] in
  let input = "A" in
  check_match pat input (Some ["a", "A"])

let test_floating_symbol () =
  let pat = [
    Ellipsis, Some "head";
    Symbol 'A', Some "a";
    Ellipsis, Some "tail";
  ] in
  let input = "012345A6789" in
  check_match pat input (Some [
    "head", "012345";
    "a", "A";
    "tail", "6789";
  ])

let test_backref ?cache () =
  let pat = [
    Ellipsis, None;
    Any_symbol, Some "orig";
    Backref "orig", Some "copy";
    Ellipsis, None;
  ] in
  let input = "ABBC" in
  check_match ?cache pat input (Some [
    "orig", "B";
    "copy", "B";
  ])

(* Caching won't help due to the capturing ellipsis. *)
let test_named_gap ?cache () =
  let pat = [
    Symbol 'A', Some "a";
    Ellipsis, Some "gap";
    Symbol 'B', Some "b";
  ] in
  let input = "A12B" in
  check_match ?cache pat input (Some [
    "a", "A";
    "gap", "12";
    "b", "B";
  ])

(*
   TODO: cache only relevant environment so that earlier captures that
         aren't referenced in the pattern aren't part of the cache key.
*)
let test_cacheable_gap ?cache () =
  let pat = [
    Ellipsis, None;
    Any_symbol, None (* Some "a" *); (* TODO *)
    Ellipsis, None;
    Symbol 'X', Some "x";
    Ellipsis, None;
  ] in
  let input = "ABCDEFG" in
  check_match ?cache pat input None

let test_backref_backtrack ?cache () =
  let pat = [
    Ellipsis, None;
    Any_symbol, Some "x";
    Ellipsis, None;
    Backref "x", None;
    Symbol 'C', None;
    Ellipsis, None;
  ] in
  let input = "ABBCA" in
  check_match ?cache pat input (Some [
    "x", "B";
  ])

(*
   Deterministically generate a random-looking string.
*)
let pseudo_random_string len pick_from =
  let n = String.length pick_from in
  assert (n > 0);
  Random.init 0;
  String.init len (fun _i ->
    pick_from.[Random.int n]
  )

(*
   This is equivalent in semgrep to searching for a pattern like
   '$A; ... foo;', in a file where the statement 'foo;' doesn't exist.

   With the naive match algorithm, the complexity is O(n^2) where n is the
   input length.
*)
let test_quadratic ?cache () =
  let pat = [
    Ellipsis, None;
    Any_symbol, None; (* matches everywhere *)
    Ellipsis, None;
    Symbol 'x', None; (* doesn't exist in the input *)
    Ellipsis, None;
  ] in
  (*
     With uniform input (e.g. all As), caching performance tanks.
     This is why we use a pseudo-random string. This is also a more realistic
     usage scenario i.e. actual programs don't repeat statements many times.
  *)
  let input = pseudo_random_string 10_000 "AB" in
  check_match ?cache pat input None

let test_cubic ?cache () =
  let pat = [
    Ellipsis, None;
    Any_symbol, None; (* matches everywhere *)
    Ellipsis, None;
    Any_symbol, None; (* matches everywhere *)
    Ellipsis, None;
    Symbol 'x', None; (* doesn't exist in the input *)
    Ellipsis, None;
  ] in
  let input = pseudo_random_string 1_000 "AB" in
  check_match ?cache pat input None

let test = "Matcher", [
  "simple symbol", `Quick, test_simple_symbol;
  "simple ellipsis", `Quick, test_simple_ellipsis;
  "any symbol", `Quick, test_any_symbol;

  "floating symbol", `Quick, test_floating_symbol;

  "backref", `Quick, test_backref ?cache:None;
  "backref cached", `Quick, test_backref ~cache:Always;

  "named gap", `Quick, test_named_gap ?cache:None;
  "named gap cached", `Quick, test_named_gap ~cache:Always;

  "cacheable gap", `Quick, test_cacheable_gap ?cache:None;
  "cacheable gap cached", `Quick, test_cacheable_gap ~cache:Always;

  "backref backtrack", `Quick, test_backref_backtrack ?cache:None;
  "backref backtrack cached", `Quick, test_backref_backtrack ~cache:Optimal;

  "quadratic", `Slow, test_quadratic ?cache:None;
  "quadratic cached", `Quick, test_quadratic ~cache:Optimal;

  "cubic", `Slow, test_cubic ?cache:None;
  "cubic cached", `Quick, test_cubic ~cache:Optimal;
]
