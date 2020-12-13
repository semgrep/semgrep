(*
   A matcher operating on sequences of symbols,
   using the same basic matching algorithm as semgrep-core.

   The goals are:

   - Clarify semgrep-core's matching algorithm by not having to deal with any
     of the real-world complexity.
   - Keep the semgrep-core implementation separate and easy to follow,
     without functors.
*)

open Printf

type pattern_atom =
  | Any_symbol         (* match any single symbol *)
  | Symbol of char     (* match a specific symbol *)
  | Ellipsis           (* match any sequence of symbols *)
  | Backref of string  (* match the same thing that was previous matched
                          by the pattern atom of that name. *)

(* A pattern is a list of pattern atoms with an optional name. *)
type pattern = (pattern_atom * string option) list

type input = char list

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

module Env = Map.Make (String)

(*
   Initialize the capture of symbols by an ellipsis.
   'init_acc' is a stack, holding symbols in reverse order.
*)
let init_ellipsis opt_name init_acc =
  match opt_name with
  | None -> Some None
  | Some name -> Some (Some (name, init_acc))

let extend_ellipsis opt_ellipsis symbol =
  match opt_ellipsis with
  | None -> opt_ellipsis
  | Some None -> opt_ellipsis
  | Some (Some (name, acc)) -> Some (Some (name, symbol :: acc))

let close_ellipsis opt_ellipsis env =
  match opt_ellipsis with
  | None -> env
  | Some None -> env
  | Some (Some (name, acc)) -> Env.add name (List.rev acc) env

(*
   Extend the environment by adding captured subsequences:
   - add the named ellipsis that ended just before the current match,
     if applicable
   - add the named atom that was just matched, if applicable
*)
let extend env opt_ellipsis opt_name captured_sequence =
  let env =
    match opt_ellipsis with
    | None -> env       (* we were not in an ellipsis *)
    | Some None -> env  (* we were in an anonymous ellipsis *)
    | Some (Some (ellipsis_name, acc)) ->
        Env.add ellipsis_name (List.rev acc) env
  in
  match opt_name with
  | None -> env
  | Some name ->
      Env.add name captured_sequence env

type stat = {
  mutable match_calls: int;
}

(*
   Check if a pattern matches the input sequence completely.
*)
let rec match_ stat ~ellipsis env pat input =
  stat.match_calls <- stat.match_calls + 1;
  let orig_pat = pat in
  let in_ellipsis = ellipsis <> None in
  match pat with
  | [] ->
      (match input with
       | [] ->
           let env = close_ellipsis ellipsis env in
           Some env
       | symbol :: input ->
           if in_ellipsis then
             let ellipsis = extend_ellipsis ellipsis symbol in
             match_ stat ~ellipsis env orig_pat input
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
               match_ stat ~ellipsis:(init_ellipsis opt_name []) env pat input
           | Backref name ->
               (match Env.find_opt name env with
                | Some [] ->
                    let env = extend env ellipsis opt_name [] in
                    match_ stat ~ellipsis:None env pat input
                | _ ->
                    None
               )
           | _ -> None
          )
      | symbol :: input ->
          let matches_here =
            match pat_atom with
            | Any_symbol ->
                let env = extend env ellipsis opt_name [symbol] in
                Some (None, env)
            | Symbol x ->
                if x = symbol then
                  let env = extend env ellipsis opt_name [symbol] in
                  Some (None, env)
                else
                  None
            | Ellipsis ->
                let env = close_ellipsis ellipsis env in
                Some (init_ellipsis opt_name [symbol], env)
            | Backref name ->
                (match Env.find_opt name env with
                 | Some [symbol0] when symbol0 = symbol ->
                     let env = extend env ellipsis opt_name [symbol] in
                     Some (None, env)
                 | _ ->
                     None
                )
          in
          match matches_here with
          | None ->
              if in_ellipsis then
                (* skip input symbol and retry *)
                let ellipsis = extend_ellipsis ellipsis symbol in
                match_ stat ~ellipsis env orig_pat input
              else
                None
          | Some (ellipsis, env) ->
              (* match the rest of the pattern with the rest of the input *)
              match_ stat ~ellipsis env pat input

(*
   Public interface for the recursive 'match_' function.
*)
let match_input pat input =
  let stat = { match_calls = 0 } in
  let opt_captures =
    match match_ stat ~ellipsis:None Env.empty pat input with
    | None -> None
    | Some env -> Some (Env.bindings env)
  in
  opt_captures, stat

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

let print_result opt_bindings =
  match opt_bindings with
  | None ->
      print_endline "not a match"
  | Some l ->
      print_endline "match: {";
      List.iter (fun (name, subseq) ->
        printf "  %s: %S\n"
          name subseq
      ) l;
      print_endline "}"

let print_time name f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  printf "%s: %.6f s\n%!" name (t2 -. t1);
  res

let check_match pat input_str expected_opt_bindings =
  let sort = Option.map (List.sort compare) in
  let normalize opt_bindings =
    Option.map (fun bindings ->
      List.map (fun (var, symbols) -> (var, unparse symbols)) bindings
    ) opt_bindings
    |> sort
  in
  check_pattern pat;
  let input = parse input_str in
  let expected = sort expected_opt_bindings in
  let actual =
    let res, stat =
      print_time "match" (fun () ->
        match_input pat input
      )
    in
    printf "input length: %i\n" (String.length input_str);
    printf "number of calls to the match function: %i\n" stat.match_calls;
    normalize res
  in
  print_result actual;
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

let test_backref () =
  let pat = [
    Ellipsis, None;
    Any_symbol, Some "orig";
    Backref "orig", Some "copy";
    Ellipsis, None;
  ] in
  let input = "ABBC" in
  check_match pat input (Some [
    "orig", "B";
    "copy", "B";
  ])

let test_gap () =
  let pat = [
    Symbol 'A', Some "a";
    Ellipsis, Some "gap";
    Symbol 'B', Some "b";
  ] in
  let input = "A12B" in
  check_match pat input (Some [
    "a", "A";
    "gap", "12";
    "b", "B";
  ])

let test_backref_backtrack () =
  let pat = [
    Ellipsis, None;
    Any_symbol, Some "x";
    Ellipsis, None;
    Backref "x", None;
    Symbol 'C', None;
    Ellipsis, None;
  ] in
  let input = "ABBCA" in
  check_match pat input (Some [
    "x", "B";
  ])

(*
   This is equivalent in semgrep to searching for a pattern like
   '$A; ... foo;', in a file where the statement 'foo;' doesn't exist.

   With the naive match algorithm, the complexity is O(n^2) where n is the
   input length.
*)
let test_quadratic () =
  let pat = [
    Ellipsis, None;
    Any_symbol, None; (* matches everywhere *)
    Ellipsis, None;
    Symbol 'x', None; (* doesn't exist in the input *)
    Ellipsis, None;
  ] in
  let input = String.make 10_000 'A' in
  check_match pat input None

let test = "Matcher", [
  "simple symbol", `Quick, test_simple_symbol;
  "simple ellipsis", `Quick, test_simple_ellipsis;
  "any symbol", `Quick, test_any_symbol;
  "floating symbol", `Quick, test_floating_symbol;
  "backref", `Quick, test_backref;
  "gap", `Quick, test_gap;
  "backref backtrack", `Quick, test_backref_backtrack;
  "quadratic", `Quick, test_quadratic;
]
