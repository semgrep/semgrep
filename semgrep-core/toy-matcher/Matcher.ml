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
let rec check_pattern env (pat : pattern) =
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
      check_pattern new_env pat

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

(*
   Match a pattern at the beginning of the input sequence.
*)
let rec match_ ~ellipsis env pat input =
  let orig_pat = pat in
  let in_ellipsis = ellipsis <> None in
  match pat with
  | [] ->
      if in_ellipsis || input = [] then
        Some env
      else
        None
  | (pat_atom, opt_name) :: pat ->
      match input with
      | [] ->
          (* end of input, only empty sequence can match *)
          (match pat_atom with
           | Ellipsis ->
               let env = extend env ellipsis opt_name [] in
               match_ ~ellipsis:(init_ellipsis opt_name []) env pat input
           | Backref name ->
               (match Env.find_opt name env with
                | Some [] ->
                    let env = extend env ellipsis opt_name [] in
                    match_ ~ellipsis:None env pat input
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
                match_ ~ellipsis env orig_pat input
              else
                None
          | Some (ellipsis, env) ->
              (* match the rest of the pattern with the rest of the input *)
              match_ ~ellipsis env pat input

(*
   Public interface for the recursive 'match_' function.
*)
let match_input pat input =
  match match_ ~ellipsis:None Env.empty pat input with
  | None -> None
  | Some env -> Some (Env.bindings env)

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
      List.iter (fun (name, symbols) ->
        printf "%s: %s\n"
          name (unparse symbols)
      ) l;
      print_endline "}"

let check_match pat input_str expected_opt_bindings =
  let sort = Option.map (List.sort compare) in
  let input = parse input_str in
  let expected = sort expected_opt_bindings in
  let actual = match_input pat input |> sort in
  Alcotest.(check bool) "equal" true (expected = actual)

let test_simple () =
  let pat = [Symbol 'A', None] in
  let input = "A" in
  check_match pat input (Some [])

let test = "Matcher", [
  "simple", `Quick, test_simple;
]
