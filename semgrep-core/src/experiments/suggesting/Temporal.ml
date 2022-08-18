(*
  val a_must_b =
*)

let ( let* ) = Option.bind
let ( >>= ) = Common.( >>= )

let il_fn_exp_to_sid (fn : IL.exp) =
  (* TODO: could use sid here when avaliable so that we can handle renaming
     cases; unclear if we would get much better results.
  *)
  match fn with
  | { e = Fetch _; eorig = SameAs { e = N (Id (id, _name_info)); _ } }
  | {
      e = Fetch _;
      eorig =
        SameAs
          {
            e = N (IdQualified { name_last = id, _; name_info = _name_info; _ });
            _;
          };
    } ->
      Some id
  | _ -> None

module Templates = struct
  let ( >> ) x y = x >>= fun _ -> y
  let accept x = Some x

  let null = function
    | [] -> Some ()
    | _ -> None

  let not = function
    | Some _ -> None
    | _ -> Some ()

  let is_none = function
    | None -> Some ()
    | _ -> None

  let pair a b (x, y) = a x >> b y

  let call ~lval ~fn ~args = function
    | { IL.i = IL.Call (x, y, z); _ } ->
        lval x >>= fun l ->
        fn y >>= fun f ->
        args z >>= fun a -> Some (l, f, a)
    | _ -> None

  let cons x y = function
    | hd :: tl -> x hd >>= fun x -> y x tl
    | _ -> None

  let only_fetch = function
    | { IL.e = Fetch l; _ } -> Some l
    | _ -> None

  type ('a, 'b) matcher = 'a -> 'b option

  let trace_templates =
    (* NOTE: lvals are accepted even if we think they should be none, because
       we often save calls into a temp even if not needed.
    *)
    [|
      (* Type 1 Trace:
         $X = $FOO();
         $BAR(..., $X, ...);
         $BAZ(..., $X, ...);
      *)
      cons (call ~lval:Fun.id ~fn:only_fetch ~args:accept) (fun (x, foo, _) ->
          cons
            (call ~lval:accept ~fn:only_fetch
               ~args:
                 (List.find_opt (function
                   | { IL.e = Fetch l; _ } -> l = x
                   | _ -> false)))
            (fun (_, bar, _) ->
              cons
                (call ~lval:accept ~fn:only_fetch
                   ~args:
                     (List.find_opt (function
                       | { IL.e = Fetch l; _ } -> l = x
                       | _ -> false)))
                (fun (_, baz, _) _ -> accept [ foo; bar; baz ])));
      (* Type 2 Trace:
         $FOO(..., $X, ...);
         $BAR(..., $X, ...);
         $BAZ(..., $X, ...);
      *)
      cons (call ~lval:accept ~fn:only_fetch ~args:accept)
        (fun (_, foo, foo_args) ->
          cons (call ~lval:accept ~fn:only_fetch ~args:accept)
            (fun (_, bar, bar_args) ->
              cons
                (call ~lval:accept ~fn:only_fetch ~args:(fun baz_args ->
                     (* This asymtotically slow, but most functions don't have
                      * many arguments *)
                     List.find_opt
                       (fun foo_arg ->
                         List.exists (fun x -> x = foo_arg) bar_args
                         && List.exists (fun x -> x = foo_arg) baz_args)
                       foo_args))
                (fun (_, baz, _) _ -> accept [ foo; bar; baz ])));
      (* Type 3 Trace:
          $FOO();
          $BAR();
          $BAZ();
      *)
      cons (call ~lval:accept ~fn:only_fetch ~args:null) (fun (_, foo, _) ->
          cons (call ~lval:accept ~fn:only_fetch ~args:null) (fun (_, bar, _) ->
              cons (call ~lval:accept ~fn:only_fetch ~args:null)
                (fun (_, baz, _) _ -> accept [ foo; bar; baz ])));
    |]

  let rec map_suffixes f = function
    | [] -> []
    | _ :: ys as xs -> f xs :: map_suffixes f ys

  let extract_matching_traces templates traces =
    map_suffixes
      (fun suff -> Array.find_map (fun template -> template suff) templates)
      traces
    |> List.concat_map Option.to_list
end

module FnSet = Set.Make (struct
  type t = IL.lval

  let compare = compare
end)

let findi_opt y =
  let rec go i = function
    | [] -> None
    | x :: xs -> if x = y then Some i else go (i + 1) xs
  in
  go 0

type basic_block = IL.instr list
type trace = IL.exp list

(* Compute score for B must follow A. The score is determined as follows:
    Count the number of traces in which A is present. Let this value be \(n\).
    Count the number of those traces where B does not appear after A. Let
    this value be \(err\). Then the result is given by \(z(n, n - err)\).
    where
      \[ z(n, e) = (e/n - p_0)/\sqrt{p_0 * (1 - p_0)/n} \]
*)
let compute_z_score p0 traces (a, b) =
  let z n e = ((e /. n) -. p0) /. sqrt (p0 *. (1. -. p0) /. n) in
  let n, err =
    List.fold_right
      (fun trace (n, err) ->
        match (findi_opt a trace, findi_opt b trace) with
        | Some i, Some j -> if i < j then (n + 1, err) else (n + 1, err + 1)
        | Some _, None -> (n + 1, err)
        | _ -> (n, err))
      traces (0, 0)
  in
  z (float_of_int n) (float_of_int (n - err))

let get_all_call_traces bbs =
  bbs
  |> Common.map
       (List.filter_map (function
         | { IL.i = Call _; _ } as i -> Some i
         | _ -> None))
  |> (fun x ->
       [%show: IL.instr list list] x |> Common.pr2;
       x)
  |> List.concat_map
       (Templates.extract_matching_traces Templates.trace_templates)

let b_must_follow_a (basic_blocks : basic_block list) =
  let traces = get_all_call_traces basic_blocks in
  Common.pr2 (string_of_int (List.length traces));
  let functions = FnSet.of_list (List.flatten traces) in
  Common.pr2 (string_of_int (FnSet.cardinal functions));
  Seq.flat_map
    (fun x ->
      Seq.filter_map
        (* 0.9 used per section 5 of Bugs as Deviant Behaviours:
           See <https://web.stanford.edu/~engler/deviant-sosp-01.pdf> *)
          (fun y ->
          Common.(
            spf "checking the pair (%s, %s)" (IL.show_lval x) (IL.show_lval y)
            |> pr2);
          if x = y then None
          else Some ((x, y), compute_z_score 0.9 traces (x, y)))
        (FnSet.to_seq functions))
    (FnSet.to_seq functions)
  |> List.of_seq
