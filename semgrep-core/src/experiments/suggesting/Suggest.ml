(* Cooper Pierce (c) r2c 2022
 *
 * See (internal) https://docs.google.com/document/d/1MvHBD-88T09EjRqUEfSBlnrcy2IEbRVRB4VzqbzPzsA/edit?usp=sharing
 *  and the associated scope doc for more information on the approach/outcomes
 *)

module G = AST_generic
module V = Visitor_AST

module Util = struct
  let cfg_to_bb (cfg : _ CFG.t) =
    let rec go seen nodei =
      if List.mem nodei seen then (seen, [])
      else
        let seen = nodei :: seen in
        match (cfg.graph#nodes#assoc nodei).IL.n with
        | NInstr i -> (
            match
              CFG.successors cfg nodei
              (* Filter out non-join successors of an assign anon instruction.
                 This is done to remove the expression which is assigned, which
                 is prototypically a function, and therefore doesn't comprise
                 comuptation done at that point.
              *)
              |> (match i.i with
                 | AssignAnon _ ->
                     List.filter (fun (succi, _) ->
                         match (cfg.graph#nodes#assoc succi).IL.n with
                         | Join -> true
                         | _ -> false)
                 | _ -> Fun.id)
              (* Partition out catch operations so that we can treat
                 the insides of, e.g., Python `try` blocks as a single basic
                 block. Need to partition instead of filter, because when we
                 get to the actual final instruction in the try we want to
                 continue from the catch instruction. For context, the cfg for
                 a try / except looks like:

                 ```py
                 def foo():
                     a()
                     try:
                         b()
                         c()
                     except _:
                         d()
                     e()
                 ```

                 [a]-->[noop try]
                           |
                           '->[b]-->[c]--.
                                \        v
                                 '-->[noop catch] -> [todo stmt] -> [d]
                                             \                      /
                                              \-->[noop finally]<--/
                                                      \
                                                       \--->[e]
              *)
              |> List.partition (fun (succi, _) ->
                     match (cfg.graph#nodes#assoc succi).IL.n with
                     | NOther (Noop "catch") -> false
                     | _ -> true)
            with
            | [ (succi, _) ], _
              when match cfg.graph#nodes#assoc succi with
                   | { n = NInstr _ } -> true
                   | _ -> false -> (
                match go seen succi with
                | seen, block :: blocks -> (seen, (i :: block) :: blocks)
                | seen, [] -> (seen, [ [ i ] ]))
            | [ (succi, _) ], _
            | [], [ (succi, _) ] ->
                let seen, blocks = go seen succi in
                (seen, [ i ] :: blocks)
            | [], _ -> (seen, [])
            | succs, _ ->
                Common.pr2
                  (Common.spf "at node %s [%i], succs are %s"
                     (IL.show_node_kind (cfg.graph#nodes#assoc nodei).IL.n)
                     nodei
                     ([%show: IL.node_kind list]
                        (List.map
                           (fun succ ->
                             fst succ |> cfg.graph#nodes#assoc |> fun x ->
                             x.IL.n)
                           succs)));
                raise Common.Impossible)
        | _ ->
            List.fold_left
              (fun (seen, blocks) (succi, _) ->
                let seen, new_blocks = go seen succi in
                (seen, blocks @ new_blocks))
              (seen, []) (CFG.successors cfg nodei)
    in
    go [] cfg.entry |> snd
end

type score = float
type scored_rules = (Rule.rule * score) list

(***
   List of suggestion generation passes to do
    - ast_passes operate on the generic AST
    - cfg_passes operate on CFG/IL (function bodies)
   ***)

(* list of function bodies *)
let ast_passes : (G.program -> scored_rules) array = [||]

let local_cfg_passes : (IL.stmt list list -> scored_rules) array =
  [| 
  (* TODO: Temporal.b_must_follow_a --- once it emits scored rules directly *)
  |]

(***
   Entry
   ***)
let suggest files =
  (*
  We first collect all of the CFGs for each function, which we'll later extract
  the traces from.
  *)
  let cfgs = ref [] in
  List.iter
    (fun file ->
      let ast = Parse_target.parse_program file in
      let lang = List.hd (Lang.langs_of_filename file) in
      Naming_AST.resolve lang ast;
      let v =
        V.mk_visitor
          {
            V.default_visitor with
            V.kfunction_definition =
              (fun (_k, _) def ->
                let _, xs = AST_to_IL.function_definition lang def in
                let cfg = CFG_build.cfg_of_stmts xs in
                Common.push cfg cfgs);
          }
      in
      v (Pr ast))
    files;
  let bbs = List.concat_map Util.cfg_to_bb !cfgs in
  (* We perform trace extraction using the templates we have implemented in
     Temporal *)
  Temporal.b_must_follow_a bbs
  |> List.sort (fun (_, s) (_, s') -> Float.compare s s')
  |> List.to_seq
  (* and take the top 10 ranked pairs *)
  |> Seq.take 10
  |> List.of_seq
  |> List.map (fun ((a, b), score) ->
         Common.spf "score: %f\t\tcall %s before %s\n" score (IL.show_lval a)
           (IL.show_lval b))
  |> List.iter Common.pr
