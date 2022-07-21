(*
  val suggest : targets -> (rule * score) list
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
              |> List.filter (fun (succi, _) ->
                     match (cfg.graph#nodes#assoc succi).IL.n with
                     | NOther (Noop _) -> false
                     | _ -> true)
            with
            | [ (succi, _) ]
              when match cfg.graph#nodes#assoc succi with
                   | { n = NInstr _ } -> true
                   | _ -> false -> (
                match go seen succi with
                | seen, block :: blocks -> (seen, (i :: block) :: blocks)
                | _, [] -> raise Common.Impossible)
            | [ (succi, _) ] ->
                let seen, blocks = go seen succi in
                (seen, [ i ] :: blocks)
            | [] -> (seen, [])
            | succs ->
                Common.pr2
                  (Common.spf "at node %i, succs are %s" nodei
                     ([%show: int list] (List.map fst succs)));
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
  [| (* Temporal.b_must_follow_a *) |]

(***
   Entry
   ***)
let suggest files =
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
  Temporal.b_must_follow_a bbs
  |> List.sort (fun (_, s) (_, s') -> Float.compare s s')
  |> List.to_seq |> Seq.take 10 |> List.of_seq
  |> List.map (fun ((a, b), score) ->
         Common.spf "score: %f\t\tcall %s before %s" score (IL.show_lval a)
           (IL.show_lval b))
  |> List.iter Common.pr2
