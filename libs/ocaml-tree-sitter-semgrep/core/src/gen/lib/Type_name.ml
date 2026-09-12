(*
   Assign good names to productions in the grammar, based on the
   enclosing rule name and the contents of the production.
*)

open Printf
open CST_grammar

(*
   Hash a string with MD5 (it's overkill but standard and sufficiently fast
   for what we do with it) and keep only the beginning.
*)
let hash_string_hex s =
  let md5_hex =
    Digest.string s
    |> Digest.to_hex
  in
  assert (String.length md5_hex = 32);
  String.sub md5_hex 0 7

(* alphabetic letter? *)
let is_alpha c =
  match c with
  | 'A'..'Z'
  | 'a'..'z' -> true
  | _ -> false

let capital_prefix = "X_"

let force_capitalize s =
  let need_prefix =
    s = "" || not (is_alpha s.[0])
  in
  if need_prefix then
    capital_prefix ^ s
  else
    String.capitalize_ascii s

let name_of_token (token : token) =
  Punct.to_alphanum token.name

(*
   Currently, this doesn't encode anything, it just fails if it contains
   undesirable characters. We would need to encode '_' which is used
   as a prefix terminator in the chain of prefixes we're constructing.
*)
let encode_prec_name name =
  for i = 0 to String.length name - 1 do
    match name.[i] with
    | 'A'..'Z'
    | 'a'..'z'
    | '0'..'9' -> ()
    | c ->
        failwith
          (sprintf "Unsupported character %C in named precedence level %S"
             c name)
  done;
  name

let name_of_num_prec n =
  if n >= 0 then sprintf "p%d" n
  else sprintf "n%d" (abs n)

let name_of_prec_value (p : Tree_sitter_grammar.prec_value) =
  match p with
  | Num n ->
      name_of_num_prec n
  | Named name ->
      sprintf "x%s" (encode_prec_name name)

let name_pattern pat =
  match Pattern_name.infer pat with
  | Some name -> name
  | None -> hash_string_hex pat

(*
   Similar to name_rule_body below but operates on the original tree-sitter
   grammar type (grammar.json). This is used to generate rule names
   for patterns that don't have a name during the simplify-grammar pass.
   See the Missing_node module.
*)
let name_ts_rule_body (body : Tree_sitter_grammar.Rule_body.t) =
  let open Tree_sitter_grammar in
  let rec name_rule_body (body : Rule_body.t) =
    match body with
    | Symbol ident -> ident
    | Literal s -> Punct.to_alphanum s
    | Blank -> "blank"
    | Pattern { value; _ } -> "pat_" ^ name_pattern value
    | Repeat x -> "rep_" ^ name_rule_body x
    | Repeat1 x -> "rep1_" ^ name_rule_body x
    | Choice (x :: _) -> "choice_" ^ name_rule_body x
    | Choice [] -> "choice"
    | Seq xs ->
        List.map name_rule_body xs
        |> String.concat "_"
    | Prec (Default, p, x) ->
        sprintf "prec_%s_" (name_of_prec_value p) ^ name_rule_body x
    | Prec (Dynamic, p, x) ->
        sprintf "pdyn_%s_" (name_of_prec_value p) ^ name_rule_body x
    | Prec(Left, p, x) ->
        sprintf "pleft_%s_" (name_of_prec_value p) ^ name_rule_body x
    | Prec (Right, p, x) ->
        sprintf "pright_%s_" (name_of_prec_value p) ^ name_rule_body x
    | Alias alias -> name_rule_body alias.content
    | Field (field_name, _x) -> field_name
    | Immediate_token x -> "imm_tok_" ^ name_rule_body x
    | Token x -> "tok_" ^ name_rule_body x
    | Reserved reserved -> "reserved_" ^ name_rule_body reserved.content
  in
  let full_name = name_rule_body body in
  Abbrev.words full_name

(*
   Derive a name from the structure of a rule, one that's hopefully not
   ambiguous, not too long, and stable over time.
*)
let name_rule_body body =
  let rec name_rule_body body =
    match body with
    | Symbol ident -> ident
    | Token token -> name_of_token token
    | Blank -> "blank"
    | Repeat body -> "rep_" ^ name_rule_body body
    | Repeat1 body -> "rep1_" ^ name_rule_body body
    | Choice ((_name, body) :: _) -> "choice_" ^ name_rule_body body
    | Choice [] -> "choice"
    | Optional body -> "opt_" ^ name_rule_body body
    | Seq l ->
        List.map name_rule_body l
        |> String.concat "_"
    | Alias (ident, _) -> ident
  in
  let full_name = name_rule_body body in
  Abbrev.words full_name

(*
   Similar to name_rule_body but recursively descend into alternatives,
   producing a longer name. Which is then hashed and discarded.
*)
let hash_rule_body body =
  let buf = Buffer.create 100 in
  let rec aux = function
    | Symbol ident
    | Alias (ident, _) -> Buffer.add_string buf ident
    | Token token -> Buffer.add_string buf token.name
    | Blank -> Buffer.add_string buf "blank"
    | Repeat body ->
        Buffer.add_string buf "repeat(";
        aux body;
        Buffer.add_char buf ')'
    | Repeat1 body ->
        Buffer.add_string buf "repeat1(";
        aux body;
        Buffer.add_char buf ')'
    | Choice cases ->
        Buffer.add_string buf "choice(";
        List.iter (fun (name, _) -> bprintf buf "%s," name) cases;
        Buffer.add_char buf ')'
    | Optional body ->
        Buffer.add_string buf "optional(";
        aux body;
        Buffer.add_char buf ')'
    | Seq l ->
        Buffer.add_string buf "seq(";
        List.iter aux l;
        Buffer.add_char buf ')'
  in
  aux body;
  let name = Buffer.contents buf in
  hash_string_hex name

let assign_case_names ?rule_name:opt_rule_name cases =
  let initial_naming =
    List.mapi (fun pos rule_body ->
      let case_name = name_rule_body rule_body in
      let name = force_capitalize case_name in
      (name, (pos, rule_body))
    ) cases
  in
  let grouped = Util_list.group_by_key initial_naming in
  let disambiguated_naming =
    List.map (fun (name, group) ->
      match group with
      | [(pos, rule_body)] -> [(pos, (name, rule_body))]
      | [] -> assert false
      | rule_bodies ->
          List.map (fun (pos, rule_body) ->
            let hex_id = hash_rule_body rule_body in
            (pos, (name ^ "_" ^ hex_id, rule_body))
          ) rule_bodies
    ) grouped
    |> List.flatten
    |> List.sort (fun (a, _) (b, _) -> compare a b)
    |> List.map snd
  in
  let names = List.map fst disambiguated_naming in
  if not (Util_list.is_deduplicated names) then
    failwith (
      sprintf "\
Failed to assign unique names to choices for rule %s.
Names we came up with are:
%s"
        (match opt_rule_name with
         | None -> "<no name>"
         | Some name -> name)
        (List.map (fun name -> "  " ^ name) names |> String.concat "\n")
    );
  disambiguated_naming
