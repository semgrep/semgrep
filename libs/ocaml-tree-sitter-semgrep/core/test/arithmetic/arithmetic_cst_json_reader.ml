open Common
module J = Json_type

(* helpers *)
type 'a json_reader = J.json_type list -> 'a option * J.json_type list

let error s json =
  let str = Json_io.string_of_json json in
  pr2 str;
  failwith (spf "Wrong format: %s, got: %s" s str)
let error2 s xs =
  let str = Json_io.string_of_json (J.Array xs) in
  pr2 str;
  failwith (spf "Wrong format: %s, got: %s" s str)


let (parse_STRING: string -> string json_reader) = fun stype xs ->
  match xs with
  | (J.Object ["type", J.String s;
               "children", J.Array []])::xs when s = stype ->
      Some "", xs
  | _ -> None, xs


let rec (parse_REPEAT: 'a json_reader -> 'a list json_reader) = fun f xs ->
  match f xs with
  | Some e, xs ->
      (match parse_REPEAT f xs with
       | Some es, xs -> Some (e::es), xs
       | None, xs -> None, xs
      )
  | None, xs -> Some [], xs

let (parse_OPTIONAL: 'a json_reader -> 'a option json_reader) = fun f xs ->
  match f xs with
  | Some (e), xs -> Some (Some e), xs
  | None, xs -> Some (None), xs

let rec parse_CHOICE fs xs =
  match fs with
  | [] -> None, xs
  | f::fs ->
      let res, ys = f () in
      (match res with
       | Some x -> Some x, ys
       | None -> parse_CHOICE fs xs
      )

(* =~ parse_SEQ *)
let (>>=) res_parser1 parser2_closure =
  match res_parser1 with
  | None, xs -> None, xs
  | Some e, xs -> parser2_closure (e, xs)



(* start of specific parser *)
open Ast_arithmetic

let rec (parse_program: program json_reader) = fun xs ->
  match xs with
  | (J.Object ["type", J.String "program";
               "children", J.Array xs])::ys ->
      parse_REPEAT parse_intermediate1 xs >>= (fun (v1, xs) ->
        if xs = []
        then Some (v1), ys
        else error2 "parse_program: remaining json elements" xs
      )
  | _ -> None, xs

and (parse_intermediate1: intermediate1 json_reader) = fun xs ->
  parse_CHOICE [
    (fun () -> parse_assignment_statement xs >>= (fun (x, xs) ->
       Some (Intermediate_type1 x), xs));
    (fun () -> parse_expression_statement xs >>= (fun (x, xs) ->
       Some (Intermediate_type2 x), xs));
  ] xs

and (parse_assignment_statement: assignment_statement json_reader) = fun xs ->
  match xs with
  | (J.Object ["type", J.String "assignment_statement";
               "children", J.Array xs])::ys ->
      parse_variable xs >>= (fun (v1, xs) ->
        parse_STRING "=" xs >>= (fun (v2, xs) ->
          parse_expression xs >>= (fun (v3, xs) ->
            parse_STRING ";" xs >>= (fun (v4, xs) ->
              if xs = []
              then Some (v1, v2, v3, v4), ys
              else error2 "parse_assignment_statement: remaining json elements" xs
            ))))
  | _ -> None, xs

and (parse_expression_statement: expression_statement json_reader) = fun xs ->
  match xs with
  | (J.Object ["type", J.String "expression_statement";
               "children", J.Array xs])::ys ->
      parse_expression xs >>= (fun (v1, xs) ->
        parse_STRING ";" xs >>= (fun (v2, xs) ->
          if xs = []
          then Some (v1, v2), ys
          else error2 "parse_expression_statement: remaining json elements" xs
        ))
  | _ -> None, xs

and (parse_expression: expression json_reader) = fun xs ->
  match xs with
  | (J.Object ["type", J.String "expression";
               "children", J.Array xs])::ys ->

      let res, xs =
        parse_CHOICE [
          (fun () -> parse_variable xs >>= (fun (x, xs) ->
             Some (Intermediate_type3 x), xs
           ));
          (fun () -> parse_number xs >>= (fun (x, xs) ->
             Some (Intermediate_type4 x), xs
           ));
          (fun () ->
             parse_expression xs >>= (fun (v1, xs) ->
               parse_STRING "+" xs >>= (fun (v2, xs) ->
                 parse_expression xs >>= (fun (v3, xs) ->
                   Some (Intermediate_type5 (v1, v2, v3)), xs
                 ))));
          (fun () ->
             parse_expression xs >>= (fun (v1, xs) ->
               parse_STRING "-" xs >>= (fun (v2, xs) ->
                 parse_expression xs >>= (fun (v3, xs) ->
                   Some (Intermediate_type6 (v1, v2, v3)), xs
                 ))));
          (fun () ->
             parse_expression xs >>= (fun (v1, xs) ->
               parse_STRING "*" xs >>= (fun (v2, xs) ->
                 parse_expression xs >>= (fun (v3, xs) ->
                   Some (Intermediate_type7 (v1, v2, v3)), xs
                 ))));
          (fun () ->
             parse_expression xs >>= (fun (v1, xs) ->
               parse_STRING "/" xs >>= (fun (v2, xs) ->
                 parse_expression xs >>= (fun (v3, xs) ->
                   Some (Intermediate_type8 (v1, v2, v3)), xs
                 ))));
          (fun () ->
             parse_expression xs >>= (fun (v1, xs) ->
               parse_STRING "^" xs >>= (fun (v2, xs) ->
                 parse_expression xs >>= (fun (v3, xs) ->
                   Some (Intermediate_type9 (v1, v2, v3)), xs
                 ))));

        ] xs
      in
      if xs = []
      then res, ys
      else error2 "parse_expression: remaining json elements" xs
  | _ -> None, xs


and (parse_variable: variable json_reader) = fun xs ->
  parse_STRING "variable" xs

and (parse_number: number json_reader) = fun xs ->
  parse_STRING "number" xs

(* entry point *)
let parse file =
  let json = Json_io.load_json file in
  match parse_program [json] with
  | Some e, [] -> e
  | Some _, xs -> error2 "parse: remaining json elements" xs
  | None, xs -> error2 "parse: unrecognized" xs
