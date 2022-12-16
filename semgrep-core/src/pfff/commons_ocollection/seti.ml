open Common2
open Common

(*****************************************************************************)
(* coded for LFS *)

(* todo: could take an incr/decr func in param, to make it generic
 * opti: remember the min/max (optimisation to have intersect biggest x -> x)
 * opti: avoid all those rev, and avoid the intervise
 * (but yes the algo are then more complex :)
 * opti: balanced set intervalle
*)

(*****************************************************************************)
type seti = elt list (* last elements is in first pos, ordered reverse *)
and elt = Exact of int | Interv of int * int

(* invariant= ordered list, no incoherent interv (one elem or zero elem),
 * merged (intervalle are separated) *)
let invariant xs =
  let aux min xs =
    xs |> List.fold_left (fun min e ->
      match e with
      | Exact i ->
          if i <= min then pr2 (spf "i = %d, min = %d" i min);
          (* todo: should be even stronger, shoud be i > min+1 *)
          assert (i > min);
          i
      | Interv (i,j) ->
          assert (i > min);
          assert (j > i);
          j
    ) min
  in
  ignore(aux min_int (List.rev xs));
  ()

let string_of_seti xs =
  "[" ^
  join "," (xs |> List.rev |> List.map (function
    | (Exact i) -> string_of_int i
    | (Interv (i,j)) -> Printf.sprintf "%d - %d" i j)) ^
  "]"

(*****************************************************************************)
let empty = []

let pack newi j = function
  | [] -> [Interv (newi,j)]
  | (Exact z)::xs ->
      (Interv (newi, j))::(if newi =|= z then xs else (Exact z)::xs)
  | (Interv (i', j'))::xs ->
      if newi =|= j'
      then (Interv (i', j))::xs  (* merge *)
      else (Interv (newi, j))::(Interv (i', j'))::xs


(* the only possible merges are when x = i-1, otherwise, the job is done before *)
let rec (add2: int -> seti -> seti) = fun x -> function
  | [] -> [Exact x]
  | (Exact i)::xs when x > i+1 -> (Exact x)::(Exact i)::xs
  | (Interv (i,j)::xs) when x > j+1 -> (Exact x)::(Interv (i,j))::xs
  | (Interv (i,j)::xs) when x =|= j+1 -> (Interv (i,x))::xs
  | (Exact i)::xs when x =|= i+1 -> (Interv (i,x))::xs

  | (Exact i)::xs when i =|= x   -> (Exact i)::xs
  | (Interv (i,j)::xs) when x <= j && x >= i -> (Interv (i,j))::xs
  | other ->
      (*         let _ = log "Cache miss" in *)
      let _ = Common2.count2 () in
      (match other with
       |       (Exact i)::xs when x =|= i-1 -> pack x i xs
       |       (Exact i)::xs when x < i-1 -> (Exact i)::add x xs

       |       (Interv (i,j)::xs) when x =|= i-1 -> pack x j xs
       |       (Interv (i,j)::xs) when x < i-1 -> (Interv (i,j))::add x xs
       |       _ -> raise Impossible
      )
and add x y = let _ = Common2.count5 () in add2 x y


let rec tolist2 = function
  | [] -> []
  | (Exact i)::xs -> i::tolist2 xs
  | (Interv (i,j))::xs -> Common2.enum i j @ tolist2 xs
let tolist xs = List.rev (tolist2 xs)

let fromlist = function xs -> List.fold_left (fun a e -> add e a) empty xs

let intervise = function
  | Exact x -> Interv (x,x)
  | y -> y
let exactize = function
  | Interv (i,j) when i =|= j -> Exact i
  | y -> y
let exactize2 x y = if x =|= y then Exact x else Interv (x,y)


let rec (remove: int -> seti -> seti) = fun x xs ->
  match xs with
  | [] -> [] (*  pb, not in  *)
  | (Exact z)::zs ->
      (match x <=> z with
       | Equal -> zs
       | Sup -> xs  (*  pb, not in *)
       | Inf -> (Exact z)::remove x zs
      )
  | (Interv (i,j)::zs) ->
      if x > j then xs (*  pb not in *)
      else
      if x >= i && x <= j then
        (
          let _ = assert (j > i) in (* otherwise can lead to construct seti such as [7,6] when removing 6 from [6,6] *)
          match () with
          | _ when x =|= i -> [exactize2 (i+1) j]
          | _ when x =|= j -> [exactize2 i (j-1)]
          | _ -> [exactize2 (x+1) j; exactize2 i (x-1)]
        ) @ zs
      else (Interv (i,j))::remove x zs

(* let _ = Example (remove 635 [Interv (3, 635)] = [Interv (3, 634)]) *)
(* let _ = Example (remove 2 [Interv (6, 7); Interv(1,4)] = [Interv (6,7); Interv (3,4); Exact 1]) *)
(* let _ = Example (remove 6 [Interv (6, 7); Interv(1,4)] = [Exact 7; Interv (1,4)]) *)
(* let _ = Example (remove 1 [Interv (6, 7); Interv(1,2)] = [Interv (6,7); Exact 2]) *)
(* let _ = Example (remove 3 [Interv (1, 7)] = [Interv (4,7); Interv (1,2)]) *)
let _ = assert_equal (remove 3 [Interv (1, 7)])  [Interv (4,7); Interv (1,2)]
let _ = assert_equal (remove 4 [Interv (3, 4)])  [Exact 3;]
(* let _ = example (try (ignore(remove 6 [Interv (6, 6)] = []); false) with _ -> true)   *)


let rec mem e = function
  | [] -> false
  | (Exact x)::xs ->
      (match e <=> x with
       | Equal -> true
       | Sup -> false
       | Inf -> mem e xs
      )
  | (Interv (i,j)::xs) ->
      if e > j then false
      else
      if e >= i && e <= j then true
      else mem e xs

let iter f xs = xs |> List.iter
                  (function
                    | Exact i -> f i
                    | Interv (i, j) -> for k = i to j do f k done
                  )

let is_empty xs = xs =*= []
let choose = function
  | [] -> failwith "not supposed to be called with empty set"
  | (Exact i)::_xs -> i
  | (Interv (i,_j))::_xs -> i

let elements xs = tolist xs
let rec cardinal = function
  | [] -> 0
  | (Exact _)::xs -> 1+cardinal xs
  | (Interv (i,j)::xs) -> (j-i) +1 + cardinal xs

(*****************************************************************************)
(*  TODO: could return corresponding osetb ? *)
let inter xs ys =
  let rec aux = fun xs ys ->
    match (xs, ys) with
    | (_, []) -> []
    | ([],_)  -> []
    | (x::xs, y::ys) ->
        (match (x, y) with
         | (Interv (i1, j1), Interv (i2, j2)) ->
             (match i1 <=> i2 with
              | Equal ->
                  (match j1 <=> j2 with
                   | Equal -> (Interv (i1,j1))::aux xs ys
                   (*  [  ] *)
                   (*  [  ] *)
                   | Inf -> (Interv (i1, j1))::aux xs      ((Interv (j1+1, j2))::ys)
                   (*  [  ] [      TODO? could have [ so cant englobe right now, but would be better *)
                   (*  [      ] *)
                   | Sup -> (Interv (i1, j2))::aux ((Interv (j2+1, j1))::xs) ys
                   (*  [    ] *)
                   (*  [ ] [       same *)
                  )
              | Inf ->
                  if j1 < i2 then aux xs (y::ys) (* need order ? *)
                  (*  [    ] *)
                  (*         [ ] *)
                  else
                    (match j1 <=> j2 with
                     | Equal -> (Interv (i2, j1))::aux xs ys
                     (*  [    ] *)
                     (*     [ ] *)
                     | Inf ->   (Interv (i2, j1))::aux xs ((Interv (j1+1, j2))::ys)
                     (*  [    ] [    same *)
                     (*     [     ]   *)
                     | Sup ->   (Interv (i2, j2))::aux ((Interv (j2+1, j1))::xs) ys
                     (*  [       ] *)
                     (*     [ ] [  same *)
                    )
              | Sup -> aux (y::ys) (x::xs) (* can cos commutative *)
             )
         | _ -> raise Impossible (* intervise *)
        )
  in
  (* TODO avoid the rev rev, but aux good ? need order ?  *)
  List.rev_map exactize (aux (List.rev_map intervise xs) (List.rev_map intervise ys))

let union xs ys =
  let rec aux = fun xs ys ->
    match (xs, ys) with
    | (vs, []) -> vs
    | ([],vs)  -> vs
    | (x::xs, y::ys) ->
        (match (x, y) with
         | (Interv (i1, j1), Interv (i2, j2)) ->
             (match i1 <=> i2 with
              | Equal ->
                  (match j1 <=> j2 with
                   | Equal -> (Interv (i1,j1))::aux xs ys
                   (*  [  ] *)
                   (*  [  ] *)
                   | Inf -> (Interv (i1, j1))::aux xs      ((Interv (j1+1, j2))::ys)
                   (*  [  ] [      TODO? could have [ so cant englobe right now, but would be better *)
                   (*  [      ] *)
                   | Sup -> (Interv (i1, j2))::aux ((Interv (j2+1, j1))::xs) ys
                   (*  [    ] *)
                   (*  [ ] [       same *)
                  )
              | Inf ->
                  if j1 < i2 then Interv (i1, j1):: aux xs (y::ys)
                  (*  [    ] *)
                  (*         [ ] *)
                  else
                    (match j1 <=> j2 with
                     | Equal -> (Interv (i1, j1))::aux xs ys
                     (*  [    ] *)
                     (*     [ ] *)
                     | Inf ->   (Interv (i1, j1))::aux xs ((Interv (j1+1, j2))::ys)
                     (*  [    ] [    same *)
                     (*     [     ]   *)
                     | Sup ->   (Interv (i1, j2))::aux ((Interv (j2+1, j1))::xs) ys
                     (*  [       ] *)
                     (*     [ ] [  same *)
                    )
              | Sup -> aux (y::ys) (x::xs) (* can cos commutative *)
             )
         | _ -> raise Impossible (* intervise *)
        )
  in
  (*     union_set (tolist xs) (tolist ys) +> fromlist *)
  List.rev_map exactize (aux (List.rev_map intervise xs) (List.rev_map intervise ys))

(* bug/feature:   discovered by vlad rusu, my invariant for intervalle is
 * not very strong, should return (Interv (1,4)) *)
(* let _ = Example (union [Interv (1, 4)] [Interv (1, 3)] = ([Exact 4; Interv (1,3)])) *)

let diff xs ys =
  let rec aux = fun xs ys ->
    match (xs, ys) with
    | (vs, []) -> vs
    | ([],_vs)  -> []
    | (x::xs, y::ys) ->
        (match (x, y) with
         | (Interv (i1, j1), Interv (i2, j2)) ->
             (match i1 <=> i2 with
              | Equal ->
                  (match j1 <=> j2 with
                   | Equal -> aux xs ys
                   (*  [  ] *)
                   (*  [  ] *)
                   | Inf -> aux xs      ((Interv (j1+1, j2))::ys)
                   (*  [  ]  *)
                   (*  [      ] *)
                   | Sup -> aux ((Interv (j2+1, j1))::xs) ys
                   (*  [    ] *)
                   (*  [ ]  *)
                  )
              | Inf ->
                  if j1 < i2 then Interv (i1, j1):: aux xs (y::ys)
                  (*  [    ] *)
                  (*         [ ] *)
                  else
                    (match j1 <=> j2 with
                     | Equal -> (Interv (i1, i2-1))::aux xs ys (* -1 cos exlude [ *)
                     (*  [    ] *)
                     (*     [ ] *)
                     | Inf ->   (Interv (i1, i2-1))::aux xs ((Interv (j1+1, j2))::ys)
                     (*  [    ]  *)
                     (*     [     ]   *)
                     | Sup ->   (Interv (i1, i2-1))::aux ((Interv (j2+1, j1))::xs) ys
                     (*  [       ] *)
                     (*     [ ]  *)
                    )
              | Sup ->
                  if j2 < i1 then aux (x::xs) ys
                  (*       [    ] *)
                  (*  [ ] *)
                  else
                    (match j1 <=> j2 with
                     | Equal -> aux xs ys
                     (*         [    ] *)
                     (*     [        ] *)
                     | Inf ->   aux xs ((Interv (j1+1, j2))::ys)
                     (*         [    ]  *)
                     (*     [           ]   *)
                     | Sup ->   aux ((Interv (j2+1, j1))::xs) ys
                     (*         [    ] *)
                     (*     [      ]  *)
                    )
             )
         | _ -> raise Impossible (* intervise *)
        )
  in
  (*       minus_set (tolist xs) (tolist ys) +> fromlist *)
  List.rev_map exactize (aux (List.rev_map intervise xs) (List.rev_map intervise ys))


(*     let _ = Example (diff [Interv (3,7)] [Interv (4,5)] = [Interv (6, 7); Exact 3]) *)

(*****************************************************************************)
let rec debug = function
  | [] -> ""
  | (Exact i)::xs -> (Printf.sprintf "Exact:%d;" i) ^  (debug xs)
  | (Interv (i,j)::xs) -> (Printf.sprintf "Interv:(%d,%d);" i j) ^ debug xs

(*****************************************************************************)
(* if operation return wrong result, then may later have to patch them *)
let patch1 xs = List.map exactize xs
let patch2 xs = xs |> List.map (fun e ->
  match e with
  | Interv (i,j) when i > j && i =|= j+1 ->
      let _ = pr2 (spf "i = %d, j = %d" i j) in
      Exact i
  | e -> e
)
let patch3 xs =
  let aux min xs =
    xs |> List.fold_left (fun (min,acc) e ->
      match e with
      | Exact i ->
          if i =|= min
          then (min, acc)
          else (i, (Exact i)::acc)
      | Interv (i,j) ->
          (j, (Interv (i,j)::acc))
    ) (min, [])
  in
  aux min_int (List.rev xs) |> snd
