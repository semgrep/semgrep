

type 'a t = 'a list * 'a list

let empty = [], []

let is_empty (f,_r) = match f with [] -> true | _ -> false

(* smart construct to enforce that the first list is only empty
   when the entire queue is empty *)
let queue f r = match f with
  | [] -> (List.rev r), []
  | _ -> f,r

let enqueue x (f,r) = queue f (x::r)

let cons x (f,r) = x::f, r

let insert = cons

let hd = function
  | [],_ -> failwith "hd"
  | x::_,_ -> x

let tl = function
  | [],_ -> failwith "tl"
  | _x::tl,r -> queue tl r

let pop = function
  | [],_ -> failwith "pop"
  | x::tl,r -> x, (queue tl r)

let dequeue = pop

let pop_back = function
  | f, (r::rs) -> (f,rs), r
  | [], [] -> failwith "pop_back"
  | hd::[], [] -> empty, hd
  | f::fs, [] ->
      match List.rev fs with
      | [] -> assert false
      | r::rs -> (queue [f] rs), r

let length (f,r) = (List.length f) + (List.length r)

let append (f1,r1) (f2,r2) =
  let r = List.rev_append f2 r1 in
  let r = List.append r2 r in
  queue f1 r

let rev (f,r) = queue r f

let iter func (f,r) =
  List.iter func f;
  List.iter func (List.rev r)

let fold func acc (f,r) =
  List.fold_left func (List.fold_left func acc f) (List.rev r)

let rev_map func (f,r) =
  (List.rev_map func r), (List.rev_map func f)

let map func (f,r) =
  (List.rev (List.rev_map func f)),
  (List.rev (List.rev_map func r))

let to_list (f,r) =
  List.rev_append (List.rev f) (List.rev r)

let from_list l = (l,[])

let rec flatten t =
  if is_empty t then empty
  else
    let x,xs = pop t in
    append x (flatten xs)
(*
let flatten (f,r) =
  let f'= List.fold_left
    (fun acc (x,y) -> List.append x (List.rev_append y acc)) [] f
  in
  let r' = List.fold_left
    (fun acc (f,r) ->
      List.append acc (List.append r (List.rev f))
    ) [] r
  in
    f',r'
*)

let rec compare_list cmp x y = match x,y with
  | [],[] -> 0
  | _::_, [] ->  1
  | [], _::_ -> -1
  | hx::xs, hy::ys -> match cmp hx hy with
    | 0 -> compare_list cmp xs ys
    | c -> c

let cmp c ((f1,r1) as l1) ((f2,r2) as l2) = match r1,r2 with
  | [],[] -> compare_list c f1 f2
  | _ -> compare_list c (to_list l1) (to_list l2)

let compare q1 q2 = cmp Stdlib.compare q1 q2

let list_to_string to_s = function
  | [] -> "[]"
  | hd::tl ->
      Printf.sprintf "[%s%s]"
        (to_s hd)
        (List.fold_left (fun acc t -> acc ^ ", " ^ (to_s t)) "" tl)

let to_string to_s (f,r) =
  "Q" ^ (list_to_string to_s f) ^
  (list_to_string to_s (List.rev r))

let rec gen_list (gen : ?size:int -> Random.State.t -> 'a)
    ?(size=50) (r : Random.State.t) : 'a list =
  let size = abs size in
  if (Random.State.int r size) = 0
  then []
  else (gen r) :: (gen_list ~size:(size-1) gen r)

let gen (gena: ?size:int -> Random.State.t -> 'a) ?size rs : 'a t =
  (gen_list ?size gena rs), (gen_list ?size gena rs)

type 'a path =
  | Top
  | Path of 'a path * 'a

type 'a cursor = 'a path * 'a t

let to_cursor t = Top,t

let current (_,t) = hd t

let _hd_at c = current c
let _tl_at (_,t) = tl t

let at_right (_p,t) = is_empty t
let at_left = function Top,_ -> true | _ -> false

let move_right (p,t) = match t with
  | [],_ -> failwith "move_right"
  | (x::xs,r) -> Path(p,x), (queue xs r)

let move_left (p,t) = match p with
  | Top -> failwith "move_left"
  | Path(p, hd) -> p, (cons hd t)

let rec goto_front c =
  if at_left c then c
  else goto_front (move_left c)

let rec goto_back c =
  if at_right c then c
  else goto_back (move_right c)

let rec from_cursor = function
  | Top,t -> t
  | c -> from_cursor (move_left c)
