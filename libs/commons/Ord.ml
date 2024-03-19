type t = Less | Equal | Greater

(* We use this to be able to factorize our code for binary search, by
   instantiating our code against different kinds of containers and
   element types.
   In particular, this is an improvement over functorization, because the
   type of Bigarray.Array1.t is actually triply-polymorphic. By making the
   container type itself unspecified, we are able to abstract over even
   multiply-polymorphic containers.
*)
type ('elt, 'container) binary_searchable = {
  length : 'container -> int;
  get : 'container -> int -> 'elt;
}

let create_binary_search (searchable : ('elt, 'container) binary_searchable) =
  let binary_search ~f arr =
    let arr_lo = 0 in
    let arr_hi = searchable.length arr in

    let rec aux lo hi =
      if Int.equal lo hi then Error lo
      else
        let mid = (lo + hi) / 2 in
        match f mid (searchable.get arr mid) with
        | Equal -> Ok (mid, searchable.get arr mid)
        | Less -> aux lo mid
        | Greater -> aux (mid + 1) hi
    in
    aux arr_lo arr_hi
  in
  binary_search

let arr_searchable = { length = Array.length; get = Array.get }

let bigarr1_searchable =
  { length = Bigarray.Array1.dim; get = Bigarray.Array1.get }

let binary_search_arr ~f x = create_binary_search arr_searchable ~f x
let binary_search_bigarr1 ~f x = create_binary_search bigarr1_searchable ~f x

let to_comparison f x y =
  let res = f x y in
  if res < 0 then Less else if res > 0 then Greater else Equal

let cmp target _i x = to_comparison Int.compare target x
let%test _ = binary_search_arr ~f:(cmp 1) [| 1; 2; 4; 5 |] = Ok (0, 1)
let%test _ = binary_search_arr ~f:(cmp 2) [| 1; 2; 4; 5 |] = Ok (1, 2)
let%test _ = binary_search_arr ~f:(cmp 5) [| 1; 2; 4; 5 |] = Ok (3, 5)

(* out of bounds or not in the array returns the position it should be inserted at *)
let%test _ = binary_search_arr ~f:(cmp 6) [| 1; 2; 4; 5 |] = Error 4
let%test _ = binary_search_arr ~f:(cmp 3) [| 1; 2; 4; 5 |] = Error 2
let%test _ = binary_search_arr ~f:(cmp 0) [| 1; 2; 4; 5 |] = Error 0
