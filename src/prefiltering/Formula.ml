open Common

let hash_fold_list = Base.hash_fold_list

type 'a t =
  | And of 'a t list  (** A conjunction of requirements. *)
  | Or of 'a t list  (** A disjunction of requirements. *)
  | Pred of 'a  (** A single requirement. *)
[@@deriving show, eq, ord, hash]

let rec map_opt f = function
  | And xs -> (
      match List_.filter_map (map_opt f) xs with
      | [] -> None
      | _ :: _ as xs -> Some (And xs))
  | Or xs ->
      let option_map f xs =
        List.fold_left
          (fun acc x ->
            let* ys = acc in
            let* y = f x in
            Some (y :: ys))
          (Some []) xs
      in
      let* xs = option_map (map_opt f) xs in
      Some (Or xs)
  | Pred x -> f x

let rec fold f formula acc =
  match formula with
  | And xs
  | Or xs ->
      List.fold_left (fun acc x -> fold f x acc) acc xs
  | Pred x -> f x acc

let rec iter f = function
  | And xs
  | Or xs ->
      List.iter (iter f) xs
  | Pred x -> f x

let rec for_all p = function
  | And xs -> List.for_all (for_all p) xs
  | Or xs -> List.exists (for_all p) xs
  | Pred x -> p x

let rec exists p = function
  | And xs -> List.exists (exists p) xs
  | Or xs -> List.for_all (exists p) xs
  | Pred x -> p x

let rec eval (p : 'a -> bool) (formula : 'a t) : bool =
  match formula with
  | And xs -> List.for_all (eval p) xs
  | Or xs ->
      (* An empty Or is treated as true for conservative prefiltering *)
      if List.is_empty xs then true else List.exists (eval p) xs
  | Pred x -> p x

let rec map (f : 'a -> 'b) (formula : 'a t) : 'b t =
  match formula with
  | And xs -> And (List_.map (map f) xs)
  | Or xs -> Or (List_.map (map f) xs)
  | Pred x -> Pred (f x)

let predicates (formula : 'a t) : 'a list =
  fold (fun pred acc -> pred :: acc) formula []
