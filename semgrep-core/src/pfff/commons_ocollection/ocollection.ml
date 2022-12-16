(*****************************************************************************)
(* Collection *)
(*****************************************************************************)

(*---------------------------------------------------------------------------*)
type ('a, 'b) view = Empty | Cons of 'a * 'b

class virtual ['a] ocollection =
  object(o: 'o)
    inherit Objet.objet

    method virtual empty: 'o
    method virtual add: 'a -> 'o

    method virtual iter: ('a -> unit) -> unit
    method virtual view: ('a, 'o) view

    (* no need virtual, but better to redefine for efficiency *)
    method virtual del: 'a -> 'o   (* can do default with: view+iter *)
    method virtual mem: 'a -> bool (* can do default with: mem(tolist) *)
    method virtual null: bool      (* can do default with: lenght(tolist)= 0 *)


    method add2: 'a -> unit = fun a ->
      o#add a |> ignore;
      ()
    method del2: 'a -> unit = fun a ->
      o#del a |> ignore;
      ()
    method clear: unit =
      o#iter (fun e -> o#del2 e);




    method fold: 'b. ('b -> 'a -> 'b) -> 'b -> 'b = fun f a ->
      let a = ref a in
      o#iter (fun e -> a := f !a e);
      !a

    method tolist: 'a list =
      List.rev (o#fold (fun acc e -> e::acc) [])
    method fromlist: 'a list -> 'o =
      fun xs -> xs |> List.fold_left (fun o e -> o#add e) o#empty

    method length: int =
      (* oldsimple: o#tolist +> List.length *)
      (* opti: *)
      let count = ref 0 in
      o#iter (fun _e -> incr count);
      !count

    method exists: ('a -> bool) -> bool = fun f ->
      o#tolist |> List.exists f

    method filter: ('a -> bool) -> 'o = fun f ->
      (* iter and call add from empty, or del *)
      o#tolist |> List.filter f |> o#fromlist

    (* forall, fold, map *)

    method getone: 'a =
      match o#view with Cons (e,_tl) -> e  | Empty -> failwith "no head"
    method others: 'o =
      match o#view with Cons (_e,tl) -> tl | Empty -> failwith "no tail"

  end
