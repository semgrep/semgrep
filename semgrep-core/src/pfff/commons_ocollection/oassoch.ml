open Common

open Oassoc

(* !!take care!!: this class does side effect, not a pure oassoc *)
class ['a,'b] oassoch xs =
  let h = Common.hash_of_list xs in
  object(o)
    inherit ['a,'b] oassoc

    val data = h

    method empty = {< data = Hashtbl.create 101 >}
    method add (k,v) = (Hashtbl.replace data k v; o) (* not add cos add make iter sux *)

    (* redefine replkey to be more efficient than default. With hash, don't need
       to delkey before add, replace do both action directly.
    *)
    method replkey (k,v) = (Hashtbl.replace data k v; o)
    method iter f = Hashtbl.iter (Common2.curry f) data
    method view = raise Todo

    method del (k,_v) = (Hashtbl.remove data k; o)
    method mem _e = raise Todo
    method null =
      (try (Hashtbl.iter (fun _k _v -> raise Common2.ReturnExn) data; false)
       with Common2.ReturnExn -> true)

    method assoc k =
      try
        Hashtbl.find data k
      with Not_found -> (Common2.log3 ("pb assoc with k = " ^ (Dumper.dump k)); raise Not_found)

    method delkey k = (Hashtbl.remove data k; o)

    method keys =
      List.map fst (o#tolist)

  end
