open Env_interpreter_php

module Taint = struct
    let taint_mode = ref false
    let taint_expr _a _b _c _d _e = failwith "taint_expr: Todo"

    (* If one thing is tainted, then the whole thing is tainted. 
     * todo: if got a single element, can return a precise Vstring
     * instead of a Vabstr Tstring
     *)
    let fold_slist sl =
      List.fold_left (fun acc x ->
        match x with
        | Vtaint _ as x -> x
        | _ -> acc
      ) (Vabstr Tstring) sl

    let check_danger _a _b _c _d _e _f = ()

    let binary_concat _env _heap _v1 _v2 _path =
      Vabstr Tstring

    module GetTaint = struct
      exception Found of string list

      let rec list f l =
        match l with
        | [] -> ()
        | [x] -> f x
        | x :: rl -> f x; list f rl
            
      let rec value path ptrs x =
        match x with
        | Vany | Vnull -> ()
        | Vtaint s -> raise (Found [s])
        | Vabstr _ -> ()
        | Vbool _ | Vint _ -> ()
        | Vref s ->
            let n = ISet.choose s in
            if not (ISet.mem n path)
            then
              let path = ISet.union s path in
              (try
                  value path ptrs (IMap.find n ptrs);
                with Not_found -> ()
              )
            else ()
        | Vptr n when ISet.mem n path ->
            let path = ISet.add n path in
            value path ptrs (IMap.find n ptrs);
        | Vptr _ -> ()
        | Vfloat _ | Vstring _ -> ()
        | Vrecord m ->
            let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
            list (fun (_x, v) -> value path ptrs v) vl;
        | Vobject m ->
            let vl = SMap.fold (fun x y acc -> (x, y) :: acc) m [] in
            list (fun (_x, v) -> value path ptrs v) vl;
        | Varray vl ->
            list (value path ptrs) vl;
        | Vmap (v1, v2) ->
            value path ptrs v1;
            value path ptrs v2;
        | Vmethod _ -> ()
        | Vsum vl ->
            list (value path ptrs) vl
              
      let value heap v =
        try value ISet.empty heap.ptrs v; None with Found l -> Some l
    end


    let when_call_not_found heap vl =
      let c = ref None in
      List.iter (fun x -> 
        let x = GetTaint.value heap x in 
        if x <> None then c := x
      ) vl;
      (match !c with
      | _ when not !taint_mode -> Vany
      | None -> Vany
      | Some x -> Vtaint (List.hd x)
      )

end
