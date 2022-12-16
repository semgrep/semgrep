(* Yoann Padioleau
 *
 * Copyright (C) 1998-2022 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
(*###########################################################################*)
(* Prelude *)
(*###########################################################################*)

(* if set, certain functions like unwind_protect will not
 * do a try and finalize and instead just call the function, which
 * helps in ocamldebug and also in getting better backtraces.
 * This is also useful to set in a js_of_ocaml (jsoo) context to
 * again get better backtraces.
*)
let debugger = ref false

(* You should set this to true when you run code compiled by js_of_ocaml
 * so some functions can change their implementation and rely
 * less on non-portable API like Unix which does not work well under
 * node or in the browser.
*)
let jsoo = ref false

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* The following functions should be in their respective sections but
 * because some functions in some sections use functions in other
 * sections, and because I don't want to take care of the order of
 * those sections (of those dependencies), I put the functions causing
 * dependency problem here. C is better than OCaml on this with the
 * ability to declare prototypes, enabling some form of forward
 * references.
*)

let spf = Printf.sprintf

(* A timeout exception with accompanying debug information:
   - a descriptive name
   - the time limit
     The mli interface makes this type private to help prevent unsafe uses of
     the exception.
*)
type timeout_info = {
  name: string;
  max_duration: float;
}

exception Timeout of timeout_info

exception ExceededMemoryLimit of string

exception UnixExit of int

(*****************************************************************************)
(* Equality *)
(*****************************************************************************)
let (=|=) : int    -> int    -> bool = (=)
let (=<=) : char   -> char   -> bool = (=)
let (=$=) : string -> string -> bool = (=)
let (=:=) : bool   -> bool   -> bool = (=)

let (=*=) = (=)

(*****************************************************************************)
(* Debugging/logging *)
(*****************************************************************************)

let pr s =
  print_string s;
  print_string "\n";
  flush stdout

let pr2 s =
  prerr_string s;
  prerr_string "\n";
  flush stderr

let pr_xxxxxxxxxxxxxxxxx () =
  pr "-----------------------------------------------------------------------"

let _already_printed = Hashtbl.create 101
let disable_pr2_once = ref false

let xxx_once f s =
  if !disable_pr2_once then pr2 s
  else
  if not (Hashtbl.mem _already_printed s)
  then begin
    Hashtbl.add _already_printed s true;
    f ("(ONCE) " ^ s);
  end

let pr2_once s = xxx_once pr2 s


(* start of dumper.ml *)

(* Dump an OCaml value into a printable string.
 * By Richard W.M. Jones (rich@annexia.org).
 * dumper.ml 1.2 2005/02/06 12:38:21 rich Exp
*)
open Obj

let rec dump2 r =
  if is_int r then
    string_of_int (magic r : int)
  else (				(* Block. *)
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (field r n :: acc) n
    in
    let rec is_list r =
      if is_int r then (
        if (magic r : int) = 0 then true (* [] *)
        else false
      ) else (
        let s = size r and t = tag r in
        if t = 0 && s = 2 then is_list (field r 1) (* h :: t *)
        else false
      )
    in
    let rec get_list r =
      if is_int r then []
      else let h = field r 0 and t = get_list (field r 1) in h :: t
    in
    let opaque name =
      (* XXX In future, print the address of value 'r'.  Not possible in
       * pure OCaml at the moment.
      *)
      "<" ^ name ^ ">"
    in

    let s = size r and t = tag r in

    (* From the tag, determine the type of block. *)
    if is_list r then ( (* List. *)
      let fields = get_list r in
      "[" ^ String.concat "; " (List.map dump2 fields) ^ "]"
    )
    else if t = 0 then (		(* Tuple, array, record. *)
      let fields = get_fields [] s in
      "(" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )

    (* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
     * clear if very large constructed values could have the same
     * tag. XXX *)
    else if t = lazy_tag then opaque "lazy"
    else if t = closure_tag then opaque "closure"
    else if t = object_tag then (	(* Object. *)
      let fields = get_fields [] s in
      let _clasz, id, slots =
        match fields with h::h'::t -> h, h', t | _ -> assert false in
      (* No information on decoding the class (first field).  So just print
       * out the ID and the slots.
      *)
      "Object #" ^ dump2 id ^
      " (" ^ String.concat ", " (List.map dump2 slots) ^ ")"
    )
    else if t = infix_tag then opaque "infix"
    else if t = forward_tag then opaque "forward"

    else if t < no_scan_tag then (	(* Constructed value. *)
      let fields = get_fields [] s in
      "Tag" ^ string_of_int t ^
      " (" ^ String.concat ", " (List.map dump2 fields) ^ ")"
    )
    else if t = string_tag then (
      "\"" ^ String.escaped (magic r : string) ^ "\""
    )
    else if t = double_tag then (
      string_of_float (magic r : float)
    )
    else if t = abstract_tag then opaque "abstract"
    else if t = custom_tag then opaque "custom"
    else failwith ("dump: impossible tag (" ^ string_of_int t ^ ")")
  )

let dump v = dump2 (repr v)

(* end of dumper.ml *)

(*
let (dump : 'a -> string) = fun x ->
  Dumper.dump x
*)

let pr2_gen x = pr2 (dump x)

(* to be used in pipe operations *)
let before_return f v =
  f v;
  v
(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

(* Report the time a function takes. *)
let with_time f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  (res, t2 -. t1)

let pr_time name f =
  let t1 = Unix.gettimeofday () in
  Fun.protect f
    ~finally:(fun () ->
      let t2 = Unix.gettimeofday () in
      pr (spf "%s: %.6f s" name (t2 -. t1))
    )

let pr2_time name f =
  let t1 = Unix.gettimeofday () in
  Fun.protect f
    ~finally:(fun () ->
      let t2 = Unix.gettimeofday () in
      pr2 (spf "%s: %.6f s" name (t2 -. t1))
    )

type prof = ProfAll | ProfNone | ProfSome of string list
let profile = ref ProfNone
let show_trace_profile = ref false

let check_profile category =
  match !profile with
  | ProfAll -> true
  | ProfNone -> false
  | ProfSome l -> List.mem category l

let _profile_table = ref (Hashtbl.create 100)

let adjust_profile_entry category difftime =
  let (xtime, xcount) =
    (try Hashtbl.find !_profile_table category
     with Not_found ->
       let xtime = ref 0.0 in
       let xcount = ref 0 in
       Hashtbl.add !_profile_table category (xtime, xcount);
       (xtime, xcount)
    ) in
  xtime := !xtime +. difftime;
  xcount := !xcount + 1;
  ()

(* subtle: don't forget to give all argumens to f, otherwise partial app
 * and will profile nothing.
 *
 * todo: try also detect when complexity augment each time, so can
 * detect the situation for a function gets worse and worse ?
*)
let profile_code category f =
  if not (check_profile category)
  then f ()
  else begin
    if !show_trace_profile then pr2 (spf "> %s" category);
    let t = Unix.gettimeofday () in
    let res, prefix =
      try Ok (f ()), ""
      with Timeout _ as exn ->
        let e = Exception.catch exn in
        Error e, "*"
    in
    let category = prefix ^ category in (* add a '*' to indicate timeout func *)
    let t' = Unix.gettimeofday () in

    if !show_trace_profile then pr2 (spf "< %s" category);

    adjust_profile_entry category (t' -. t);
    (match res with
     | Ok res -> res
     | Error e -> Exception.reraise e
    );
  end


let _is_in_exclusif = ref (None: string option)

let profile_code_exclusif category f =
  if not (check_profile category)
  then f ()
  else begin

    match !_is_in_exclusif with
    | Some s ->
        failwith (spf "profile_code_exclusif: %s but already in %s " category s);
    | None ->
        _is_in_exclusif := (Some category);
        Fun.protect
          (fun () ->
             profile_code category f
          )
          ~finally:(fun () ->
            _is_in_exclusif := None
          )

  end

let profile_code_inside_exclusif_ok _category _f =
  failwith "Todo"


let (with_open_stringbuf: (((string -> unit) * Buffer.t) -> unit) -> string) =
  fun f ->
  let buf = Buffer.create 1000 in
  let pr s = Buffer.add_string buf (s ^ "\n") in
  f (pr, buf);
  Buffer.contents buf


(* todo: also put  % ? also add % to see if coherent numbers *)
let profile_diagnostic () =
  if !profile = ProfNone then "" else
    let xs =
      Hashtbl.fold (fun k v acc -> (k,v)::acc) !_profile_table []
      |> List.sort (fun (_k1, (t1,_n1)) (_k2, (t2,_n2)) -> compare t2 t1)
    in
    with_open_stringbuf (fun (pr,_) ->
      pr "---------------------";
      pr "profiling result";
      pr "---------------------";
      xs |> List.iter (fun (k, (t,n)) ->
        pr (Printf.sprintf "%-40s : %10.3f sec %10d count" k !t !n)
      )
    )

let report_if_take_time timethreshold s f =
  let t = Unix.gettimeofday () in
  let res = f () in
  let t' = Unix.gettimeofday () in
  if (t' -. t  > float_of_int timethreshold)
  then pr2 (Printf.sprintf "Note: processing took %7.1fs: %s" (t' -. t) s);
  res

let profile_code2 category f =
  profile_code category (fun () ->
    if !profile = ProfAll
    then pr2 ("starting: " ^ category);
    let t = Unix.gettimeofday () in
    let res = f () in
    let t' = Unix.gettimeofday () in
    if !profile = ProfAll
    then pr2 (spf "ending: %s, %fs" category (t' -. t));
    res
  )

(*###########################################################################*)
(* List functions *)
(*###########################################################################*)

(*****************************************************************************)
(* List.map *)
(*****************************************************************************)

(*
   Custom list type used to store intermediate lists, while minimizing
   the number of allocated blocks.
*)
type 'a list5 =
  | Elt of 'a * 'a list5
  | Tuple of 'a * 'a * 'a * 'a * 'a * 'a list5
  | Empty

let rev5 l =
  let rec aux acc l =
    match l with
    | Tuple (e, d, c, b, a, l) ->
        (* common case *)
        aux (a :: b :: c :: d :: e :: acc) l
    | Elt (a, l) ->
        aux (a :: acc) l
    | Empty -> acc
  in
  aux [] l

let rec slow_map acc f l =
  match l with
  | [] -> rev5 acc
  | [a] -> rev5 (Elt (f a, acc))
  | [a; b] ->
      let a = f a in
      let b = f b in
      rev5 (Elt (b, Elt (a, acc)))
  | [a; b; c] ->
      let a = f a in
      let b = f b in
      let c = f c in
      rev5 (Elt (c, (Elt (b, (Elt (a, acc))))))
  | [a; b; c; d] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      rev5 (Elt (d, (Elt (c, (Elt (b, (Elt (a, acc))))))))
  | [a; b; c; d; e] ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      rev5 (Elt (e, (Elt (d, (Elt (c, (Elt (b, (Elt (a, acc))))))))))
  | a :: b :: c :: d :: e :: l ->
      let a = f a in
      let b = f b in
      let c = f c in
      let d = f d in
      let e = f e in
      slow_map (Tuple (e, d, c, b, a, acc)) f l

let rec fast_map rec_calls_remaining f l =
  if rec_calls_remaining <= 0 then
    slow_map Empty f l
  else
    match l with
    | [] -> []
    | [a] -> [f a]
    | [a; b] ->
        let a = f a in
        let b = f b in
        [a; b]
    | [a; b; c] ->
        let a = f a in
        let b = f b in
        let c = f c in
        [a; b; c]
    | [a; b; c; d] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        [a; b; c; d]
    | [a; b; c; d; e] ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        [a; b; c; d; e]
    | a :: b :: c :: d :: e :: l ->
        let a = f a in
        let b = f b in
        let c = f c in
        let d = f d in
        let e = f e in
        a :: b :: c :: d :: e :: fast_map (rec_calls_remaining - 1) f l

(*
   This implementation of List.map makes at most 1000 non-tailrec calls
   before switching to a slower tailrec implementation.

   Additionally, this implementation guarantees left-to-right evaluation.
*)
let map f l = fast_map 1000 f l


(*****************************************************************************)
(* Other list functions *)
(*****************************************************************************)


(* Tail-recursive to prevent stack overflows. *)
let flatten xss =
  xss
  |> List.fold_left (fun acc xs ->
    List.rev_append xs acc)
    []
  |> List.rev

let rec drop n xs =
  match (n,xs) with
  | (0,_) -> xs
  | (_,[]) -> failwith "drop: not enough"
  | (n,_x::xs) -> drop (n-1) xs

let take n xs =
  let rec next n xs acc =
    match (n,xs) with
    | (0,_) -> List.rev acc
    | (_,[]) -> failwith "Common.take: not enough"
    | (n,x::xs) -> next (n-1) xs (x::acc) in
  next n xs []

let enum x n =
  if not(x <= n)
  then failwith (Printf.sprintf "bad values in enum, expect %d <= %d" x n);
  let rec enum_aux acc x n =
    if x = n then n::acc else enum_aux (x::acc) (x+1) n
  in
  List.rev (enum_aux [] x n)

let exclude p xs =
  List.filter (fun x -> not (p x)) xs

let rec (span: ('a -> bool) -> 'a list -> 'a list * 'a list) =
  fun p -> function
    | []    -> ([], [])
    | x::xs ->
        if p x then
          let (l1, l2) = span p xs in
          (x::l1, l2)
        else ([], x::xs)

let rec take_safe n xs =
  match (n,xs) with
  | (0,_) -> []
  | (_,[]) -> []
  | (n,x::xs) -> x::take_safe (n-1) xs

let group_by f xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in

  xs |> List.iter (fun x ->
    let k = f x in
    Hashtbl.replace hkeys k true;
    Hashtbl.add h k x
  );
  Hashtbl.fold (fun k _ acc -> (k, Hashtbl.find_all h k)::acc) hkeys []

let group_by_multi fkeys xs =
  (* use Hashtbl.find_all property *)
  let h = Hashtbl.create 101 in

  (* could use Set *)
  let hkeys = Hashtbl.create 101 in

  xs |> List.iter (fun x ->
    let ks = fkeys x in
    ks |> List.iter (fun k ->
      Hashtbl.replace hkeys k true;
      Hashtbl.add h k x;
    )
  );
  Hashtbl.fold (fun k _ acc -> (k, Hashtbl.find_all h k)::acc) hkeys []


(* you should really use group_assoc_bykey_eff *)
let rec group_by_mapped_key fkey l =
  match l with
  | [] -> []
  | x::xs ->
      let k = fkey x in
      let (xs1,xs2) = List.partition (fun x' -> let k2 = fkey x' in k=*=k2) xs
      in
      (k, (x::xs1))::(group_by_mapped_key fkey xs2)

let rec zip xs ys =
  match (xs,ys) with
  | ([],[]) -> []
  | ([],_) -> failwith "zip: not same length"
  | (_,[]) -> failwith "zip: not same length"
  | (x::xs,y::ys) -> (x,y)::zip xs ys

let null xs =
  match xs with [] -> true | _ -> false

let index_list xs =
  if null xs then [] (* enum 0 (-1) generate an exception *)
  else zip xs (enum 0 ((List.length xs) -1))

let index_list_0 xs = index_list xs

let index_list_1 xs =
  xs |> index_list |> map (fun (x,i) -> x, i+1)

let sort_prof a b =
  profile_code "Common.sort_by_xxx" (fun () -> List.sort a b)

let sort_by_val_highfirst xs =
  sort_prof (fun (_k1,v1) (_k2,v2) -> compare v2 v1) xs
let sort_by_val_lowfirst xs =
  sort_prof (fun (_k1,v1) (_k2,v2) -> compare v1 v2) xs

let sort_by_key_highfirst xs =
  sort_prof (fun (k1,_v1) (k2,_v2) -> compare k2 k1) xs
let sort_by_key_lowfirst xs =
  sort_prof (fun (k1,_v1) (k2,_v2) -> compare k1 k2) xs

let push v l =
  l := v :: !l

(*###########################################################################*)
(* Exn *)
(*###########################################################################*)

let unwind_protect f cleanup =
  if !debugger then f ()
  else
    try f ()
    with exn ->
      let e = Exception.catch exn in
      cleanup e;
      Exception.reraise e

(* TODO: remove and use Fun.protect instead? but then it will not have the
 * !debugger goodies *)
let finalize f cleanup =
  (* Does this debugger mode changes the semantic of the program too much?
   * Is it ok in a debugging context to not call certain cleanup()
   * and let the exn bubble up?
   * TODO: maybe I should not use save_excursion/finalize so much? maybe
   *  -debugger can help to see code that I should refactor?
  *)
  if !debugger then begin
    let res = f () in
    cleanup ();
    res
  end
  else
    Fun.protect f ~finally:cleanup

let (lines: string -> string list) = fun s ->
  let rec lines_aux = function
    | [] -> []
    | [x] -> if x = "" then [] else [x]
    | x::xs ->
        x::lines_aux xs
  in
  Str.split_delim (Str.regexp "\n") s |> lines_aux

let save_excursion reference newv f =
  let old = !reference in
  reference := newv;
  finalize f (fun _ -> reference := old;)

let memoized ?(use_cache=true) h k f =
  if not use_cache
  then f ()
  else
    try Hashtbl.find h k
    with Not_found ->
      let v = f () in
      begin
        Hashtbl.add h k v;
        v
      end

exception Todo
exception Impossible

exception Multi_found (* to be consistent with Not_found *)

let exn_to_s exn =
  Printexc.to_string exn

(*###########################################################################*)
(* Basic features *)
(*###########################################################################*)

(*****************************************************************************)
(* Test *)
(*****************************************************************************)

(* See Testutil *)

(*****************************************************************************)
(* Persistence *)
(*****************************************************************************)

let get_value filename =
  let chan = open_in_bin filename in
  let x = input_value chan in (* <=> Marshal.from_channel  *)
  (close_in chan; x)

let write_value valu filename =
  let chan = open_out_bin filename in
  (output_value chan valu;  (* <=> Marshal.to_channel *)
   (* Marshal.to_channel chan valu [Marshal.Closures]; *)
   close_out chan)

(*****************************************************************************)
(* Composition/Control *)
(*****************************************************************************)

(*****************************************************************************)
(* Error managment *)
(*****************************************************************************)

(*****************************************************************************)
(* Arguments/options and command line (cocci and acomment) *)
(*****************************************************************************)

(*
 * todo? isn't unison or scott-mcpeak-lib-in-cil handles that kind of
 * stuff better ? That is the need to localize command line argument
 * while still being able to gathering them. Same for logging.
 * Similiar to the type prof = PALL | PNONE | PSOME of string list.
 * Same spirit of fine grain config in log4j ?
 *
 * todo? how mercurial/cvs/git manage command line options ? because they
 * all have a kind of DSL around arguments with some common options,
 * specific options, conventions, etc.
 *
 *
 * todo? generate the corresponding noxxx options ?
 * todo? generate list of options and show their value ?
 *
 * todo? make it possible to set this value via a config file ?
 *
 *
 *)

type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list

(* the format is a list of triples:
 *  (title of section * (optional) explanation of sections * options)
*)
type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list


(* ---------------------------------------------------------------------- *)

(* now I use argv as I like at the call sites to show that
 * this function internally use argv.
*)
let parse_options options usage_msg argv =
  let args = ref [] in
  (try
     Arg.parse_argv argv options (fun file -> args := file::!args) usage_msg;
     args := List.rev !args;
     !args
   with
   | Arg.Bad msg -> Printf.eprintf "%s" msg; exit 2
   | Arg.Help msg -> Printf.printf "%s" msg; exit 0
  )




let usage usage_msg options  =
  Arg.usage (Arg.align options) usage_msg


(* for coccinelle *)

(* If you don't want the -help and --help that are appended by Arg.align *)
let arg_align2 xs =
  Arg.align xs |> List.rev |> drop 2 |> List.rev


let short_usage usage_msg  ~short_opt =
  usage usage_msg short_opt

let long_usage  usage_msg  ~short_opt ~long_opt  =
  pr usage_msg;
  pr "";
  let all_options_with_title =
    (("main options", "", short_opt)::long_opt) in
  all_options_with_title |> List.iter
    (fun (title, explanations, xs) ->
       pr title;
       pr_xxxxxxxxxxxxxxxxx();
       if explanations <> ""
       then begin pr explanations; pr "" end;
       arg_align2 xs |> List.iter (fun (key,_action,s) ->
         pr ("  " ^ key ^ s)
       );
       pr "";
    );
  ()


(* copy paste of Arg.parse. Don't want the default -help msg *)
let arg_parse2 l msg short_usage_fun =
  let args = ref [] in
  let f = (fun file -> args := file::!args) in
  let l = Arg.align l in
  (try begin
     Arg.parse_argv Sys.argv l f msg;
     args := List.rev !args;
     !args
   end
   with
   | Arg.Bad msg -> (* eprintf "%s" msg; exit 2; *)
       let xs = lines msg in
       (* take only head, it's where the error msg is *)
       pr2 (List.hd xs);
       short_usage_fun();
       raise (UnixExit 2)
   | Arg.Help _msg -> (* printf "%s" msg; exit 0; *)
       raise Impossible  (* -help is specified in speclist *)
  )


(* ---------------------------------------------------------------------- *)

type flag_spec   = Arg.key * Arg.spec * Arg.doc
type action_spec = Arg.key * Arg.doc * action_func
and action_func = (string list -> unit)

type cmdline_actions = action_spec list
exception WrongNumberOfArguments

let options_of_actions action_ref actions =
  actions |> map (fun (key, doc, _func) ->
    (key, (Arg.Unit (fun () -> action_ref := key)), doc)
  )

let (action_list: cmdline_actions -> Arg.key list) = fun xs ->
  map (fun (a,_b,_c) -> a) xs

let (do_action: Arg.key -> string list (* args *) -> cmdline_actions -> unit) =
  fun key args xs ->
  let assoc = xs |> map (fun (a,_b,c) -> (a,c)) in
  let action_func = List.assoc key assoc in
  action_func args


(* todo? if have a function with default argument ? would like a
 *  mk_action_0_or_1_arg ?
*)

let mk_action_0_arg f =
  (function
    | [] -> f ()
    | _ -> raise WrongNumberOfArguments
  )

let mk_action_1_arg f =
  (function
    | [file] -> f file
    | _ -> raise WrongNumberOfArguments
  )

let mk_action_2_arg f =
  (function
    | [file1;file2] -> f file1 file2
    | _ -> raise WrongNumberOfArguments
  )

let mk_action_3_arg f =
  (function
    | [file1;file2;file3] -> f file1 file2 file3
    | _ -> raise WrongNumberOfArguments
  )

let mk_action_4_arg f =
  (function
    | [file1;file2;file3;file4] -> f file1 file2 file3 file4
    | _ -> raise WrongNumberOfArguments
  )

let mk_action_n_arg f = f

(*###########################################################################*)
(* Basic types *)
(*###########################################################################*)

(*****************************************************************************)
(* Bool *)
(*****************************************************************************)

(*****************************************************************************)
(* Char *)
(*****************************************************************************)

(*****************************************************************************)
(* Num *)
(*****************************************************************************)

(*****************************************************************************)
(* Tuples *)
(*****************************************************************************)

(*****************************************************************************)
(* Option/Either *)
(*****************************************************************************)

let (let*) = Option.bind

(* type 'a maybe  = Just of 'a | None *)
let (>>=) m1 m2 =
  match m1 with
  | None -> None
  | Some x -> m2 x

(*
 (*http://roscidus.com/blog/blog/2013/10/13/ocaml-tips/#handling-option-types*)
  let (|?) maybe default =
    match maybe with
    | Some v -> v
    | None -> Lazy.force default
*)
(* deprecated: use Option.map
   let map_opt f = function
   | None -> None
   | Some x -> Some (f x)
*)
(* deprecated: use Option.iter
   let do_option f = function
   | None -> ()
   | Some x -> f x
*)
(* deprecated: use Option.to_list
   let opt_to_list : 'a option -> 'a list = function
   | None -> []
   | Some x -> [x]
*)

(* often used in grammar actions in menhir *)
let optlist_to_list = function None -> [] | Some xs -> xs

(* not sure why but can't use let (?:) a b = ... then at use time ocaml yells*)
let (|||) a b =
  match a with
  | Some x -> x
  | None -> b

let (<|>) a b =
  match a with
  | Some _ -> a
  | _ -> b

type ('a,'b) either = Left of 'a | Right of 'b
[@@deriving eq, show]
(* with sexp *)
type ('a, 'b, 'c) either3 = Left3 of 'a | Middle3 of 'b | Right3 of 'c
[@@deriving eq, show]
(* with sexp *)

(* If you don't want to use [@@deriving eq, show] above, you
 * can copy-paste manually the generated code by getting the
 * result of ocamlfind ocamlc -dsource ... on this code
 *  type ('a, 'b) either =
 *  | Left of 'a
 *  | Right of 'b
 *  [@@deriving show]
 *
 * which should look like this:
 * let pp_either = fun poly_a -> fun poly_b -> fun fmt -> function
 *   | Left a0 ->
 *       (Format.fprintf fmt "(@[<2>Left@ ";
 *        (poly_a fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Right a0 ->
 *       (Format.fprintf fmt "(@[<2>Right@ ";
 *        (poly_b fmt) a0;
 *        Format.fprintf fmt "@])")
 *
 * let pp_either3 = fun poly_a -> fun poly_b -> fun poly_c -> fun fmt -> function
 *   | Left3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Left3@ ";
 *        (poly_a fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Middle3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Middle3@ ";
 *        (poly_b fmt) a0;
 *        Format.fprintf fmt "@])")
 *   | Right3 a0 ->
 *       (Format.fprintf fmt "(@[<2>Right3@ ";
 *        (poly_c fmt) a0;
 *        Format.fprintf fmt "@])")
*)

let partition_either f l =
  let rec part_either left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l ->
        (match f x with
         | Left  e -> part_either (e :: left) right l
         | Right e -> part_either left (e :: right) l) in
  part_either [] [] l

let partition_either3 f l =
  let rec part_either left middle right = function
    | [] -> (List.rev left, List.rev middle, List.rev right)
    | x :: l ->
        (match f x with
         | Left3  e -> part_either (e :: left) middle right l
         | Middle3  e -> part_either left (e :: middle) right l
         | Right3 e -> part_either left middle (e :: right) l) in
  part_either [] [] [] l

let partition_result f l =
  let rec aux left right = function
    | [] -> (List.rev left, List.rev right)
    | x :: l ->
        (match f x with
         | Ok x -> aux (x :: left) right l
         | Error x -> aux left (x :: right) l) in
  aux [] [] l

let rec filter_some = function
  | [] -> []
  | None :: l -> filter_some l
  | Some e :: l -> e :: filter_some l

(* Tail-recursive to prevent stack overflows. *)
let map_filter f xs =
  List.fold_left (fun acc x ->
    match f x with
    | None -> acc
    | Some y -> (y::acc))
    []
    xs
  |> List.rev


let rec find_some_opt p = function
  | [] -> None
  | x :: l ->
      match p x with
      |	Some v -> Some v
      |	None -> find_some_opt p l

let find_some p xs =
  match find_some_opt p xs with
  | None -> raise Not_found
  | Some x -> x

let find_opt f xs =
  find_some_opt (fun x -> if f x then Some x else None) xs


(*****************************************************************************)
(* Regexp, can also use PCRE *)
(*****************************************************************************)

let (matched: int -> string -> string) = fun i s ->
  Str.matched_group i s

let matched1 = fun s -> matched 1 s
let matched2 = fun s -> (matched 1 s, matched 2 s)
let matched3 = fun s -> (matched 1 s, matched 2 s, matched 3 s)
let matched4 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s)
let matched5 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s)
let matched6 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s)
let matched7 = fun s -> (matched 1 s, matched 2 s, matched 3 s, matched 4 s, matched 5 s, matched 6 s, matched 7 s)

let _memo_compiled_regexp = Hashtbl.create 101
let candidate_match_func s re =
  (* old: Str.string_match (Str.regexp re) s 0 *)
  let compile_re =
    memoized _memo_compiled_regexp re (fun () -> Str.regexp re)
  in
  Str.string_match compile_re s 0

let match_func s re =
  profile_code "Common.=~" (fun () -> candidate_match_func s re)

let (=~) s re =
  match_func s re

let split sep s = Str.split (Str.regexp sep) s

let join  sep xs = String.concat sep xs

(*****************************************************************************)
(* Strings *)
(*****************************************************************************)

(* ruby *)
let i_to_s = string_of_int
let s_to_i = int_of_string

let null_string s =
  s =$= ""

(*****************************************************************************)
(* Filenames *)
(*****************************************************************************)

(* TODO: we should use strong types like in Li Haoyi filename Scala library! *)
type filename = string (* TODO could check that exist :) type sux *)
[@@deriving show, eq]
(* with sexp *)
type dirname = string (* TODO could check that exist :) type sux *)
(* with sexp *)

(* file or dir *)
type path = string

let chop_dirsymbol = function
  | s when s =~ "\\(.*\\)/$" -> matched1 s
  | s -> s

(* pre: prj_path must not contain regexp symbol *)
let filename_without_leading_path prj_path s =
  let prj_path = chop_dirsymbol prj_path in
  if s =$= prj_path
  then "."
  else
    (* Note that we should handle multiple consecutive '/' as in 'path/to//file' *)
  if s =~ ("^" ^ prj_path ^ "/+\\(.*\\)$")
  then matched1 s
  else
    failwith
      (spf "cant find filename_without_project_path: %s  %s" prj_path s)

(* TODO: we should use strong types like in Li Haoyi filename Scala library! *)
let readable ~root s =
  match root with
  | "/" -> s
  | "." ->
      (match s with
       | s when s =~ "^/" ->
           failwith (spf "file %s shouldn't start with / when root is ." s)
       | s when s =~ "^\\./\\(.*\\)" -> matched1 s
       | _ -> s
      )
  | _ ->
      filename_without_leading_path root s

let is_directory file =
  (* old: (Unix.stat file).Unix.st_kind =*= Unix.S_DIR *)
  Sys.is_directory file

(*****************************************************************************)
(* Dates *)
(*****************************************************************************)

(*****************************************************************************)
(* Lines/words/strings *)
(*****************************************************************************)

(*****************************************************************************)
(* Process/Files *)
(*****************************************************************************)

let command2 s = ignore(Sys.command s)

exception CmdError of Unix.process_status * string

let process_output_to_list2 ?(verbose=false) command =
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    if verbose then pr2 e;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)

let cmd_to_list ?verbose command =
  let (l,exit_status) = process_output_to_list2 ?verbose command in
  match exit_status with
  | Unix.WEXITED 0 -> l
  | _ -> raise (CmdError (exit_status,
                          (spf "CMD = %s, RESULT = %s"
                             command (String.concat "\n" l))))

let cmd_to_list_and_status = process_output_to_list2

(*
   Input a line of text from a file in a way that works for Windows files
   or Unix files in any mode on any platform. Line terminators are eaten
   up.

   Known bug: CR characters preceding a CRLF sequence are eaten up anyway
   in text mode on Windows. Workaround: open the file in binary mode.

   Unix file on Unix:    "a\nb"   -> "a"
   Windows file on Unix: "a\r\nb" -> "a"  (* input_line returns "a\r" *)
   Unix file on Windows text mode:      "a\nb" -> "a"
   Unix file on Windows binary mode:    "a\nb" -> "a"
   Windows file on Windows text mode:   "a\r\nb" -> "a"
   Windows file on Windows binary mode: "a\r\nb" -> "a"

   What you need to know about Windows vs. Unix line endings
   =========================================================

   CR designates the byte '\r', LF designates '\n'.
   CRLF designates the sequence "\r\n".

   On Windows, there are two modes for opening a file for reading or writing:
   - binary mode, which is the same as on Unix.
   - text mode, which is special.

   A file opened in text mode on Windows causes the reads and the writes
   to translate line endings:
   - read from text file: CRLF -> LF
   - write to text file: LF -> CRLF

   To avoid these translations when running on Windows, the file must be
   opened in binary mode.

   Q: When must a file be opened in text mode?
   A: Never. Only stdin, stdout, and stderr connected to a console should
      be in text mode but the OS takes care of this for us.

   Q: Really never?
   A: Never for reading. For writing, maybe some Windows applications behave
      so badly that they can't recognize single LFs, in which case local logs
      and such might have to be opened in text mode. See next question.

   Q: Do all text processing applications on Windows support LF line endings
      in input files?
   A: Hopefully. They really should. The only way they don't support single LFs
      is if they open files in binary mode and then search for CRLF
      sequences as line terminators exclusively, which would be strange.

   Q: Do the parsers in pfff support all line endings?
   A: Yes unless there's a bug. All parsers for real programming languages
      should use the pattern '\r'?'\n' to match line endings.
*)
let input_text_line ic =
  let s = input_line ic in
  let len = String.length s in
  if len > 0 && s.[len-1] = '\r' then
    String.sub s 0 (len-1)
  else
    s

let cat file =
  let acc = ref [] in
  let chan = open_in_bin file in
  try
    while true do
      acc := input_text_line chan :: !acc
    done;
    assert false
  with End_of_file ->
    close_in chan;
    List.rev !acc

(*
   This implementation works even with Linux files like /dev/fd/63
   created by bash's process substitution e.g.

     my-ocaml-program <(echo contents)

   See https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   In bash, '<(echo contents)' is replaced by something like
   '/dev/fd/63' which is a special file of apparent size 0 (as
   reported by `Unix.stat`) but contains data (here,
   "contents\n"). So we can't use 'Unix.stat' or 'in_channel_length'
   to obtain the length of the file contents. Instead, we read the file
   chunk by chunk until there's nothing left to read.

   Why such a function is not provided by the ocaml standard library is
   unclear.
*)
let read_file2 ?(max_len = max_int) path =
  let buf_len = 4096 in
  let extbuf = Buffer.create 4096 in
  let buf = Bytes.create buf_len in
  let rec loop fd =
    match Unix.read fd buf 0 buf_len with
    | 0 -> Buffer.contents extbuf
    | num_bytes ->
        assert (num_bytes > 0);
        assert (num_bytes <= buf_len);
        Buffer.add_subbytes extbuf buf 0 num_bytes;
        if Buffer.length extbuf >= max_len then
          Buffer.sub extbuf 0 max_len
        else
          loop fd
  in
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> loop fd)

let read_file ?max_len a =
  profile_code "Common.read_file" (fun () -> read_file2 ?max_len a)

let write_file ~file s =
  let chan = open_out_bin file in
  (output_string chan s; close_out chan)

(* could be in control section too *)

let filemtime file =
  if !jsoo
  then failwith "JSOO: Common.filemtime"
  else (Unix.stat file).Unix.st_mtime



(*
Using an external C functions complicates the linking process of
programs using commons/. Thus, I replaced realpath() with an OCaml-only
similar functions fullpath().

external c_realpath: string -> string option = "caml_realpath"

let realpath2 path =
  match c_realpath path with
  | Some s -> s
  | None -> failwith (spf "problem with realpath on %s" path)

let realpath2 path =
  let stat = Unix.stat path in
  let dir, suffix =
    match stat.Unix.st_kind with
    | Unix.S_DIR -> path, ""
    | _ -> Filename.dirname path, Filename.basename path
  in

  let oldpwd = Sys.getcwd () in
  Sys.chdir dir;
  let realpath_dir = Sys.getcwd () in
  Sys.chdir oldpwd;
  Filename.concat realpath_dir suffix

let realpath path =
  profile_code "Common.realpath" (fun () -> realpath2 path)
*)

let fullpath file =
  if not (Sys.file_exists file)
  then failwith (spf "fullpath: file (or directory) %s does not exist" file);
  let dir, base =
    if Sys.is_directory file
    then file, None
    else Filename.dirname file, Some (Filename.basename file)
  in
  (* save *)
  let old = Sys.getcwd () in

  Sys.chdir dir;
  let here = Sys.getcwd () in

  (* restore *)
  Sys.chdir old;

  match base with
  | None -> here
  | Some x -> Filename.concat here x

(* Why a use_cache argument ? because sometimes want disable it but dont
 * want put the cache_computation funcall in comment, so just easier to
 * pass this extra option.
*)
let cache_computation2 ?(use_cache=true) file ext_cache f =
  if not use_cache
  then f ()
  else begin
    if not (Sys.file_exists file)
    then begin
      logger#error "WARNING: cache_computation: can't find %s" file;
      logger#error "defaulting to calling the function";
      f ()
    end else begin
      let file_cache = (file ^ ext_cache) in
      if Sys.file_exists file_cache &&
         filemtime file_cache >= filemtime file
      then begin
        logger#info "using cache: %s" file_cache;
        get_value file_cache
      end
      else begin
        let res = f () in
        write_value res file_cache;
        res
      end
    end
  end
let cache_computation ?use_cache a b c =
  profile_code "Common.cache_computation" (fun () ->
    cache_computation2 ?use_cache a b c)


(* emacs/lisp inspiration (eric cooper and yaron minsky use that too) *)
let (with_open_outfile: filename -> (((string -> unit) * out_channel) -> 'a) -> 'a) =
  fun file f ->
  let chan = open_out_bin file in
  let pr s = output_string chan s in
  unwind_protect (fun () ->
    let res = f (pr, chan) in
    close_out chan;
    res)
    (fun _e -> close_out chan)

let (with_open_infile: filename -> ((in_channel) -> 'a) -> 'a) = fun file f ->
  let chan = open_in_bin file in
  unwind_protect (fun () ->
    let res = f chan in
    close_in chan;
    res)
    (fun _e -> close_in chan)

let string_of_timeout_info { name; max_duration } =
  spf "%s:%g" name max_duration

let current_timer = ref None

(* it seems that the toplevel block such signals, even with this explicit
 *  command :(
 *  let _ = Unix.sigprocmask Unix.SIG_UNBLOCK [Sys.sigalrm]
*)

(* could be in Control section *)

(*
   This is tricky stuff.

   We have to make sure that timeout is not intercepted before here, so
   avoid exn handle such as try (...) with _ -> cos timeout will not bubble up
   enough. In such case, add a case before such as
   with Timeout -> raise Timeout | _ -> ...

  question: can we have a signal and so exn when in a exn handler ?
*)
let set_timeout ~name max_duration = fun f ->
  (match !current_timer with
   | None -> ()
   | Some { name = running_name; max_duration = running_val } ->
       invalid_arg (
         spf
           "Common.set_timeout: cannot set a timeout %S of %g seconds. \
            A timer for %S of %g seconds is still running."
           name max_duration
           running_name running_val
       )
  );
  let info (* private *) = { name; max_duration } in
  let raise_timeout () = raise (Timeout info) in
  let clear_timer () =
    current_timer := None;
    Unix.setitimer Unix.ITIMER_REAL
      { Unix.it_value = 0.; it_interval = 0. }
    |> ignore
  in
  let set_timer () =
    current_timer := Some info;
    Unix.setitimer Unix.ITIMER_REAL
      { Unix.it_value = max_duration; it_interval = 0. }
    |> ignore
  in
  try
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise_timeout ()));
    set_timer ();
    let x = f () in
    clear_timer ();
    Some x
  with
  | Timeout {name; max_duration} ->
      clear_timer ();
      logger#info "%S timeout at %g s (we abort)" name max_duration;
      None
  | exn ->
      let e = Exception.catch exn in
      (* It's important to disable the alarm before relaunching the exn,
         otherwise the alarm is still running.

         robust?: and if alarm launched after the log (...) ?
         Maybe signals are disabled when process an exception handler ?
      *)
      clear_timer ();
      logger#info "exn while in set_timeout";
      Exception.reraise e

let set_timeout_opt ~name time_limit f =
  match time_limit with
  | None -> Some (f ())
  | Some x -> set_timeout ~name x f

(* creation of tmp files, a la gcc *)

let _temp_files_created = Hashtbl.create 101

(* ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c" *)
let new_temp_file prefix suffix =
  let pid =
    if !jsoo
    then 42
    else Unix.getpid ()
  in
  let processid = i_to_s pid in
  let tmp_file = Filename.temp_file (prefix ^ "-" ^ processid ^ "-") suffix in
  Hashtbl.add _temp_files_created tmp_file ();
  tmp_file

let save_tmp_files = ref false
let erase_temp_files () =
  if not !save_tmp_files then begin
    _temp_files_created |> Hashtbl.iter (fun s () ->
      logger#info "erasing: %s" s;
      Sys.remove s;
    );
    Hashtbl.clear _temp_files_created;
  end

let erase_this_temp_file f =
  if not !save_tmp_files then begin
    Hashtbl.remove _temp_files_created f;
    logger#info "erasing: %s" f;
    Sys.remove f
  end

(*###########################################################################*)
(* Collection-like types other than List *)
(*###########################################################################*)

(*****************************************************************************)
(* Assoc *)
(*****************************************************************************)
type ('a, 'b) assoc = ('a * 'b) list

(*****************************************************************************)
(* Arrays *)
(*****************************************************************************)

(*****************************************************************************)
(* Matrix *)
(*****************************************************************************)

(*****************************************************************************)
(* Set. Have a look too at set*.mli  *)
(*****************************************************************************)

(*****************************************************************************)
(* Hash *)
(*****************************************************************************)

let hash_to_list h =
  Hashtbl.fold (fun k v acc -> (k,v)::acc) h []
  |> List.sort compare

let hash_of_list xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.replace h k v);
  h


(*****************************************************************************)
(* Hash sets *)
(*****************************************************************************)

type 'a hashset = ('a, bool) Hashtbl.t
(* with sexp *)

let hashset_to_list h =
  hash_to_list h |> map fst

(* old: slightly slower?
 * let hashset_of_list xs =
 *   xs +> map (fun x -> x, true) +> hash_of_list
*)
let hashset_of_list (xs: 'a list) : ('a, bool) Hashtbl.t =
  let h = Hashtbl.create (List.length xs) in
  xs |> List.iter (fun k -> Hashtbl.replace h k true);
  h

let hkeys h =
  let hkey = Hashtbl.create 101 in
  h |> Hashtbl.iter (fun k _v -> Hashtbl.replace hkey k true);
  hashset_to_list hkey

let group_assoc_bykey_eff2 xs =
  let h = Hashtbl.create 101 in
  xs |> List.iter (fun (k, v) -> Hashtbl.add h k v);
  let keys = hkeys h in
  keys |> map (fun k -> k, Hashtbl.find_all h k)

let group_assoc_bykey_eff xs =
  profile_code "Common.group_assoc_bykey_eff" (fun () ->
    group_assoc_bykey_eff2 xs)

(*****************************************************************************)
(* Stack *)
(*****************************************************************************)

type 'a stack = 'a list
(* with sexp *)

(*****************************************************************************)
(* Tree *)
(*****************************************************************************)

(*****************************************************************************)
(* Graph. Have a look too at Ograph_*.mli  *)
(*****************************************************************************)

(*****************************************************************************)
(* Generic op *)
(*****************************************************************************)
let sort xs = List.sort compare xs

(* maybe too slow? use an hash instead to first group, and then in
 * that group remove duplicates? *)
let rec uniq_by eq xs =
  match xs with
  | [] -> []
  | x::xs ->
      (match List.find_opt (fun y -> eq x y) xs with
       | Some _ -> uniq_by eq xs
       | None -> x :: uniq_by eq xs
      )
let uniq_by a b =
  profile_code "Common.uniq_by" (fun () -> uniq_by a b)

(*###########################################################################*)
(* Misc functions *)
(*###########################################################################*)

(*###########################################################################*)
(* Postlude *)
(*###########################################################################*)

(*****************************************************************************)
(* Flags and actions *)
(*****************************************************************************)

(*****************************************************************************)
(* Postlude *)
(*****************************************************************************)

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)



(* now in prelude: exception UnixExit of int *)
let exn_to_real_unixexit f =
  try f ()
  with UnixExit x -> exit x

let pp_do_in_zero_box f = Format.open_box 0; f (); Format.close_box ()


let main_boilerplate f =
  if not (!Sys.interactive) then
    exn_to_real_unixexit (fun () ->

      Sys.set_signal Sys.sigint (Sys.Signal_handle   (fun _ ->
        pr2 "C-c intercepted, will do some cleaning before exiting";
        (* But if do some try ... with e -> and if do not reraise the exn,
         * the bubble never goes at top and so I cant really C-c.
         *
         * A solution would be to not raise, but do the erase_temp_file in the
         * syshandler, here, and then exit.
         * The current solution is to not do some wild  try ... with e
         * by having in the exn handler a case: UnixExit x -> raise ... | e ->
        *)
        Sys.set_signal Sys.sigint Sys.Signal_default;
        raise (UnixExit (-1))
      ));

      (* The finalize() below makes it tedious to go back from exns when we use
       * 'back' in ocamldebug. Hence the special code in finalize() to
       * run differently when in "debugger mode". However the
       * Common.debugger global will be set in main(), so too late, so
       * we have to be quicker here and set it for the finalize() below.
      *)
      if Sys.argv |> Array.to_list |> List.exists
           (fun x -> x =$= "-debugger" || x =$= "--debugger")
      then debugger := true;

      finalize          (fun ()->
        pp_do_in_zero_box (fun () ->
          try
            f (); (* <---- here it is *)
          with Unix.Unix_error (e, fm, argm) ->
            pr2 (spf "exn Unix_error: %s %s %s\n"
                   (Unix.error_message e) fm argm);
            raise (Unix.Unix_error (e, fm, argm))
        ))
        (fun()->
           if !profile <> ProfNone
           then begin
             pr2 (profile_diagnostic ());
             Gc.print_stat stderr;
           end;
           erase_temp_files ();
        )
    )
(* let _ = if not !Sys.interactive then (main ()) *)

let follow_symlinks = ref false

let arg_symlink () =
  if !follow_symlinks
  then " -L "
  else ""

let grep_dash_v_str =
  "| grep -v /.hg/ |grep -v /CVS/ | grep -v /.git/ |grep -v /_darcs/" ^
  "| grep -v /.svn/ | grep -v .git_annot | grep -v .marshall"

let files_of_dir_or_files_no_vcs_nofilter xs =
  xs |> map (fun x ->
    if is_directory x
    then
      (* todo: should escape x *)
      let cmd = (spf "find %s '%s' -type f %s" (* -noleaf *)
                   (arg_symlink()) x grep_dash_v_str) in
      let (xs, status) =
        cmd_to_list_and_status cmd in
      (match status with
       | Unix.WEXITED 0 -> xs
       (* bugfix: 'find -type f' does not like empty directories, but it's ok *)
       | Unix.WEXITED 1 when Array.length (Sys.readdir x) = 0 -> []
       | _ -> raise (CmdError (status,
                               (spf "CMD = %s, RESULT = %s"
                                  cmd (String.concat "\n" xs))))
      )
    else [x]
  ) |> flatten


(*****************************************************************************)
(* Maps *)
(*****************************************************************************)

module SMap = Map.Make (String)
type 'a smap = 'a SMap.t
