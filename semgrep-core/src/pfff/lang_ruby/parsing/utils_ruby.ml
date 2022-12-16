module StrSet = Set.Make(String)

external id : 'a -> 'a = "%identity"

let default_opt def opt = match opt with
  | None -> def
  | Some x -> x

let rec last = function
  | [] -> raise (Invalid_argument "last")
  | [x] -> x
  | _x::tl -> last tl

let string_fold_left f acc s =
  let len = String.length s in
  if len < 1 then acc
  else
    let rec work idx acc =
      if idx < len then work (idx+1) (f acc s.[idx])
      else acc
    in work 0 acc

let do_opt ~none:none ~some:some opt = match opt with
  | None -> none
  | Some s -> some s

let map_preserve map f t =
  let changed = ref false in
  let t' =
    map (fun v ->
      let v' = f v in
      if v <> v'
      then changed := true;
      v'
    ) t
  in if !changed then t' else t

let map_opt_preserve f = function
  | None -> None
  | (Some x) as s ->
      let x' = f x in
      if x <> x' then Some x' else s

(* escapes specified character(s) by going over each character of the string *)
type esc_mode =
    NonEsc of string
  | Esc of string

let escape_chars haystack esc_chars =
  let bslash = String.make 1 (Char.chr 92) in (* back slash *)
  let rec fold_left f (a : esc_mode) (s : string) : esc_mode = match s with
    | "" -> a
    | s ->
        let acc = (f a (String.sub s 0 1)) in
        try
          fold_left f acc (String.sub s 1 ((String.length s) - 1))
        with Invalid_argument(_) -> acc
  in
  let result =
    fold_left
      (fun (mode : esc_mode) (c : string) -> match mode with
         | NonEsc(s) -> (* nonescaping mode *)
             if List.mem (String.get c 0) esc_chars then NonEsc(s ^ bslash ^ c)
             else if c = "\\" then Esc(s ^ c)
             else NonEsc(s ^ c)
         | Esc(s) -> (* escaping mode; only one char is allowed anyway *)
             NonEsc(s ^ c)
      ) (NonEsc("")) haystack
  in
  match result with
  | NonEsc(s) -> s
  | Esc(s) ->
      Printf.fprintf stderr
        "[WARN] string has inappropriate format (Esc) orig:%s res:%s\n"
        haystack s;
      haystack (* but supposed to be an error *)
