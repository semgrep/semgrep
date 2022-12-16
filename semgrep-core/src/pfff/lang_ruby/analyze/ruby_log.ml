
open Format

type pos = Lexing.position

type ctx =
  | Ctx_Empty
  | Ctx_Pos of pos * ctx
  | Ctx_Msg of string * ctx
  | Ctx_Merge of ctx * ctx

let empty = Ctx_Empty
let loc pos ctx = Ctx_Pos(pos,ctx)
let of_loc pos = loc pos Ctx_Empty
let of_tok _x = raise Common.Todo

let msg str ctx = Ctx_Msg(str,ctx)
let of_msg str = msg str Ctx_Empty
let merge c1 c2 = Ctx_Merge(c1,c2)

let rec exists msg pos = function
  | Ctx_Empty -> false
  | Ctx_Msg(msg',Ctx_Pos(pos',rest)) ->
      if msg = msg' && pos = pos'
      then true
      else exists msg pos rest
  | Ctx_Pos(_,rest)
  | Ctx_Msg(_,rest) -> exists msg pos rest
  | Ctx_Merge(c1,c2) ->
      exists msg pos c1 || exists msg pos c2

let rec append c1 c2 = match c1 with
  | Ctx_Empty -> c2
  | Ctx_Msg(msg,Ctx_Pos(pos,rest)) ->
      if exists msg pos c2
      then append rest c2
      else Ctx_Msg(msg,Ctx_Pos(pos,append rest c2))
  | Ctx_Pos(p,ctx) -> Ctx_Pos(p,append ctx c2)
  | Ctx_Msg(s,ctx) -> Ctx_Msg(s,append ctx c2)
  | Ctx_Merge _ -> Ctx_Merge(c1,c2)

let kfsprintf f fmt =
  let b = Buffer.create 127 in
  let ppf = Format.formatter_of_buffer b in
  Format.pp_set_margin ppf (*(Format.pp_get_margin std_formatter ())*) 80;
  Format.kfprintf
    (fun ppf ->
       Format.pp_print_flush ppf ();
       f (Buffer.contents b)
    ) ppf fmt

let in_ctx ctx ?pos fmt = match pos with
  | None -> kfsprintf (fun msg -> Ctx_Msg(msg,ctx)) fmt
  | Some p -> kfsprintf (fun msg -> Ctx_Msg(msg,Ctx_Pos(p,ctx))) fmt

let format_pos ppf pos =
  fprintf ppf "%s:%d "
    ((*Filename.basename*) pos.Lexing.pos_fname)
    pos.Lexing.pos_lnum

let lvl _i = (*conf.debug_level >= i*) true

let stderr_ppf = formatter_of_out_channel stderr

(*
let fake_ppf = make_formatter (fun s pos len -> ()) (fun () -> ())
*)

let dup_tbl = Hashtbl.create 12373 (* prime *)

let rec format_ctx ppf ctx =
  let rec work ppf = function
    | Ctx_Empty -> ()
    | Ctx_Pos(pos,Ctx_Empty) ->
        fprintf ppf "at %a" format_pos pos
    | Ctx_Msg(msg,Ctx_Pos(pos,Ctx_Empty)) ->
        fprintf ppf "@[<v 0>in %s@,at %a@]" msg format_pos pos
    | Ctx_Msg(msg,Ctx_Empty) ->
        fprintf ppf "in %s" msg
    | Ctx_Pos(pos,ctx) ->
        fprintf ppf "at %a@," format_pos pos; work ppf ctx
    | Ctx_Msg(msg,Ctx_Pos(pos,ctx)) ->
        fprintf ppf "in %s@,at %a@," msg format_pos pos; work ppf ctx
    | Ctx_Msg(msg,ctx) ->
        fprintf ppf "in %s@," msg; work ppf ctx
    | Ctx_Merge(ctx1,ctx2) ->
        fprintf ppf "@[<v 0>@[<v 0>in MERGING@,  %a@]@,@[<v 0>AND@,  %a@]@]"
          format_ctx ctx2 format_ctx ctx1
  in
  fprintf ppf "@[<v>%a@]" work ctx

let output_header cont code fmt =
  kfsprintf cont ("@[<v 2>[%s] " ^^ fmt ^^ "@]") code

let output_msg_ctx ppf msg ctx =
  fprintf ppf "@[<v 0>%s@,  @[<v>%a@]@]@\n@.%!" msg format_ctx ctx

let show b no_dup ctx code fmt =
  output_header
    (fun msg ->
       if b then begin
         let buf = Buffer.create 127 in
         let ppf = Format.formatter_of_buffer buf in
         let () = output_msg_ctx ppf msg ctx in
         if no_dup then
           let full_msg = Buffer.contents buf in
           if Hashtbl.mem dup_tbl full_msg
           then ()
           else begin
             Hashtbl.add dup_tbl full_msg ();
             pp_print_string stderr_ppf full_msg
           end
         else Buffer.output_buffer stderr buf
       end
    ) code fmt

let flush () = Format.pp_print_flush stderr_ppf ()

let () = at_exit flush

let fixme ?(ctx=Ctx_Empty) fmt =
  show true (*conf.no_dup_errors*)true ctx "FIXME" fmt

let debug ?pos fmt = match pos with
  | None -> show (lvl 10) false Ctx_Empty "DEBUG" fmt
  | Some p -> show (lvl 10) false (of_loc p) "DEBUG" fmt

let note ?(ctx=Ctx_Empty) fmt = show (lvl 10) false ctx "NOTE" fmt

let warn ?(ctx=Ctx_Empty) fmt = show (lvl 1) false ctx "WARNING" fmt

let show_raise ctx code fmt =
  let fail m =
    if not (*conf.error_raises_exc*)false
    then output_msg_ctx stderr_ppf m ctx;
    Format.pp_print_flush stderr_ppf ();
    failwith m
  in
  output_header fail code fmt

let err ?(ctx=Ctx_Empty) fmt =
  if (*conf.error_raises_exc *)false
  then show_raise ctx "ERROR" fmt
  else show true (*conf.no_dup_errors*)true ctx "ERROR" fmt

let fatal ctx fmt =
  (*conf.print_error_ctx <- true;*)
  show_raise ctx "FATAL" fmt
