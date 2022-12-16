
type pos = Lexing.position

type ctx

val empty : ctx
val of_loc : pos -> ctx
val of_tok : Parse_info.t -> ctx

val loc : pos -> ctx -> ctx
val of_msg : string -> ctx
val msg : string -> ctx -> ctx
val merge : ctx -> ctx -> ctx
val append : ctx -> ctx -> ctx

val flush : unit -> unit

val format_ctx : Format.formatter -> ctx -> unit
val format_pos : Format.formatter -> pos -> unit

val in_ctx : ctx -> ?pos:Lexing.position
  -> ('a, Format.formatter, unit, ctx) format4 -> 'a

val debug : ?pos:Lexing.position -> ('a, Format.formatter, unit, unit) format4
  -> 'a

val note : ?ctx:ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a
val warn : ?ctx:ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a
val err  : ?ctx:ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a

val fatal : ctx -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val fixme : ?ctx:ctx -> ('a, Format.formatter, unit, unit) format4 -> 'a

val kfsprintf : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
