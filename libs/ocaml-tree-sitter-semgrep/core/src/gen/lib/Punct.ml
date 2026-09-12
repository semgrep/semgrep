(*
   Names for graphical ascii symbols commonly used in programming language,
   aka punctuations.
*)

(* Fallback for bytes outside the ascii range. *)
let unknown = "UNK"

let list = [
  '\x00', "NUL";
  '\x01', "SOH";
  '\x02', "STX";
  '\x03', "ETX";
  '\x04', "EOT";
  '\x05', "ENQ";
  '\x06', "ACK";
  '\x07', "BEL";
  '\x08', "BS";
  '\x09', "HT";
  '\x0A', "LF";
  '\x0B', "VT";
  '\x0C', "FF";
  '\x0D', "CR";
  '\x0E', "SO";
  '\x0F', "SI";
  '\x10', "DLE";
  '\x11', "DC1";
  '\x12', "DC2";
  '\x13', "DC3";
  '\x14', "DC4";
  '\x15', "NAK";
  '\x16', "SYN";
  '\x17', "ETB";
  '\x18', "CAN";
  '\x19', "EM";
  '\x1A', "SUB";
  '\x1B', "ESC";
  '\x1C', "FS";
  '\x1D', "GS";
  '\x1E', "RS";
  '\x1F', "US";
  ' ', "SPACE";
  '!', "BANG";
  '"', "DQUOT";
  '#', "HASH";
  '$', "DOLLAR";
  '%', "PERC";
  '&', "AMP";
  '\'', "SQUOT";
  '(', "LPAR";
  ')', "RPAR";
  '*', "STAR";
  '+', "PLUS";
  ',', "COMMA";
  '-', "DASH";
  '.', "DOT";
  '/', "SLASH";
  (* omit 0-9 *)
  ':', "COLON";
  ';', "SEMI";
  '<', "LT";
  '=', "EQ";
  '>', "GT";
  '?', "QMARK";
  '@', "AT";
  (* omit A-Z *)
  '[', "LBRACK";
  '\\', "BSLASH";
  ']', "RBRACK";
  '^', "HAT";
  (* omit _ *)
  '`', "BQUOT";
  (* omit a-z *)
  '{', "LCURL";
  '|', "BAR";
  '}', "RCURL";
  '~', "TILDE";
  '\x7F', "DEL";
]

let table =
  let tbl = Hashtbl.create 200 in
  List.iter (fun (c, name) ->
    Hashtbl.add tbl c name
  ) list;
  tbl

let get_char_name c =
  if Char.code c < 128 then
    Hashtbl.find_opt table c
  else
    Some unknown

let to_alphanum s =
  let buf = Buffer.create 100 in
  String.iter (fun c ->
    match get_char_name c with
    | None -> Buffer.add_char buf c
    | Some name ->
        (* do we want to separate char names with underscores? *)
        Buffer.add_string buf name
  ) s;
  Buffer.contents buf
