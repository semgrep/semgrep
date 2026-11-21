(** Python literal decoding. Scan escape sequences once, preserving unknown
    escapes and keeping byte and Unicode semantics separate. The immutable
    Unicode-name index is initialized on demand through a domain-safe cache. *)

module Names = Map.Make (String)

let unicode_names =
  SharedMemo.make (fun () ->
      let rec collect codepoint names =
        if codepoint > 0x10ffff then names
        else if not (Uchar.is_valid codepoint) then
          collect (codepoint + 1) names
        else
          let uchar = Uchar.of_int codepoint in
          let name = Uucp.Name.name uchar in
          let names =
            if name = "" then names else Names.add name codepoint names
          in
          let names =
            List.fold_left
              (fun names (_, alias) -> Names.add alias codepoint names)
              names
              (Uucp.Name.name_alias uchar)
          in
          collect (codepoint + 1) names
      in
      collect 0 Names.empty)

exception Invalid_escape of string

let add_codepoint buffer codepoint =
  if codepoint > 0x10ffff then
    raise (Invalid_escape "Unicode escape is outside the Unicode range")
  else if Uchar.is_valid codepoint then
    Buffer.add_utf_8_uchar buffer (Uchar.of_int codepoint)
  else begin
    (* Python permits lone surrogates; preserve them using surrogate-pass UTF-8. *)
    Buffer.add_char buffer (Char.chr (0xe0 lor (codepoint lsr 12)));
    Buffer.add_char buffer (Char.chr (0x80 lor ((codepoint lsr 6) land 0x3f)));
    Buffer.add_char buffer (Char.chr (0x80 lor (codepoint land 0x3f)))
  end

let decode ~prefix text =
  let prefix = String.lowercase_ascii prefix in
  if String.contains prefix 'r' then Ok text
  else
    let bytes = String.contains prefix 'b' in
    let length = String.length text in
    let buffer = Buffer.create length in
    let numeric start count base =
      if start + count > length then
        raise (Invalid_escape "Truncated numeric escape");
      let digits = String.sub text start count in
      let marker = if base = 16 then "0x" else "0o" in
      match int_of_string_opt (marker ^ digits) with
      | Some value
        when value >= 0
             && String.for_all
                  (fun c ->
                    match c with
                    | '0' .. '7' -> true
                    | '8' .. '9'
                    | 'a' .. 'f'
                    | 'A' .. 'F' ->
                        base = 16
                    | _ -> false)
                  digits ->
          value
      | _ -> raise (Invalid_escape "Invalid numeric escape")
    in
    let add_numeric value =
      if bytes then Buffer.add_char buffer (Char.chr (value land 0xff))
      else add_codepoint buffer value
    in
    let rec scan index =
      if index = length then Buffer.contents buffer
      else if text.[index] <> '\\' then begin
        Buffer.add_char buffer text.[index];
        scan (index + 1)
      end
      else if index + 1 = length then
        raise (Invalid_escape "Trailing backslash in string")
      else
        let next = index + 2 in
        match text.[index + 1] with
        | '\n' -> scan next
        | '\r' ->
            scan
              (if next < length && text.[next] = '\n' then next + 1 else next)
        | ('\\' | '\'' | '"') as c ->
            Buffer.add_char buffer c;
            scan next
        | ('a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v') as c ->
            let decoded =
              match c with
              | 'a' -> '\007'
              | 'b' -> '\b'
              | 'f' -> '\012'
              | 'n' -> '\n'
              | 'r' -> '\r'
              | 't' -> '\t'
              | _ -> '\011'
            in
            Buffer.add_char buffer decoded;
            scan next
        | '0' .. '7' ->
            let start = index + 1 in
            let rec end_octal stop =
              if stop < length && stop < start + 3 then
                match text.[stop] with
                | '0' .. '7' -> end_octal (stop + 1)
                | _ -> stop
              else stop
            in
            let stop = end_octal start in
            add_numeric (numeric start (stop - start) 8);
            scan stop
        | 'x' ->
            add_numeric (numeric next 2 16);
            scan (next + 2)
        | ('u' | 'U') as c when not bytes ->
            let count = if c = 'u' then 4 else 8 in
            add_codepoint buffer (numeric next count 16);
            scan (next + count)
        | 'N' when not bytes -> (
            if next = length || text.[next] <> '{' then
              raise (Invalid_escape "Missing opening brace in Unicode name");
            match String.index_from_opt text (next + 1) '}' with
            | None -> raise (Invalid_escape "Unterminated Unicode name")
            | Some stop ->
                let name = String.sub text (next + 1) (stop - next - 1) in
                let name = String.uppercase_ascii name in
                (match Names.find_opt name (unicode_names ()) with
                | None ->
                    raise (Invalid_escape ("Unknown Unicode name: " ^ name))
                | Some codepoint -> add_codepoint buffer codepoint);
                scan (stop + 1))
        | c ->
            Buffer.add_char buffer '\\';
            Buffer.add_char buffer c;
            scan next
    in
    try Ok (scan 0) with
    | Invalid_escape message -> Error message
