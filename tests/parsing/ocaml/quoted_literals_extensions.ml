let text = {custom|hello @world %s|custom}
let escaped = "a\n\x41\u{0042}"
let char = '\n'
let payload = [%identity 42]
[%%item let x = 1]
let quoted = {%text.data|hello|}
{%%text.data|world|}
let unicode_delimiter = {à|normalized|à}
(* é' '"' *)
let after_comment = 42
