(*
   A special built-in string analyzer that estimates whether a string
   looks like a secret.
*)

let log2 x = log x /. log 2.

(* Since the trigram counts are given only for uppercase letters A-Z,
   we have to assume a reasonable entropy value for the other characters
   whose frequency is often more than 1/256.
   Here, we assume a frequency of 1/62 which is what we get if we picked
   uniformly among [A-Za-z0-9].
*)
let unknown_char_entropy = log2 62.

let normalize_string s =
  let buf = Buffer.create (String.length s) in
  s
  |> String.iter (fun c ->
         match c with
         | 'a' .. 'z' -> Buffer.add_char buf (Char.uppercase_ascii c)
         | 'A' .. 'Z' -> Buffer.add_char buf c
         | c ->
             (* everything else, including:
                - digits
                - punctuation
                - non-ascii UTF-8 bytes
             *)
             Buffer.add_char buf c);
  Buffer.contents buf

(* Set in Data_init.init() from data in Entropy_data.ml
 * The intermediate variable below is used to save space in engine.js
 *)
let english_trigrams_ref = ref [||]

(*
   Load trigrams:
   - determine each trigram's frequency, expressed as an entropy
     (entropy = -log_2(freq)).
   - determine each character's frequence, expressed as an entropy.
   The character entropy is used to obtain a nonzero frequency for the
   trigrams whose count is zero. They could also be used to handle the
   end of strings where we don't have full trigrams.
*)
let load_trigrams () =
  let ar = !english_trigrams_ref in
  (* Load trigram frequencies *)
  let trigram_entropies = Hashtbl.create (Array.length ar) in
  (* we explicitly use int64 here to avoid overflow *)
  (* on 32 bit systems (like Js_of_ocaml), trigram_total_count *)
  (* overflows :( *)
  let trigram_total_count = ref 0L in
  Array.iter
    (fun (trigram, count) ->
      assert (count >= 1);
      assert (String.length trigram = 3);
      trigram_total_count := Int64.add !trigram_total_count (Int64.of_int count))
    ar;
  let trigram_total_count = !trigram_total_count in
  Array.iter
    (fun (trigram, count) ->
      let trigram_entropy =
        log2 (Int64.to_float trigram_total_count /. float count)
      in
      (* Ensure is not nan *)
      assert (not Float.(is_nan trigram_entropy));
      Hashtbl.add trigram_entropies trigram trigram_entropy)
    ar;
  (* Load character frequencies *)
  let char_total_count = Int64.mul 3L trigram_total_count in
  let char_counts = Array.make 256 0 in
  Array.iter
    (fun (trigram, count) ->
      trigram
      |> String.iter (fun c ->
             let i = Char.code c in
             char_counts.(i) <- char_counts.(i) + count))
    ar;
  let char_entropies =
    Array.init 256 (fun i ->
        let char_count = char_counts.(i) in
        if char_count = 0 (* digit, punctuation, emoji byte, etc. *) then
          unknown_char_entropy
        else log2 (Int64.to_float char_total_count /. float char_count))
  in
  (trigram_entropies, char_entropies)

let data_tables = lazy (load_trigrams ())

(*
   A string is scanned as follows:

   012345
   ---
    ---
     ---
      ---
       --
        -

   For each position, we consider a substring which is either a trigram
   or the longest possible substring, whichever is shorter. Since a trigram
   overlaps with two other substrings, we divide its entropy by 3 to
   get back the entropy corresponding to a single byte.
*)
let get_substring_entropy s =
  let trigram_entropies, char_entropies = Lazy.force data_tables in
  match String.length s with
  | 3 ->
      let trigram_entropy =
        match Hashtbl.find_opt trigram_entropies s with
        | Some x -> x
        | None ->
            let e1 = s.[0] |> Char.code |> Array.get char_entropies in
            let e2 = s.[1] |> Char.code |> Array.get char_entropies in
            let e3 = s.[2] |> Char.code |> Array.get char_entropies in
            e1 +. e2 +. e3
      in
      trigram_entropy /. 3.
  | 2 ->
      let e1 = s.[0] |> Char.code |> Array.get char_entropies in
      let e2 = s.[1] |> Char.code |> Array.get char_entropies in
      (e1 +. e2) /. 2.
  | 1 -> s.[0] |> Char.code |> Array.get char_entropies
  | __else__ -> assert false

let iter_substrings s f =
  for i = 0 to String.length s - 3 do
    String.sub s i 3 |> normalize_string |> f
  done;
  let i = String.length s - 2 in
  if i >= 0 then String.sub s i 2 |> normalize_string |> f;
  let i = String.length s - 1 in
  if i >= 0 then String.sub s i 1 |> normalize_string |> f

let entropy_from_trigrams s =
  let e = ref 0. in
  iter_substrings (normalize_string s) (fun sub ->
      e := !e +. get_substring_entropy sub);
  !e

(*
   This test is useful for long chains of the same character such
   as "xxxxxxxxxxxxxxxxxxxxxxx".
   Shorter strings have low entropy anyway.

   This works only with ascii characters but that's probably good enough.
*)
let is_repeated_char s =
  if s = "" then false
  else
    let c0 = s.[0] in
    try
      String.iter (fun c -> if c <> c0 then raise Exit) s;
      true
    with
    | Exit -> false

let entropy s =
  if is_repeated_char s then (* somewhat arbitrary low entropy *) 1.
  else entropy_from_trigrams s

let entropy_data s =
  let ent = entropy s in
  let density = ent /. (8. *. float (String.length s)) in
  (ent, density)

(* correctly returns NaN for the empty string *)
let information_density s = entropy_data s |> snd

(* Minimum number of bits of information in the string to consider it
   high-entropy *)
let entropy_threshold = 64. (* bits *)
let density_threshold = 0.6
let score_entropy x = if x > entropy_threshold then 1 else 0
let score_density x = if x > density_threshold then 1 else 0

let score s =
  if s = "" then 0
  else
    let ent, density = entropy_data s in
    score_entropy ent + score_density density

let has_high_score s = score s = 2
