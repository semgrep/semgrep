(*
   Unit tests for the Entropy module
*)

open Printf

let t = Testo.create
let entropy_threshold = 64.
let density_threshold = 0.6
let low_entropy_strings = [ ""; "a"; "ab"; "!@"; "change"; "1234" ]

let high_entropy_strings =
  [
    "ringerringerringerringerringerringerringerringerringerringerringer";
    "Many hands make light work.";
    "TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsuCg==";
    (* same, Base64 *)
    "4d616e792068616e6473206d616b65206c6967687420776f726b2e0a";
    (* same, hex *)
    "4d616e792068616e" (* 64 bits, hex-encoded *);
  ]

let low_density_strings =
  [
    "ringerringerringerringerringerringerringerringerringerringerringer";
    "Many hands make light work.";
  ]

let high_density_strings =
  [
    "!@";
    "TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsuCg==";
    "4d616e792068616e6473206d616b65206c6967687420776f726b2e0a";
    (* same, hex *)
    "4d616e792068616e" (* 64 bits *);
  ]

let low_score_strings =
  [
    "";
    "a";
    "ab";
    "!@";
    "change";
    "1234";
    "deadbeef";
    "trainers";
    "1one2two3three";
    "iamarealsentence";
    "password";
    "password123";
    "someReallyLongComplicatedObjectiveCMethodName";
    "{$variable.someProperty}";
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@";
    (* grey zone *)
    "ringerringerringerringerringerringerringerringerringerringerringer";
    "Many hands make light work.";
    "p@ssw0rd123!";
    "yrdffxsinh";
    "vasuesuugk";
    "labtknqenu";
    "mrlghjolna";
    "mppugsdwep";
    "sjuvalxrbm";
    "ksndhkwhfv";
  ]

let high_score_strings =
  [
    "TWFueSBoYW5kcyBtYWtlIGxpZ2h0IHdvcmsuCg==";
    (* same, Base64 *)
    "4d616e792068616e6473206d616b65206c6967687420776f726b2e0a";
    (* same, hex *)
    "4d616e792068616e";
    (* 64 bits *)
    "d3a447630194bd4b";
    "3904f09ecc562711";
    "e8c74bc779f309cc";
    "deb6fe1bd0cc66b9";
    "f87a601e58d448be";
    "818a9aa26d57d906";
    "ed99c76aa0cdb4a1";
    "d800a95092455de2";
    "d7fa5a34f0342748";
    "ba5e0fc0242f210e";
    "dafsklmztynxi";
    (* grey zone *)
    "wezfzcqmoz";
    "nvzihfyebn";
    "vxawzbjtyt";
  ]

let print_info s =
  let entropy = Entropy.entropy s in
  let density = Entropy.information_density s in
  let score = Entropy.score s in
  printf "%s:\n  entropy: %.3f bits\n  information density: %.6f\n  score: %i\n"
    s entropy density score

let test_information_density () =
  print_info "";
  print_info "a";
  print_info "ab";
  print_info "abc";
  Alcotest.(check bool)
    "NaN information density" true
    (Float.is_nan (Entropy.information_density ""));
  Alcotest.(check bool)
    "positive information density" true
    (Entropy.information_density "a" > 0.);
  Alcotest.(check bool)
    "positive information density" true
    (Entropy.information_density "ab" > 0.);
  Alcotest.(check bool)
    "positive information density" true
    (Entropy.information_density "abc" > 0.)

let get_entropies strings =
  strings
  |> List_.map (fun s ->
         print_info s;
         (s, Entropy.entropy s))

let test_low_entropy () =
  get_entropies low_entropy_strings
  |> List.iter (fun (s, e) ->
         if not (e < entropy_threshold) then
           Alcotest.fail
             (sprintf
                "string %S has an entropy of %g but it was expected to be less \
                 than %g."
                s e entropy_threshold))

let test_high_entropy () =
  get_entropies high_entropy_strings
  |> List.iter (fun (s, e) ->
         if not (e > entropy_threshold) then
           Alcotest.fail
             (sprintf
                "string %S has an entropy of %g but it was expected to be less \
                 than %g."
                s e entropy_threshold))

let get_densities strings =
  strings
  |> List_.map (fun s ->
         print_info s;
         (s, Entropy.information_density s))

let test_low_density () =
  get_densities low_density_strings
  |> List.iter (fun (s, e) ->
         assert (e > 0.);
         if not (e < 1.) then
           Alcotest.fail
             (sprintf
                "string %S has an information density of %g but it was \
                 expected to be less than %g."
                s e density_threshold))

let test_high_density () =
  get_densities high_density_strings
  |> List.iter (fun (s, e) ->
         if not (e > 1.) then
           Alcotest.fail
             (sprintf
                "string %S has an information density of %g but it was \
                 expected to be less than %g."
                s e density_threshold))

let get_scores strings =
  strings
  |> List_.map (fun s ->
         print_info s;
         (s, Entropy.score s))

let test_low_score () =
  get_scores low_score_strings
  |> List.iter (fun (s, x) ->
         assert (x >= 0);
         if not (x < 2) then
           Alcotest.fail
             (sprintf
                "string %S has a score of %i but it was expected to be less \
                 than 2."
                s x))

let test_high_score () =
  get_scores high_score_strings
  |> List.iter (fun (s, x) ->
         assert (x >= 0);
         if not (x = 2) then
           Alcotest.fail
             (sprintf "string %S has a score of %i but it was expected to be 2."
                s x))

let tests =
  [
    t "information density" test_information_density;
    t "low entropy" test_low_entropy;
    t "high entropy" test_high_entropy;
    t "low density" test_low_entropy;
    t "high density" test_high_entropy;
    t "low score" test_low_score;
    t "high score" test_high_score;
  ]
