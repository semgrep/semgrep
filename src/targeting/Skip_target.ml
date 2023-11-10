(*
   Skip targets.
*)
open Common
module Resp = Semgrep_output_v1_t

(****************************************************************************)
(* Minified files detection (via whitespace stats) *)
(****************************************************************************)

(*
   The 7% threshold works well for javascript files based on looking at
   .min.js files and other .js found in various github repos.

   - very few minified files have more than 5% of whitespace. Those with more
     than 5% contain embedded data strings e.g.
     "1C10 LX0 1C10 LX0 1C10 Mn0 MN0 2jz0 ...".
   - no legitimate source files have less than 8% of whitespace and are larger
     than 500 bytes at the same time.
   - only some really short source files (2-3 lines) were found to have
     between 5% and 8% whitespace.

  However, this threshold works less well for java files because it is not
  unusual to have large import blocks at the top, resulting in a very low
  whitespace fraction. This should not affect very many Java files, so for
  now it is ok that we leave it. The consequence of reducing the frequency
  is that Semgrep would likely take longer as it scans more costly minified
  files. TODO we might want to do some benchmarking and change this default
*)
let min_whitespace_frequency = 0.07

(*
   This is for the few minified files that embed a bunch of space-separated
   items in large data strings.
   0.001 is an average of 1000 bytes per line, which doesn't occur with
   normal programming languages. It can approach this with Markdown or maybe
   files that are mostly comments made of very long lines.
*)
let min_line_frequency = 0.001

type whitespace_stat = {
  sample_size : int;
  (* size of the block; possibly the whole file *)
  ws_freq : float;
  (* whitespace: CR, LF, space, tab *)
  line_freq : float; (* frequency of lines = 1/(avg line length) *)
}

let whitespace_stat_of_string s =
  (* number of lines = number of LF characters + 1 *)
  let lines = ref 1 in
  let whitespace = ref 0 in
  let other = ref 0 in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | ' '
    | '\t'
    | '\r' ->
        incr whitespace
    | '\n' ->
        incr whitespace;
        incr lines
    | __else__ -> incr other
  done;
  let total = !whitespace + !other in
  let sample_size = String.length s in
  if total =|= 0 then { sample_size; ws_freq = 1.; line_freq = 1. }
  else
    let ws_freq = float !whitespace /. float total in
    let line_freq = float !lines /. float total in
    { sample_size; ws_freq; line_freq }

let whitespace_stat_of_block ?block_size path =
  let s = Guess_lang.get_first_block ?block_size path in
  whitespace_stat_of_string s

let is_minified (path : Fpath.t) =
  if not !Flag_semgrep.skip_minified_files then Ok path
  else
    let stat = whitespace_stat_of_block ~block_size:4096 path in
    (*
     A small file could contain a long URL with no whitespace without being
     minified. That's why we require a minimum file size.
  *)
    if stat.sample_size > 1000 then
      if stat.ws_freq < min_whitespace_frequency then
        Error
          {
            Resp.path;
            reason = Minified;
            details =
              Some
                (spf
                   "file contains too little whitespace: %.3f%% (min = %.1f%%)"
                   (100. *. stat.ws_freq)
                   (100. *. min_whitespace_frequency));
            rule_id = None;
          }
      else if stat.line_freq < min_line_frequency then
        Error
          {
            Resp.path;
            reason = Minified;
            details =
              Some
                (spf
                   "file contains too few lines for its size: %.4f%% (min = \
                    %.2f%%)"
                   (100. *. stat.line_freq)
                   (100. *. min_line_frequency));
            rule_id = None;
          }
      else Ok path
    else Ok path

let exclude_minified_files paths = Common.partition_result is_minified paths

(****************************************************************************)
(* Big file filtering *)
(****************************************************************************)

(*
   Some source files are really huge (> 20 MB) and they cause
   some annoying 'out of memory' crash that sometimes even the use
   of mem_limit_mb above does not solve.

   We could configure the size limit based on a per-language basis if we
   know that a language parser can handle larger files.
*)
let exclude_big_files paths =
  let max_bytes = !Flag_semgrep.max_target_bytes in
  paths
  |> Common.partition_result (fun path ->
         let size = File.filesize path in
         if max_bytes > 0 && size > max_bytes then
           Error
             {
               Resp.path;
               reason = Too_big;
               details =
                 Some
                   (spf "target file size exceeds %i bytes at %i bytes"
                      max_bytes size);
               rule_id = None;
             }
         else Ok path)
