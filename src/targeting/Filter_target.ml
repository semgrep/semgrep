(*************************************************************************)
(* Prelude *)
(*************************************************************************)
(* Filter target candidates.

   Filtering each (rule, target) pair can become problematic since the number
   of such pairs is O(number of targets * number of rules).
   TODO? This is why we should cache the results of this step.
   This allows reducing the number of rules to the number of different
   languages and patterns used by the rules.
   update: there used to be such an opti, but Pad removed it, because
   it was not clear that was the actual bottleneck. Gitignore
   seems currently to be the thing to optimize.

   Partially translated from target_manager.py
*)

(*************************************************************************)
(* Helpers *)
(*************************************************************************)

(*************************************************************************)
(* Entry points *)
(*************************************************************************)

(* Used by Core_runner.split_jobs_by_language() *)
let filter_target_for_analyzer (analyzer : Analyzer.t) (path : Fpath.t) : bool =
  match analyzer with
  | L (lang, langs) ->
      (* ok if the file appears to be in one of rule's languages *)
      lang :: langs
      |> List.exists (fun lang -> Guess_lang.inspect_file_p lang path)
  | LRegex
  | LSpacegrep
  | LAliengrep ->
      true

(*
   Tentative plan:

   - write tests specifically to test paths.include and paths.exclude
     in different contexts (absolute target paths, relative target paths,
     git project, non-git project);
   - make sure that the paths passed to the glob matcher are relative
     to the project root;
   - use the same rules as for gitignore (forget about Python's wcmatch
     that we're not going to reimplement completely):
     * a slash at the beginning or in the middle of the path makes it
       anchored;
     * reuse what we implemented for gitignore and semgrepignore.
   - update the semgrep documentation accordingly i.e. mention that the
     standard is gitignore/semgrepignore, not wcmatch anymore.

   Details:

   Used by Run_semgrep.semgrep_with_rules().
   See also https://semgrep.dev/docs/writing-rules/rule-syntax/#paths
   TODO: according to the doc,
   "Paths [in include: and exclude: patterns] are relative to the root
   directory of the scanned project" but it seems pysemgrep does not honor
   this thing because adding an exclude such as "metachecking/*" will
   filter code even in subdirs calls metachecking. The glob is not anchored,
   even when it contains a '/'.
   LESS: there used to be a time where semgrep-core was handling the
   include/exclude too. Can we find back this code?
*)
let filter_paths (paths : Rule.paths) (path : Fpath.t) : bool =
  let match_glob (pat : Rule.glob) (path : Fpath.t) : bool =
    (* ugly: pysemgrep adds a leading **/ and a suffix /** to each pattern.
     * See target_manager.py preprocess_path_patterns()
     * python:
     *    Convert semgrep's path include/exclude patterns to wcmatch's glob
     *    patterns. In semgrep, pattern "foo/bar" should match paths
     *    "x/foo/bar", "foo/bar/x", and "x/foo/bar/x". It implicitly matches
     *    zero or more directories at the beginning and the end
     *    of the pattern. In contrast, we have to explicitly specify the
     *    globstar (**) patterns in wcmatch. This function will converts
     *    a pattern "foo/bar" into "**/foo/bar" and "**/foo/bar/**". We need
     *    the pattern without the trailing "/**" because "foo/bar.py/**"
     *    won't match "foo/bar.py".
     * LATER? compute the ppath instead? and use a Gitignore like semantic?
     *)
    let pat1 = Glob.Pattern.(append [ Any_subpath ] (snd pat)) in
    let pat2 =
      Glob.Pattern.(append [ Any_subpath ] (append (snd pat) [ Any_subpath ]))
    in

    let cpat1 =
      Glob.Match.(compile ~source:(string_loc ~source_kind:None (fst pat)) pat1)
    in
    let cpat2 =
      Glob.Match.(compile ~source:(string_loc ~source_kind:None (fst pat)) pat2)
    in
    Glob.Match.run cpat1 (Fpath.to_string path)
    || Glob.Match.run cpat2 (Fpath.to_string path)
  in

  let { Rule.require; exclude } = paths in
  let is_excluded = exclude |> List.exists (fun pat -> match_glob pat path) in
  (* from the doc: "when mixing inclusion and exclusion filters,
   * the exclusion ones take precedence."
   *)
  if is_excluded then false
  else
    let is_required =
      match require with
      (* no require patterns means no constraints *)
      | [] -> true
      | _xs -> require |> List.exists (fun pat -> match_glob pat path)
    in
    is_required
[@@profiling]
