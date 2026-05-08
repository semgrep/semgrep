(* Auto-generated from "semgrep_output_v1.atd" *)
[@@@ocaml.warning "-27-32-33-35-39"]

(**
  Specification of the Semgrep CLI JSON output formats using ATD (see
  https://atd.readthedocs.io/en/latest/ for information on ATD).
  
  This file specifies mainly the JSON formats of:
  
  - the output of the [semgrep scan --json] command
  
  - the output of the [semgrep test --json] command
  
  - the messages exchanged with the Semgrep backend by the [semgrep ci]
  command
  
  It's also (ab)used to specify the JSON input and output of semgrep-core,
  some RPC between pysemgrep and semgrep-core, and a few more internal
  things. We should use separate .atd for those different purposes but ATD
  does not have a proper module system yet and many types are shared so it is
  simpler for now to have everything in one file.
  
  There are other important form of outputs which are not specified here:
  
  - The semgrep metrics sent to https://metrics.semgrep.dev in
  semgrep_metrics.atd
  
  - The parsing stats of semgrep-core -parsing_stats -json have its own
  Parsing_stats.atd
  
  For the definition of the Semgrep input (the rules), see rule_schema_v2.atd
  
  This file has the _v1 suffix to explicitely represent the version of this
  JSON format. If you need to extend this file, please be careful because you
  may break consumers of this format (e.g., the Semgrep playground or Semgrep
  backend or external users of this JSON). See
  https://atd.readthedocs.io/en/latest/atdgen-tutorial.html#smooth-protocol-upgrades
  for more information on how to smoothly extend the types in this file.
  
  Any backward incompatible changes should require to upgrade the major
  version of Semgrep as this JSON output is part of the "API" of Semgrep (any
  incompatible changes to the rule format should also require a major version
  upgrade). Hopefully, we will always be backward compatible. However, a few
  fields are tagged with \[EXPERIMENTAL\] meaning external users should not
  rely on them as those fields may be changed or removed. They are not part
  of the "API" of Semgrep.
  
  Again, keep in mind that this file is used both by the CLI to *produce* a
  JSON output, and by our backends to *consume* the JSON, including to
  consume the JSON produced by old versions of the CLI. As of Nov 2024, our
  backend is still supporting as far as Semgrep 1.50.0 released Nov 2023.
  (see server/semgrep_app/util/cli_version_support.py in the semgrep-app
  repo)
  
  This file is translated in OCaml modules by atdgen. Look for the
  corresponding Semgrep_output_v1_\[tj\].ml\[i\] generated files under dune's
  _build/ folder. A few types below have the 'deriving show' decorator
  because those types are reused in semgrep core data structures and we make
  heavy use of 'deriving show' in OCaml to help debug things.
  
  This file is also translated in Python modules by atdpy. For Python, a few
  types have the 'dataclass(frozen=True)' decorator so that the class can be
  hashed and put in set. Indeed, with 'Frozen=True' the class is immutable
  and dataclass can autogenerate a hash function for it.
  
  Finally this file is translated in jsonschema/openapi spec by atdcat, and
  in Typescript modules by atdts.
  
  History:
  
  - the types in this file were originally inferred from JSON_report.ml for
  use by spacegrep when it was separate from semgrep-core. It's now also
  useds in JSON_report.ml (now called Core_json_output.ml)
  
  - it was extended to not only support semgrep-core JSON output but also
  (py)semgrep CLI output!
  
  - it was then simplified with the osemgrep migration effort by removing
  gradually the semgrep-core JSON output.
  
  - it was extended to support 'semgrep ci' output to type most messages sent
  between the Semgrep CLI and the Semgrep backend
  
  - we use this file to specify RPCs between pysemgrep and semgrep-core for
  the gradual migration effort of osemgrep
  
  - merged what was in Input_to_core.atd here
*)

(** RFC 3339 format *)
type datetime = ATD_string_wrap.Datetime.t
  [@@deriving ord]

type dependency_child = { package: string; version: string } [@@deriving ord]

type dependency_kind = 
    Direct
      (**
        we depend directly on the 3rd-party library mentioned in the lockfile
        (e.g., use of log4j library and concrete calls to log4j in 1st-party
        code). log4j must be declared as a direct dependency in the manifest
        file.
      *)
  | Transitive
      (**
        we depend indirectly (transitively) on the 3rd-party library (e.g.,
        if we use lodash which itself uses internally log4j then lodash is a
        Direct dependency and log4j a Transitive one)
        
        alt: Indirect
      *)
  | Unknown
      (**
        If there is insufficient information to determine the transitivity,
        such as a requirements.txt file without a requirements.in manifest,
        we leave it Unknown.
      *)

  [@@deriving ord, eq, show]

(**
  both ecosystem and transitivity below have frozen=True so the generated
  classes can be hashed and put in sets (see calls to reachable_deps.add() in
  semgrep SCA code)
  
  alt: type package_manager
*)
type ecosystem = 
    Npm
  | Pypi
  | Gem
  | Gomod
  | Cargo
  | Maven
  | Composer
  | Nuget
  | Pub
  | SwiftPM
  | Cocoapods
  | Mix
      (**
        Deprecated: Mix is a build system, should use Hex, which is the
        ecosystem
      *)
  | Hex
  | Opam

  [@@deriving eq, ord, show { with_path = false }]

type fpath = ATD_string_wrap.Fpath.t [@@deriving eq, ord, show]

type found_dependency = {
  package: string;
  version: string;
  ecosystem: ecosystem;
  allowed_hashes: (string * string list) list (** ??? *);
  resolved_url: string option;
  transitivity: dependency_kind;
  manifest_path: fpath option
    (**
      Path to the manifest file that defines the project containing this
      dependency. Examples: package.json, nested/folder/pom.xml
    *);
  lockfile_path: fpath option
    (**
      Path to the lockfile that contains this dependency. Examples:
      package-lock.json, nested/folder/requirements.txt, go.mod. Since 1.87.0
    *);
  line_number: int option
    (**
      The line number of the dependency in the lockfile. When combined with
      the lockfile_path, this can identify the location of the dependency in
      the lockfile.
    *);
  children: dependency_child list option
    (**
      If we have dependency relationship information for this dependency,
      this field will include the name and version of other found_dependency
      items that this dependency requires. These fields must match values in
      `package` and `version` of another `found_dependency` in the same set
    *);
  git_ref: string option
    (**
      Git ref of the dependency if the dependency comes directly from a git
      repo. Examples: refs/heads/main, refs/tags/v1.0.0,
      e5c704df4d308690fed696faf4c86453b4d88a95. Since 1.66.0
    *)
}
  [@@deriving ord]

type lockfile_kind = 
    PipRequirementsTxt
  | PoetryLock
  | PipfileLock
  | UvLock
  | NpmPackageLockJson
  | YarnLock
  | PnpmLock
  | BunLock
  | BunBinaryLock (** Bun's deprecated binary bun.lockb format *)
  | GemfileLock
  | GoModLock
  | CargoLock
  | MavenDepTree (** Not a real lockfile *)
  | GradleLockfile
  | ComposerLock
  | NugetPackagesLockJson
  | PubspecLock
  | SwiftPackageResolved (** not a real lockfile *)
  | PodfileLock
  | MixLock
  | ConanLock
  | OpamLocked

  [@@deriving show { with_path = false }, eq, yojson]

type lockfile = { kind: lockfile_kind; path: fpath } [@@deriving show, eq]

type manifest_kind = 
    RequirementsIn
      (**
        A Pip Requirements.in in file, which follows the format of
        requirements.txt
        https://pip.pypa.io/en/stable/reference/requirements-file-format/
      *)
  | SetupPy
      (**
        A setup.py file, which is a Python file that contains the setup
        configuration for a Python project.
        https://packaging.python.org/en/latest/guides/distributing-packages-using-setuptools/#setup-py
      *)
  | PackageJson
      (**
        An NPM package.json manifest file
        https://docs.npmjs.com/cli/v10/configuring-npm/package-json
      *)
  | Gemfile
      (**
        A Ruby Gemfile manifest https://bundler.io/v2.5/man/gemfile.5.html
      *)
  | GoModManifest (** go.mod https://go.dev/doc/modules/gomod-ref *)
  | CargoToml
      (**
        cargo.toml - https://doc.rust-lang.org/cargo/reference/manifest.html
      *)
  | PomXml
      (**
        A Maven pom.xml manifest file
        https://maven.apache.org/guides/introduction/introduction-to-the-pom.html
      *)
  | BuildGradle
      (**
        A Gradle build.gradle build file
        https://docs.gradle.org/current/userguide/build_file_basics.html
      *)
  | BuildGradleKts
      (**
        A Gradle build.gradle.kts file, which uses Kotlin instead of Groovy.
      *)
  | SettingsGradle
      (**
        A Gradle settings.gradle file
        https://docs.gradle.org/current/userguide/settings_file_basics.html.
        Multi-project builds are defined by settings.gradle rather than
        build.gradle:
        https://docs.gradle.org/current/userguide/multi_project_builds.html#multi_project_builds
      *)
  | ComposerJson
      (** composer.json - https://getcomposer.org/doc/04-schema.md *)
  | NugetManifestJson
      (**
        manifest for nuget. Could not find a reference; this may not actually
        exist
      *)
  | PubspecYaml (** pubspec.yaml - https://dart.dev/tools/pub/pubspec *)
  | PackageSwift
      (**
        Package.swift
        https://docs.swift.org/package-manager/PackageDescription/PackageDescription.html
      *)
  | Podfile
      (** Podfile - https://guides.cocoapods.org/using/the-podfile.html *)
  | MixExs
      (**
        mix.exs
        https://hexdocs.pm/elixir/introduction-to-mix.html#project-compilation
      *)
  | Pipfile (** Pipfile - https://pipenv.pypa.io/en/latest/pipfile.html *)
  | PyprojectToml
      (**
        pyproject.toml
        https://packaging.python.org/en/latest/guides/writing-pyproject-toml/
      *)
  | ConanFileTxt
      (**
        conanfile.txt
        https://docs.conan.io/2.9/reference/conanfile_txt.html#conanfile-txt
      *)
  | ConanFilePy
      (**
        conanfile.py - https://docs.conan.io/2.9/reference/conanfile.html
      *)
  | Csproj
      (**
        .csproj - https://docs.microsoft.com/en-us/dotnet/core/tools/csproj
      *)
  | OpamFile
      (**
        opam - https://opam.ocaml.org/doc/Manual.html#Package-definitions
      *)
  | BuildSbt
      (** build.sbt - https://www.scala-sbt.org/1.x/docs/Basic-Def.html *)

  [@@deriving show { with_path = false }, eq]

type manifest = { kind: manifest_kind; path: fpath } [@@deriving show, eq]

(**
  This is used in rules to specify the severity of matches/findings. alt:
  could be called rule_severity, or finding_severity.
  
{v
  Error = something wrong that must be fixed
  Warning = something wrong that should be fixed
  Info = some special condition worth knowing about
  Experiment = deprecated: guess what
  Inventory = deprecated: was used for the Code Asset Inventory (CAI) project
v}
*)
type match_severity = [
    `Error
  | `Warning
  | `Experiment
  | `Inventory
  | `Critical
      (**
        since 1.72.0, meant to replace the cases above where Error -> High,
        Warning -> Medium. Critical/Low are the only really new category here
        without equivalent before. Experiment and Inventory above should be
        removed. Info can be kept.
      *)
  | `High
  | `Medium
  | `Low
  | `Info
      (** generic placeholder for non-risky things (including experiments) *)
]
  [@@deriving eq, ord, show]

(**
  Note that this type is used in Matching_explanation.ml hence the need for
  deriving show below.
*)
type matching_operation = 
    And
  | Or
  | Inside
  | Anywhere
  | XPat of string
      (**
        XPat for eXtended pattern. Can be a spacegrep pattern, a regexp
        pattern, or a proper semgrep pattern. see
        semgrep-core/src/core/XPattern.ml
      *)
  | Negation
  | Filter of string
  | Taint
  | TaintSource
  | TaintSink
  | TaintSanitizer
  | EllipsisAndStmts
  | ClassHeaderAndElems

  [@@deriving show { with_path = false}]

(** Note that there is no filename here like in 'location' below *)
type position = {
  line: int;
  col: int;
  offset: int
    (**
      Byte position from the beginning of the file, starts at 0. OCaml code
      sets it correctly. Python code sets it to a dummy value (-1). This uses
      '~' because pysemgrep < 1.30? was *producing* positions without offset
      sometimes, and we want the backend to still *consume* such positions.
      Note that pysemgrep 1.97 was still producing dummy positions without an
      offset so we might need this ~offset longer than expected?
    *)
}
  [@@deriving ord, show]

(** a.k.a range *)
type location = { path: fpath; start: position; end_ (*atd end *): position }
  [@@deriving ord, show]

(**
  The string attached to the location is the actual code from the file. This
  can contain sensitive information so be careful!
  
  TODO: the type seems redundant since location already specifies a range.
  maybe this saves some effort to the user of this type which do not need to
  read the file to get the content.
*)
type loc_and_content = (location * string)
  [@@deriving ord]

(**
  This type happens to be mostly the same as a loc_and_content for now, but
  it's split out because Iago has plans to extend this with more information
*)
type match_intermediate_var = {
  location: location;
  content: string
    (**
      Unlike abstract_content, this is the actual text read from the
      corresponding source file
    *)
}
  [@@deriving ord]

(**
  Used for a best-effort report to users about what findings they get with
  the pro engine that they couldn't with the oss engine.
  
{v
  interproc_taint = requires interprocedural taint
  interfile_taint = requires interfile taint
  proprietary_language = requires some non-taint pro feature
v}
*)
type pro_feature = {
  interproc_taint: bool;
  interfile_taint: bool;
  proprietary_language: bool
}
  [@@deriving ord, show]

(**
  Report the engine used to detect each finding. Additionally, if we are able
  to infer that the finding could only be detected using the pro engine,
  report that the pro engine is required and include basic information about
  which feature is required.
  
{v
  OSS = ran with OSS
  PRO = ran with PRO, but we didn't infer that OSS couldn't have found this
  finding
  PRO_REQUIRED = ran with PRO and requires a PRO feature (see pro_feature_used)
v}
  
  Note: OSS and PRO could have clearer names, but for backwards compatibility
  we're leaving them as is
*)
type engine_of_finding = [
    `OSS
  | `PRO
  | `PRO_REQUIRED of pro_feature (** Semgrep 1.64.0 or later *)
]
  [@@deriving ord, show]

(** escape hatch *)
type raw_json = JSON.Yojson.t [@@deriving eq, ord, show]

(** e.g., "javascript.security.do-not-use-eval" *)
type rule_id = Rule_ID.t
  [@@deriving show, eq, ord]

type sbom_kind = 
  CycloneDXJson (** cyclonedx json - https://cyclonedx.org/docs/1.4/json/ *)

  [@@deriving show { with_path = false }, eq]

type sbom = {
  kind: sbom_kind;
  is_ephemeral: bool
    (**
      whether or not the SBOM is produced ephemerally, i.e. is not checked in
      to version control. if true, references in resolved dependencies will
      not point to the SBOM itself.
    *);
  path: fpath
}
  [@@deriving show, eq]

type sca_pattern = {
  ecosystem: ecosystem;
  package: string;
  semver_range: string
}
  [@@deriving ord]

type dependency_match = {
  dependency_pattern: sca_pattern;
  found_dependency: found_dependency;
  lockfile: fpath
}
  [@@deriving ord]

type sha1 = ATD_string_wrap.Sha1.t [@@deriving ord]

(** part of cli_match_extra *)
type historical_info = {
  git_commit: sha1
    (**
      Git commit at which the finding is present. Used by "historical" scans,
      which scan non-HEAD commits in the git history. Relevant for finding,
      e.g., secrets which are buried in the git history which we wouldn't
      find at HEAD
    *);
  git_blob: sha1 option
    (**
      Git blob at which the finding is present. Sent in addition to the
      commit since some SCMs have permalinks which use the blob sha, so this
      information is useful when generating links back to the SCM.
    *);
  git_commit_timestamp: datetime
}
  [@@deriving ord]

type svalue_value = {
  svalue_start: position option;
  svalue_end: position option;
  svalue_abstract_content: string (** value? *)
}
  [@@deriving ord]

type metavar_value = {
  start: position
    (**
      for certain metavariable like $...ARGS, 'end' may be equal to 'start'
      to represent an empty metavariable value. The rest of the Python code
      (message metavariable substitution and autofix) works without change
      for empty ranges (when end = start).
    *);
  end_ (*atd end *): position;
  abstract_content: string (** value? *);
  propagated_value: svalue_value option
}
  [@@deriving ord]

(**
  Name/value map of the matched metavariables. The leading '$' must be
  included in the metavariable name.
*)
type metavars = (string * metavar_value) list
  [@@deriving ord]

type transitive_undetermined = { explanation: string option }
  [@@deriving ord]

type transitive_unreachable = {
  analyzed_packages: found_dependency list
    (**
      We didn't find any findings in all the 3rd party libraries that are
      using the 3rd party vulnerable library. This is a "proof of work".
    *);
  explanation: string option
    (** some extra explanation that the user can understand *)
}
  [@@deriving ord]

(**
  This type is used by postprocessors for secrets to report back the validity
  of a finding. No_validator is currently also used when no validation has
  yet occurred, which if that becomes confusing we could adjust that, by
  adding another state.
*)
type validation_state = [
    `Confirmed_valid | `Confirmed_invalid | `Validation_error | `No_validator
]
  [@@deriving eq, ord, show]

type dependency_source = 
    ManifestOnly of manifest
  | LockfileOnly of lockfile
  | ManifestLockfile of (manifest * lockfile)
  | MultiLockfile of dependency_source list
      (**
        The dependency_source should be LockfileOnly or ManifestLockfile, but
        not ManifestOnlyDependencySource. Right now this variant is only used
        by pysemgrep; it is deconstructed in multiple LockfileXxx when
        calling the dynamic resolver. Note that this variant introduces a
        series of problems in the Python code because atdpy generates a
        List\[DependencySource\] and List are not hashable in Python. We had
        to define a special hash function for Subproject to avoid hashing the
        dependency_source.
      *)
  | AuxillarySBOM of (sbom * dependency_source)
      (**
        An SBOM containing dependency information that is not part of the
        dependency source files directly interpreted by the package manager.
        This is connected to a standard dependency source. The attached
        dependency source should not be another AuxillarySBOM. Ideally we
        would restructure this type to encode this requirement.
      *)

  [@@deriving show]

type match_call_trace = 
    CliLoc of loc_and_content
  | CliCall
      of (loc_and_content * match_intermediate_var list * match_call_trace)

  [@@deriving ord]

type match_dataflow_trace = {
  taint_source: match_call_trace option;
  intermediate_vars: match_intermediate_var list option
    (**
      Intermediate variables which are involved in the dataflow. This
      explains how the taint flows from the source to the sink.
    *);
  taint_sink: match_call_trace option
}
  [@@deriving ord]

type cli_match = {
  check_id: rule_id;
  path: fpath;
  start: position;
  end_ (*atd end *): position;
  extra: cli_match_extra
}

and cli_match_extra = {
  metavars: metavars option
    (**
      Since 1.98.0, you need to be logged in to get this field. note: we also
      need ?metavars because dependency_aware code
    *);
  message: string
    (**
      Those fields are derived from the rule but the metavariables they
      contain have been expanded to their concrete value.
    *);
  fix: string option
    (**
      If present, semgrep was able to compute a string that should be
      inserted in place of the text in the matched range in order to fix the
      finding. Note that this is the result of applying both the fix: or
      fix_regex: in a rule.
    *);
  fixed_lines: string list option;
  metadata: raw_json (** fields coming from the rule *);
  severity: match_severity;
  fingerprint: string
    (** Since 1.98.0, you need to be logged in to get those fields *);
  lines: string;
  is_ignored: bool option (** for nosemgrep *);
  sca_info: sca_match option
    (** EXPERIMENTAL: added by dependency_aware code *);
  validation_state: validation_state option
    (**
      EXPERIMENTAL: If present indicates the status of postprocessor
      validation. This field not being present should be equivalent to
      No_validator. Added in semgrep 1.37.0
    *);
  historical_info: historical_info option
    (**
      EXPERIMENTAL: added by secrets post-processing & historical scanning
      code Since 1.60.0.
    *);
  dataflow_trace: match_dataflow_trace option
    (**
      EXPERIMENTAL: For now, present only for taint findings. May be extended
      to others later on.
    *);
  engine_kind: engine_of_finding option;
  extra_extra: raw_json option
    (** EXPERIMENTAL: see core_match_extra.extra_extra *)
}

(** part of cli_match_extra, core_match_extra, and finding *)
and sca_match = {
  reachability_rule: bool
    (**
      does the rule has a pattern part; otherwise it's a "parity" or
      "upgrade-only" rule.
    *);
  sca_finding_schema: int;
  dependency_match: dependency_match;
  reachable: bool;
  kind: sca_match_kind option (** EXPERIMENTAL since 1.108.0 *)
}

(**
  Note that in addition to "reachable" there are also the notions of
  "vulnerable" and "exploitable".
*)
and sca_match_kind = 
    LockfileOnlyMatch of dependency_kind
      (**
        This is used for "parity" or "upgrade-only" rules. transitivity
        indicates whether the match is for a direct or transitive usage of
        the dependency; for a dependency that is both direct and transitive
        two findings should be generated.
      *)
  | DirectReachable
      (**
        found the pattern-part of the SCA rule in 1st-party code (reachable
        as originally defined by Semgrep Inc.) the match location will be in
        some target code.
      *)
  | TransitiveReachable of transitive_reachable
      (**
        found the pattern-part of the SCA rule in third-party code and
        ultimately found a path from 1st party code to this vulnerable
        third-party code. The goal of transitive reachability analysis is to
        change some Undetermined or (LockfileOnlyMatch Transitive) into
        TransitiveReachable or TransitiveUnreachable
      *)
  | TransitiveUnreachable of transitive_unreachable
      (**
        This is a "positive" finding in the sense that semgrep was able to
        prove that the transitive finding is "safe" and can be ignored
        because either there is no call to the pattern-part of the SCA rule
        in 3rd party code, or if there is it's in third-party code that is
        not accessed from the 1st-party code (e.g., via callgraph analysis)
        Note that there is no need for DirectUnreachable because semgrep
        would never generate such a finding. We have TransitiveUnreachable
        because semgrep first generates some Undetermined that we then retag
        as DirectUnreachable.
      *)
  | TransitiveUndetermined of transitive_undetermined
      (**
        could not decide because of the engine limitations (e.g., found the
        use of a vulnerable library in the lockfile but could not find the
        pattern in first party code and could not access third-party code for
        further investigation (similar to (LockfileOnlyMatch Transitive))
      *)

  [@@deriving ord]

and transitive_reachable = {
  matches: (found_dependency * cli_match list) list
    (**
      The matches we found in 3rd party libraries. Ideally the location in
      cli_match are relative to the root of the project so one can display
      matches as package\@/path/to/finding.py
    *);
  callgraph_reachable: bool option
    (**
      LATER: add callgraph information so one can see the path from 1st party
      code to the vulnerable intermediate 3rd party function. This is set to
      None for now.
    *);
  explanation: string option
    (** some extra explanation that the user can understand *)
}

(**
  See the corresponding comment in cli_match_extra for more information about
  the fields below.
*)
type core_match_extra = {
  metavars: metavars;
  engine_kind: engine_of_finding;
  is_ignored: bool;
  message: string option
    (**
      These fields generally come from the rule, but may be set here if
      they're being overriden for that particular finding. This would
      currently occur for rule with a validator for secrets, depending on
      what the validator might match, but could be expanded in the future.
    *);
  metadata: raw_json option;
  severity: match_severity option;
  fix: string option;
  dataflow_trace: match_dataflow_trace option;
  sca_match: sca_match option;
  validation_state: validation_state option;
  historical_info: historical_info option;
  extra_extra: raw_json option
    (**
      Escape hatch to pass untyped info from semgrep-core to the semgrep
      output. Useful for quick experiments, especially when combined with
      semgrep --core-opts flag.
    *)
}

type core_match = {
  check_id: rule_id;
  path: fpath;
  start: position;
  end_ (*atd end *): position;
  extra: core_match_extra
}

(**
  For any "extra" information that we cannot fit at the node itself. This is
  useful for kind-specific information, which we cannot put in the operation
  itself without giving up our ability to derive `show` (needed for
  `matching_operation` below).
*)
type matching_explanation_extra = {
  before_negation_matches: core_match list option
    (**
      Only present in And kind. This information is useful for determining
      the input matches to the first Negation node.
    *);
  before_filter_matches: core_match list option
    (**
      Only present in nodes which have children Filter nodes. This
      information is useful for determining the input matches to the first
      Filter node, as there is otherwise no way of obtaining the
      post-intersection matches in an And node, for instance
    *)
}

(** EXPERIMENTAL *)
type matching_explanation = {
  op: matching_operation;
  children: matching_explanation list;
  matches: core_match list
    (** result matches at this node (can be empty when we reach a nomatch) *);
  loc: location
    (**
      location in the rule file! not target file. This tries to delimit the
      part of the rule relevant to the current operation (e.g., the position
      of the 'patterns:' token in the rule for the And operation).
    *);
  extra: matching_explanation_extra option (** NEW: since v1.79 *)
}

(**
  These ratios are numbers in \[0, 1\], and we would hope that both
  'time_ratio' and 'count_ratio' are very close to 0. In bad cases, we may
  see the 'count_ratio' being close to 0 while the 'time_ratio' is above 0.5,
  meaning that a small number of very slow files/etc represent a large amount
  of the total processing time. EXPERIMENTAL
*)
type very_slow_stats = {
  time_ratio: float (** Ratio "sum of very slow time" / "total time" *);
  count_ratio: float (** Ratio "very slow count" / "total count" *)
}

(** e.g., '1.1.0' *)
type version = string [@@deriving show]

type uuid = ATD_string_wrap.Uuidm.t [@@deriving ord]

type uri = ATD_string_wrap.Uri.t [@@deriving ord]

(** A symbol is a FQN. *)
type symbol = { fqn: string list }
  [@@deriving show]

(**
  We store the location of the usage, because we may want to be able to know
  how many uses of the symbol there are, and where.
*)
type symbol_usage = { symbol: symbol; locs: location list }
  [@@deriving show]

type symbol_analysis = symbol_usage list [@@deriving show]

type upload_subproject_symbol_analysis_params = {
  token: string;
  scan_id: int;
  manifest: fpath option;
  lockfile: fpath option;
  symbol_analysis: symbol_analysis
}

type unresolved_reason = 
    UnresolvedFailed (** Resolution was attempted, but was unsuccessful. *)
  | UnresolvedSkipped
      (**
        Resolution was skipped because the dependency source was not relevant
        to the scanned targets.
      *)
  | UnresolvedUnsupported
      (**
        Resolution was skipped because the dependency source is not
        supported.
      *)
  | UnresolvedDisabled
      (**
        Resolution was not attempted because a required feature (such as
        local builds) was disabled.
      *)


(**
  A subproject defined by some kind of manifest file (e.g., pyproject.toml,
  package.json, ...). This may be at the root of the repo being scanned or
  may be some other folder. Used as the unit of analysis for supply chain.
*)
type subproject = {
  root_dir: fpath;
  ecosystem: ecosystem option
    (**
      This is used to match code files with subprojects. It is necessary to
      have it here, even before a subproject's dependencies are resolved, in
      order to decide whether a certain subproject must be resolved given the
      changes included in a certain diff scan. It can be None if this
      subproject is for a package manager whose ecosystem is not yet
      supported (i.e. one that is identified only for tracking purposes)
    *);
  dependency_source: dependency_source
    (**
      The dependency source is how we resolved the dependencies. This might
      be a lockfile/manifest pair (the only current one), but in the future
      it might also be dynamic resolution based on a manifest, an SBOM, or
      something else
    *)
}
  [@@deriving show]

(**
  JSON names are to maintain backwards compatibility with the python enum it
  is replacing. The P prefix (for parser) is to avoid ambiguity with similar
  construtor names in the manifest and ecosystem types.
*)
type sca_parser_name = 
    PGemfile_lock | PGo_mod | PGo_sum | PGradle_lockfile | PGradle_build
  | PJsondoc | PPipfile | PPnpm_lock | PPoetry_lock | PPyproject_toml
  | PRequirements | PYarn_1 | PYarn_2 | PPomtree | PCargo_parser
  | PComposer_lock | PPubspec_lock | PPackage_swift | PPodfile_lock
  | PPackage_resolved | PMix_lock

  [@@deriving show]

type resource_inaccessible = {
  command: string;
  registry_url: string option
    (**
      we attempt to parse out the actual registry URL that we tried to access
    *);
  message: string
    (** and just include the entire error message too, just in case *)
}
  [@@deriving show]

type resolution_cmd_failed = { command: string; message: string }
  [@@deriving show]

type resolution_error_kind = 
    UnsupportedManifest
  | MissingRequirement of string
  | ResolutionCmdFailed of resolution_cmd_failed
  | ParseDependenciesFailed of string
      (**
        when we produce some dependency list in lockfileless scanning (by
        talking to the package manager) but fail to parse it correctly
      *)
  | ScaParseError of sca_parser_name
      (**
        a lockfile parser failed since semgrep 1.109.0 (to replace
        dependency_parser_error)
      *)
  | ResourceInaccessible of resource_inaccessible
      (**
        unable to access private registry, likely due to missing credentials
      *)

  [@@deriving show]

(** used only from pysemgrep for now *)
type sca_resolution_error = {
  type_: resolution_error_kind;
  dependency_source_file: fpath
}

type dependency_parser_error = {
  path: fpath;
  parser: sca_parser_name;
  reason: string;
  line: int option
    (**
      Not using `position` because this type must be backwards compatible
      with the python class it is replacing.
    *);
  col: int option;
  text: string option
}

type sca_error = 
    SCAParse of dependency_parser_error
  | SCAResol of sca_resolution_error


type unresolved_subproject = {
  info: subproject;
  reason: unresolved_reason;
  errors: sca_error list
    (** this is set only when the reason is UnresolvedFailed *)
}

(**
  Instead of serving snippets here, we could just give the locations of the
  patterns and matches. For convenience when scripting with this in rule
  generation, we will just get the source text here.
*)
type snippet = { line: int; text: string }

type killing_parent_kind = [ `And | `Inside | `Negation | `Filter of string ]

(**
  a "killing parent" is a parent operator that could have killed the
  unexpected match along its way to being returned Intuitively, these are all
  the sites at which the rule could have removed the unexpected match, but
  didn't. Note that because of the order of operations, this technically
  means that in the following pattern:
  
{v
  all:
    - pattern: A
    - not: B
v}
  
  the [not] node is a "parent" of the [pattern] node, even though they are
  siblings in the actual tree. This is because the ranges of the [pattern]
  are input to the [not] node.
*)
type killing_parent = {
  killing_parent_kind: killing_parent_kind;
  snippet: snippet
}

type unexpected_no_match_diagnosis_kind = [
    `Never_matched
  | `Killed_by_nodes of killing_parent list
]

type unexpected_no_match_diagnosis = {
  line: int;
  kind: unexpected_no_match_diagnosis_kind
}

type originating_node_kind = [ `Focus | `Xpattern ]

type unexpected_match_diagnosis = {
  matched_text: snippet;
  originating_kind: originating_node_kind
    (**
      information about the originating pattern in the rule file. This is
      where the unexpected match came from.
    *);
  originating_text: snippet;
  killing_parents: killing_parent list
}

type triage_ignored = {
  triage_ignored_syntactic_ids: string list;
  triage_ignored_match_based_ids: string list
}

type transitive_finding = {
  m: core_match
    (**
      the important part is the sca_match in core_match_extra that we need to
      adjust and especially the sca_match_kind.
    *)
}

(**
  Information about a 3rd-party lib downloaded for Transitive Reachability.
  To accompany a found_dependency within the Semgrep CLI, passed back and
  forth from OCaml to Python via RPC. See also SCA_dependency.t in OCaml.
  
  Source paths is a list of paths to either folders containing source code or
  source code files. It is necessary to use a list here because package
  managers like pip may unpack a package into multiple folders.
*)
type downloaded_dependency = { source_paths: fpath list }

type resolved_dependency = (found_dependency * downloaded_dependency option)

type transitive_reachability_filter_params = {
  rules_path: fpath;
  findings: transitive_finding list;
  dependencies: resolved_dependency list;
  write_to_cache: bool
}

(** The "value" *)
type tr_cache_match_result = { matches: cli_match list }

(**
  We want essentially to cache semgrep computation on third party packages to
  quickly know (rule_id x package_version) -> sca_transitive_match_kind to
  avoid downloading and recomputing each time the same thing.
  
  The "key". The rule_id and resolved_url should form a valid key for our TR
  cache database table. Indeed, semgrep should always return the same result
  when using the same rule and same resolved_url package. The content at the
  URL should hopefully not change (we could md5sum it just in case) and the
  content of the rule_id should also not change (could md5sum it maybe too).
  
  I've added tr_version below just in case we want to invalidate past cached
  entries (e.g., the semgrep engine itself changed enough that some past
  cached results might be wrong and should be recomputed.
*)
type tr_cache_key = {
  rule_id: rule_id;
  rule_version: string
    (**
      this can be the checksum of the content of the rule (JSON or YAML form)
    *);
  engine_version: int
    (**
      does not have to match the Semgrep CLI version; can be bumped only when
      we think the match should be recomputed
    *);
  package_url: string
    (**
      e.g. http://some-website/hello-world.0.1.2.tgz like in found_dependency
      [resolved_url] field, but could be anything to describe a particular
      package. We could rely on https://github.com/package-url/purl-spec
    *);
  extra: string (** extra key just in case (e.g., "prod" vs "dev") *)
}
  [@@deriving show, eq]

(** Response by the backend the the POST /api/cli/tr_cache/lookup *)
type tr_query_cache_response = {
  cached: (tr_cache_key * tr_cache_match_result) list
}

(** Sent by the CLI to the POST /api/cli/tr_cache/lookup *)
type tr_query_cache_request = { entries: tr_cache_key list }

(** Sent by the CLI to the POST /api/cli/tr_cache *)
type tr_add_cache_request = {
  new_entries: (tr_cache_key * tr_cache_match_result) list
}

type todo = int

(**
  EXPERIMENTAL
  
  A "matching diagnosis" is a postprocessed interpretation of matching
  explanations, specific to a particular test-annotated target file.
  
  For instance, suppose we have the rule:
  
{v
  1 | all:
  2 | - pattern: foo(...)
  3 | - not: foo(goood)
v}
  
  and the following Python annotated target:
  
{v
  1 | # ruleid: my_rule
  2 | foo()
  3 | # ok: my_rule
  4 | foo(good)
v}
  
  We would get an unexpected match on line 4, which would fail the test
  assertion.
  
  By looking at the matching explanation, we can deduce that the match on
  line 4 must clearly have been introduced by the positive [foo(..)] pattern.
  The rule-writer probably meant to kill [foo(good)] with the negative
  [foo(goood)] pattern.
  
  This is essentially what matching diagnoses are -- using matching
  explanations to point out where the erroneous parts of the rule _may_ be.
  
  Note that this is a _may_, because an unexpected match could have been
  killed by the [foo(bad)], but if there were more negative patterns, it
  could have been killed elsewhere too. All we can do is point out places
  where the rule-writer _may_ have messed up.
  
  So in this case, we would expect an [unexpected_match_diagnosis] with the
  form:
  
{v
  \{ matched_text = \{ line = 4; text = "foo(bad)" \};
    originating_kind = Xpattern;
    originating_text = \{ line = 2; text = "- pattern: foo(...)" \};
    killing_parents = [
      \{ killing_parent_kind = Negation;
        snippet = \{ line = 3; text = "- not: foo(good)" \} \}
    ]
  \}
v}
*)
type matching_diagnosis = {
  target: fpath (** specifically, the test target *);
  unexpected_match_diagnoses: unexpected_match_diagnosis list;
  unexpected_no_match_diagnoses: unexpected_no_match_diagnosis list
}

type expected_reported = {
  expected_lines: int list;
  reported_lines: int list
}

type rule_result = {
  passed: bool;
  matches: (string * expected_reported) list
    (** (target filename, expected_reported) list *);
  errors: todo list;
  diagnosis: matching_diagnosis option (** NEW: since 1.79 *)
}

type fixtest_result = { passed: bool }

type config_error_reason =  UnparsableRule 

type config_error = { file: fpath; reason: config_error_reason }

type checks = {
  checks: (string * rule_result) list (** (rule ID, rule_result) list *)
}

type tests_result = {
  results: (string * checks) list (** (rule file, checks) list *);
  fixtest_results: (string * fixtest_result) list
    (** (target file, fixtest_result) list *);
  config_missing_tests: fpath list;
  config_missing_fixtests: fpath list;
  config_with_errors: config_error list
}

(** See Scan_CLI.ml on how to convert command-line options to this *)
type project_root = [
    `Filesystem of string (** path *)
  | `Git_remote of string (** URL *)
]
  [@@deriving show]

(**
  This type is similar to the type Find_targets.conf used by osemgrep.
  
  We could share the type but it would be slightly more complicated. This
  solution will be easier to undo when we're fully migrated to osemgrep.
  
  It encodes options derived from the pysemgrep command line. Upon receiving
  this record, semgrep-core will discover the target files like osemgrep
  does.
  
  See Find_targets.mli for the meaning of each field. See Scan_CLI.ml for the
  mapping between semgrep CLI and this type.
*)
type targeting_conf = {
  exclude: string list;
  include_: string list option;
  max_target_bytes: int;
  respect_gitignore: bool;
  respect_semgrepignore_files: bool;
  extra_gitignore_patterns_to_exclude_git_untracked_files: string list;
  semgrepignore_filename: string option;
  always_select_explicit_targets: bool;
  explicit_targets: string list
    (** This is a hash table in Find_targets.conf *);
  force_project_root: project_root option
    (**
      osemgrep-only option (is it still the case?) (see Git_project.ml and
      the force_root parameter)
    *);
  force_novcs_project: bool;
  exclude_minified_files: bool;
  baseline_commit: string option
}
  [@@deriving show]

type product = [
    `SAST (** a.k.a. Code *)
  | `SCA (** a.k.a. SSC *)
  | `Secrets
]
  [@@deriving eq, ord, show]

type ppath = Ppath.t [@@deriving show, eq]

(**
  Same as Fppath.t: a nice filesystem path + the path relative to the project
  root provided for pattern-based filtering purposes.
*)
type fppath = { fpath: fpath; ppath: ppath }
  [@@deriving show, eq]

type analyzer = Analyzer.t [@@deriving show]

(**
  A normal semgrep target, optionally with an associated \[lockfile\] The
  lockfile means: the code in this file has its dependencies specified by
  this lockfile We don't want to commit to a specific way of associating
  these in semgrep-core, so we leave it up to the caller (pysemgrep or
  osemgrep) to do it.
*)
type code_target = {
  path: fppath (** source file *);
  analyzer: analyzer
    (**
      Must be a valid target analyzer as defined in Analyzer.mli. examples:
      "ocaml", "python", but also "spacegrep" or "regexp".
    *);
  products: product list;
  dependency_source: dependency_source option
}
  [@@deriving show]

(**
  A target can either be a traditional code target (now with optional
  associated lockfile) or it can be a lockfile target, which will be used to
  generate lockfile-only findings. Currently *ALL TARGETS FROM PYSEMGREP ARE
  CODETARGETS*
*)
type target = [
    `CodeTarget of code_target
  | `DependencySourceTarget of dependency_source
]
  [@@deriving show]

type scanning_roots = {
  root_paths: fpath list;
  targeting_conf: targeting_conf
}
  [@@deriving show]

(**
  The same path can be present multiple times in targets below, with
  different languages each time, so a Python file can be both analyzed with
  Python rules, but also with generic/regexp rules.
  
  alt: we could have a list of languages instead in target above, but because
  of the way semgrep-core is designed (with its file_and_more type), you
  could have at most one PL language, and then possibly "generic" and
  "regexp".
*)
type targets = [
    `Scanning_roots of scanning_roots
      (** list of paths used to discover targets *)
  | `Targets of target list
      (** targets already discovered from the scanning roots by pysemgrep *)
]
  [@@deriving show]

type target_times = {
  path: fpath;
  num_bytes: int;
  match_times: float list
    (** each elt in the list refers to a rule in profile.rules *);
  parse_times: float list;
  run_time: float (** run time for all rules on target *)
}

(**
  A reason for skipping a target file or a pair (target, rule). Note that
  this type is also used in Report.ml hence the need for deriving show here.
  
  For consistency, please make sure all the JSON constructors use the same
  case rules (lowercase, underscores). This is hard to fix later! Please
  review your code carefully before committing to interface changes.
*)
type skip_reason = 
    Always_skipped
  | Semgrepignore_patterns_match
  | Cli_include_flags_do_not_match
  | Cli_exclude_flags_match
  | Exceeded_size_limit
  | Analysis_failed_parser_or_internal_error
  | Excluded_by_config
  | Wrong_language
  | Too_big
  | Minified
  | Binary
  | Irrelevant_rule
  | Too_many_matches
  | Gitignore_patterns_match
  | Dotfile
      (**
        since 1.40.0. They were always ignored, but not shown in the skip
        report
      *)
  | Nonexistent_file (** since 1.44.0 *)
  | Insufficient_permissions (** since 1.94.0 *)

  [@@deriving show]

(** coupling: ugly: with yield_json_objects() in target_manager.py *)
type skipped_target = {
  path: fpath;
  reason: skip_reason;
  details: string option
    (** since semgrep 1.39.0 (used to be return only by semgrep-core) *);
  rule_id: rule_id option
    (**
      If the 'rule_id' field is missing, the target is assumed to have been
      skipped for all the rules
    *)
}
  [@@deriving show]

type incompatible_rule = {
  rule_id: rule_id;
  this_version: version;
  min_version: version option;
  max_version: version option
}
  [@@deriving show]

type error_type = 
    LexicalError
      (**
        File parsing related errors; coupling: if you add a target parse
        error then metrics for cli need to be updated. See
        cli/src/semgrep/parsing_data.py.
      *)
  | ParseError (** a.k.a SyntaxError *)
  | OtherParseError
  | AstBuilderError
  | RuleParseError
      (**
        Pattern parsing related errors. There are more precise info about the
        error in Rule.invalid_rule_error_kind in Rule.ml.
      *)
  | SemgrepWarning (** generated in pysemgrep only *)
  | SemgrepError
  | InvalidRuleSchemaError
  | UnknownLanguageError
  | InvalidYaml
  | MatchingError (** internal error, e.g., NoTokenLocation *)
  | SemgrepMatchFound
  | TooManyMatches
  | FatalError (** missing file, OCaml errors, etc. *)
  | Timeout
  | OutOfMemory
  | FixpointTimeout (** since semgrep 1.132.0 *)
  | StackOverflow (** since semgrep 1.86.0 *)
  | TimeoutDuringInterfile
  | OutOfMemoryDuringInterfile
  | MissingPlugin (** since semgrep 1.40.0 *)
  | PatternParseError of string list
      (**
        the string list is the "YAML path" of the pattern, e.g. [\["rules";
        "1"; ...\]]
      *)
  | PartialParsing of location list
      (** list of skipped tokens. Since semgrep 0.97. *)
  | IncompatibleRule of incompatible_rule (** since semgrep 1.38.0 *)
  | PatternParseError0
      (**
        Those Xxx0 variants were introduced in semgrep 1.45.0, but actually
        they are here so that our backend can read the cli_error.type_ from
        old semgrep versions that were translating the PatternParseError _
        and IncompatibleRule _ above as a single string (instead of a list
        \["PatternParseError", ...\] now). There is no PartialParsing0
        because this was encoded as a ParseError instead.
      *)
  | IncompatibleRule0
  | DependencyResolutionError of resolution_error_kind
      (** since semgrep 1.94.0 *)

  [@@deriving show]

(**
  This is used to specify the severity of errors which happened during
  Semgrep execution (e.g., a parse error).
  
{v
  Error = Always an error
  Warning = Only an error if "strict" is set
  Info = Nothing may be wrong
v}
  
  alt: could reuse match_severity but seems cleaner to define its own type
*)
type error_severity = [ `Error | `Warning | `Info ]
  [@@deriving show, eq]

(** See Semgrep_error_code.ml *)
type core_error = {
  error_type: error_type;
  severity: error_severity;
  message: string;
  details: string option;
  location: location option;
  rule_id: rule_id option
}

(**
  Result of get_targets internal RPC, similar to scanned_and_skipped but more
  complete
*)
type target_discovery_result = {
  target_paths: fppath list;
  errors: core_error list;
  skipped: skipped_target list
}

(** EXPERIMENTAL *)
type summary_stats = { mean: float; std_dev: float }

(** EXPERIMENTAL *)
type def_rule_time = {
  fpath: fpath;
  fline: int;
  rule_id: rule_id;
  time: float
}
  [@@deriving show]

(** EXPERIMENTAL *)
type tainting_time = {
  total_time: float;
  per_def_and_rule_time: summary_stats;
  very_slow_stats: very_slow_stats;
  very_slow_rules_on_defs: def_rule_time list (** ascending order *)
}

(** e.g. "webapp" *)
type tag = string

type symbol_analysis_upload_response = {
  upload_url: uri (** Presigned AWS URL for uploading symbol analysis data *)
}

type symbol_analysis_params = {
  root_path: fpath;
  lang: string option;
  files: fpath list
}

type resolution_method = [
    `LockfileParsing
      (** we parsed a lockfile that was already included in the repository *)
  | `DynamicResolution
      (** we communicated with the package manager to resolve dependencies *)
  | `SbomParsing
      (**
        We parsed an SBOM separate from the dependency source files, either
        an ephemeral or a checked-in one.
      *)
]
  [@@deriving show]

type dependency_source_file_kind = [
    `Lockfile of lockfile_kind
  | `Manifest of manifest_kind
]
  [@@deriving show]

type dependency_source_file = {
  kind: dependency_source_file_kind;
  path: fpath
}

type dependency_resolution_stats = {
  resolution_method: resolution_method;
  dependency_count: int;
  ecosystem: ecosystem
}

type subproject_stats = {
  subproject_id: string
    (**
      The [subproject_id] is derived as a stable hash of the sorted paths of
      [dependency_source_file]s. Any change to the set of dependency sources
      (addition, removal, or modification) results in a new [subproject_id],
      as different dependency sources indicate a different subproject
      context.
    *);
  dependency_sources: dependency_source_file list
    (**
      Files used to determine the subproject's dependencies (lockfiles,
      manifest files, etc
    *);
  resolved_stats: dependency_resolution_stats option
    (** Results of dependency resolution, empty if resolution failed *);
  unresolved_reason: unresolved_reason option
    (** Reason why resolution failed, empty if resolution succeeded *);
  errors: sca_error list
    (** Errors encountered during subproject resolution *)
}

type supply_chain_stats = { subprojects_stats: subproject_stats list }

(**
  Sent by the CLI to the POST
  /api/agent/scans/\{scan_id\}/subproject_symbols_upload_url/
*)
type subproject_symbol_analysis_url_request = {
  manifest_path: fpath option;
  lockfile_path: fpath option
}

type single_subproject_plan = {
  subproject_id: string;
  root_dir: fpath;
  resolution_planned: bool
}

(** Subproject dump output. Experimental and for internal use only. *)
type subproject_resolution_plan = {
  subprojects: single_subproject_plan list
}

type skipped_rule = {
  rule_id: rule_id;
  details: string;
  position: position (** position of the error in the rule file *)
}

(** EXPERIMENTAL *)
type file_time = { fpath: fpath; ftime: float }
  [@@deriving show]

(** Scanning time (includes matching and tainting) EXPERIMENTAL *)
type scanning_time = {
  total_time: float;
  per_file_time: summary_stats;
  very_slow_stats: very_slow_stats;
  very_slow_files: file_time list (** ascending order *)
}

type scanned_and_skipped = {
  scanned: fpath list;
  skipped: skipped_target list option
}

(** meta info about the scan *)
type scan_info = {
  id: int option (** the scan id, null for dry-runs *);
  enabled_products: product list;
  deployment_id: int;
  deployment_name: string
}

(** config specific to the scan *)
type scan_configuration = {
  rules: raw_json;
  triage_ignored_syntactic_ids: string list;
  triage_ignored_match_based_ids: string list;
  project_merge_base: sha1 option
    (** From 1.131.0, tells us what merge base to use if it's a diff scan *);
  fips_mode: bool
    (**
      From 1.126.0. Customers in FIPS environments have specific hash
      function requirements that this flag will override. See SAF-2057 for
      details.
    *)
}

type glob = string

type product_ignored_files = (product * glob list) list

(**
  configuration for scanning version control history, e.g., looking back at
  past git commits for committed credentials which may have been removed
*)
type historical_configuration = { enabled: bool; lookback_days: int option }

(** settings for the cli *)
type engine_configuration = {
  autofix: bool;
  deepsemgrep: bool;
  dependency_query: bool;
  path_to_transitivity: bool (** a.k.a. dependency path *);
  scan_all_deps_in_diff_scan: bool
    (**
      normally we resolve dependencies for changed subprojects only in diff
      scans. This flag causes all subprojects to be resolved in diff scans
    *);
  symbol_analysis: bool
    (**
      Whether to collect "symbol analysis" info from the repo being scanned
      See
      https://www.notion.so/semgrep/Semgrep-Code-Reconnaissance-Toolbox-18a3009241a880f2a439eed6b2cffe66?pvs=4
    *);
  transitive_reachability_enabled: bool
    (**
      Whether to enable transitive reachability analysis for SCA findings
    *);
  ignored_files: string list;
  product_ignored_files: product_ignored_files option (** from 1.71.0 *);
  generic_slow_rollout: bool
    (** for features we only want to turn on for select customers *);
  historical_config: historical_configuration option (** from 1.63.0 *);
  always_suppress_errors: bool
    (**
      from 1.93. Indicate that fail-open should always be enabled, overriding
      the CLI flag.
    *)
}

(**
  Response from the backend to the CLI to the (deprecated) POST
  /api/cli/scans
*)
type scan_response = {
  info: scan_info;
  config: scan_configuration;
  engine_params: engine_configuration
}

(** Scan metadata generated by the CLI during the scan process. *)
type scan_metadata = {
  cli_version: version;
  unique_id: uuid (** client generated uuid for the scan *);
  requested_products: product list;
  dry_run: bool (** since 1.47.0 *);
  sms_scan_id: string option
    (**
      unique id associated with the scan in Semgrep Managed Scanning. Since
      1.96.0
    *);
  ecosystems: string list;
  packages: string list;
  enable_mal_deps: bool option
    (**
      Override to enable malicious dependency rules for this scan, even if
      disabled at the deployment level.
    *)
}

(**
  Collect information about a project from the environment, filesystem, git
  repo, etc. See also semgrep_metrics.atd and PRIVACY.md
*)
type project_metadata = {
  scan_environment: string (** TODO: use enum with [<json name="...">] *);
  repository: string;
  repo_url: uri option;
  repo_id: string option
    (**
      The two fields below are stable across repository renaming and even org
      renaming, which can be useful to not report new findings on a repo just
      because this repo was renamed. Since Semgrep 1.46.0. The string is
      usually an int, but more general to use a string.
    *);
  org_id: string option (** a.k.a repository owner id *);
  repo_display_name: string option
    (**
      Users can set a different name for display and for PR comments. This
      allows monorepos to be scanned as separate projects.
    *);
  branch: string option;
  commit: sha1 option;
  commit_title: string option;
  commit_timestamp: datetime option (** since 1.38.0 *);
  commit_author_email: string option;
  commit_author_name: string option;
  commit_author_username: string option;
  commit_author_image_url: uri option;
  ci_job_url: uri option;
  on: string
    (**
      CI event name
      ("pull_request"|"pull_request_target"|"push"|"unknown"|...)
      
      TODO: use enum
    *);
  pull_request_author_username: string option;
  pull_request_author_image_url: uri option;
  pull_request_id: string option;
  pull_request_title: string option;
  base_branch_head_commit: sha1 option
    (**
      the latest commit in the base branch of a PR, used to determine the git
      merge base on the app side if needed. This should really be called
      base_sha but that term is already misused below for something that's
      gitlab only
    *);
  base_sha: sha1 option
    (**
      This is gitlab only, and is actually only the baseline commit sha if
      provided, OR it's the git merge-base if not provided. It is NOT the
      head commit of the base branch
    *);
  start_sha: sha1 option
    (**
      this is CI_MERGE_REQUEST_DIFF_BASE_SHA which is strictly the git merge
      base
    *);
  is_full_scan: bool
    (**
      Check if the current Git repository has enough to determine the
      merge_base_ref.
    *);
  is_sca_scan: bool option (** added later in ci.py (not from meta.py) *);
  is_code_scan: bool option (** since 1.40.0 *);
  is_secrets_scan: bool option (** since 1.41.0 *);
  project_id: string option
    (** Identifies a semgrep project where findings belong to. *)
}

(**
  Content of a possible .semgrepconfig.yml in the repository.
  
  This config allows to configure Semgrep per repo, e.g., to store a
  category/tag like "webapp" in a repo so that the Semgrep WebApp can return
  a set of relevant rules automatically for this repo in scan_config later
  when given this ci_config_from_repo in the scan_request.
*)
type ci_config_from_repo = {
  version: version
    (** version of the .semgrepconfig.yml format. "v1" right now (useful?) *);
  tags: tag list option
}

(**
  Sent by the CLI to the (deprecated) POST /api/cli/scans to create a scan.
*)
type scan_request = {
  project_metadata: project_metadata;
  scan_metadata: scan_metadata;
  project_config: ci_config_from_repo option
}

type ci_env = (string * string) list

(**
  Note that we should use very simple types below for the configuration of
  Semgrep: booleans or small enums. No int, as people often don't understand
  how to set values. For example even if we documented very well the
  --timeout option in Semgrep, people still didn't know which value to use.
*)
type ci_config = {
  env: ci_env
    (**
      to override environment variables, as lots of the configuration of
      'semgrep ci' comes from environment variables (e.g., SEMGREP_REPO_URL)
    *);
  enabled_products: product list;
  ignored_files: string list (** glob patterns *);
  autofix: bool;
  deepsemgrep: bool;
  dependency_query: bool;
  path_to_transitivity: bool (** a.k.a. dependency path *);
  scan_all_deps_in_diff_scan: bool
    (**
      normally we resolve dependencies for changed subprojects only in diff
      scans. This flag causes all subprojects to be resolved in diff scans
    *);
  symbol_analysis: bool
    (**
      Whether to collect "symbol analysis" info from the repo being scanned
      See
      https://www.notion.so/semgrep/Semgrep-Code-Reconnaissance-Toolbox-18a3009241a880f2a439eed6b2cffe66?pvs=4
    *);
  transitive_reachability_enabled: bool
    (**
      Whether to enable transitive reachability analysis for SCA findings
    *)
}

(**
  The actions below allow the WebApp to modify the behavior of the CLI
  dynamically, which is especially useful for old versions of the CLI (e.g.,
  insist on the deprecation of an old version of the CLI). The action below
  will be executed by the CLI just after receiving the scan configuration.
  It's a bit similar to injecting code dynamically, except the possible
  actions are clearly delimited here (this is not eval()).
  
  Note that the version of the CLI is sent to the WebApp in project_metadata
  so the backend has all the necessary information to send back an
  appropriate action depending on the CLI version.
*)
type action = [
    `Message of string
  | `Delay of float (** in seconds *)
  | `Exit of int (** process exit code *)
]

(** Semgrep config from the WebApp *)
type ci_config_from_cloud = {
  repo_config: ci_config;
  org_config: ci_config option;
  dirs_config: (fpath * ci_config) list option
    (** for monorepos, to be "monorepo-friendly" like they say in Ruff *);
  actions: action list
}

(**
  Response by the backend to the CLI to the POST deployments/scans/config The
  record is similar to scan_response.
*)
type scan_config = {
  deployment_id: int;
  deployment_name: string;
  policy_names: string list (** e.g. "audit", "comment", "block" *);
  rule_config: string
    (** rules raw content in JSON format (but still sent as a string) *);
  autofix: bool;
  deepsemgrep: bool;
  dependency_query: bool;
  path_to_transitivity: bool (** a.k.a. dependency path *);
  scan_all_deps_in_diff_scan: bool
    (**
      normally we resolve dependencies for changed subprojects only in diff
      scans. This flag causes all subprojects to be resolved in diff scans
    *);
  symbol_analysis: bool
    (**
      Whether to collect "symbol analysis" info from the repo being scanned
      See
      https://www.notion.so/semgrep/Semgrep-Code-Reconnaissance-Toolbox-18a3009241a880f2a439eed6b2cffe66?pvs=4
    *);
  transitive_reachability_enabled: bool
    (**
      Whether to enable transitive reachability analysis for SCA findings
    *);
  triage_ignored_syntactic_ids: string list;
  triage_ignored_match_based_ids: string list;
  ignored_files: string list (** glob patterns *);
  enabled_products: product list option (** since 1.37.0 *);
  actions: action list (** since 1.64.0 *);
  ci_config_from_cloud: ci_config_from_cloud option
    (** since 1.47.0 but not created by the backend (nor used by the CLI) *)
}

type sarif_format = {
  rules: fpath
    (**
      Path to the rules file. We need it because rules can't be reconstructed
      from cli_output (which is one of the other param of CallSarifFormat)
    *);
  is_pro: bool;
  show_dataflow_traces: bool
}

type engine_kind = [ `OSS | `PRO ] [@@deriving ord, show]

type rule_id_and_engine_kind = (rule_id * engine_kind)

type resolve_dependencies_params = {
  dependency_sources: dependency_source list;
  download_dependency_source_code: bool;
  allow_local_builds: bool
    (** whether to allow executing package manager commands *);
  package_manager_env: (string * string) list option
    (**
      extra environment variables to pass to package manager subprocesses
    *)
}

(**
  Profiling info obtained from the OCaml executable, to be aggregated further
  in pysemgrep.
*)
type profiling_entry = {
  name: string
    (**
      The name given to piece of code for which we measured how long it took.
    *);
  total_time: float
    (** Total clock time in seconds. Divide by the count to get the mean. *);
  count: int
}

type prefiltering_stats = {
  project_level_time: float
    (** The time (seconds) it took to execute project-level prefilters *);
  file_level_time: float
    (** The time (seconds) it took to execute file-level prefilters *);
  rules_with_project_prefilters_ratio: float
    (**
      The ratio of rules which the engine generated a project-level prefilter
      for
    *);
  rules_with_file_prefilters_ratio: float
    (**
      The ratio of rules which the engine generated a file-level prefilter
      for
    *);
  rules_selected_ratio: float
    (**
      The ratio of rules which executed beyond prefiltering on at least one
      target
    *);
  rules_matched_ratio: float
    (** The ratio of rules which generated at least one match *)
}
  [@@deriving show]

(** EXPERIMENTAL *)
type parsing_time = {
  total_time: float;
  per_file_time: summary_stats;
  very_slow_stats: very_slow_stats option;
  very_slow_files: file_time list (** ascending order *)
}

(** EXPERIMENTAL *)
type file_rule_time = { fpath: fpath; rule_id: rule_id; time: float }
  [@@deriving show]

(** EXPERIMENTAL *)
type matching_time = {
  total_time: float;
  per_file_and_rule_time: summary_stats;
  very_slow_stats: very_slow_stats;
  very_slow_rules_on_files: file_rule_time list (** ascending order *)
}

(** Run locally $ ./run-benchmarks --dummy --upload *)
type profile = {
  rules: rule_id list
    (**
      List of rules, including the one read but not run on any target. This
      list is actually more an array which allows other fields to reference
      rule by number instead of rule_id (e.g., match_times further below)
      saving space in the JSON.
      
      Upgrade note: this used to be defined as a rule_id_dict where each
      rule_id was inside a \{id: rule_id; ...\} record so we could give
      parsing time info about each rule, but parsing one rule was never the
      slow part, so now we just juse the aggregated rules_parse_time below
      and do not need a complex rule_id_dict record anymore.
    *);
  rules_parse_time: float;
  profiling_times: (string * float) list;
  parsing_time: parsing_time option (** EXPERIMENTAL *);
  scanning_time: scanning_time option (** EXPERIMENTAL *);
  matching_time: matching_time option (** EXPERIMENTAL *);
  tainting_time: tainting_time option (** EXPERIMENTAL *);
  fixpoint_timeouts: core_error list option
    (**
      EXPERIMENTAL: Dafatlow fixpoint-function timeouts
      
      Happen more often than we would like, and it's mainly Semgrep devs that
      will use this info for debugging, so for now we are reporting these
      timeouts as part of the profiling report.
    *);
  prefiltering: prefiltering_stats option;
  targets: target_times list;
  total_bytes: int;
  max_memory_bytes: int option
    (**
      maximum amount of memory used by Semgrep(-core) during its execution
    *)
}

type output_format = 
    Text
  | Json
  | Emacs
  | Vim
  | Sarif
  | Gitlab_sast
  | Gitlab_secrets
  | Junit_xml
  | Files_with_matches (** osemgrep-only *)
  | Incremental
      (**
        used to disable the final display of match results because we
        displayed them incrementally instead
      *)

  [@@deriving show]

type mcp_scan_results = { rules: string list; total_bytes_scanned: int }

type format_context = {
  is_ci_invocation: bool;
  is_logged_in: bool;
  is_using_registry: bool
}
  [@@deriving show]

type error_span = {
  file: fpath (** for InvalidRuleSchemaError *);
  start: position;
  end_ (*atd end *): position;
  source_hash: string option;
  config_start: position option option
    (**
      The path to the pattern in the yaml rule and an adjusted start/end
      within just the pattern. Used to report playground parse errors in the
      simple editor
    *);
  config_end: position option option;
  config_path: string list option option;
  context_start: position option option;
  context_end: position option option
}

type edit = {
  path: fpath;
  start_offset: int;
  end_offset: int;
  replacement_text: string
}

type dump_rule_partitions_params = {
  rules: raw_json;
  n_partitions: int;
  output_dir: fpath;
  strategy: string option
}

(**
  This is the public version of subproject_stats, which is used in the CLI
  output. This is distinguised from subproject_stats below in order to
  produce more normal-looking JSON and to avoid including unnecessary fields.
*)
type cli_output_subproject_info = {
  dependency_sources: fpath list
    (**
      We use fpath here rather than the dependency_source_file type because
      ATD makes strange-looking JSON output for the dependency_source_file
      type.
    *);
  resolved: bool
    (** true if the subproject's dependencies were resolved successfully *);
  unresolved_reason: unresolved_reason option
    (** Reason why resolution failed, empty if resolution succeeded *);
  resolved_stats: dependency_resolution_stats option
    (** Results of dependency resolution, empty if resolution failed *)
}

(** (called SemgrepError in error.py) *)
type cli_error = {
  code: int (** exit code for the type_ of error *);
  level: error_severity;
  type_: error_type
    (**
      before 1.45.0 the type below was 'string', but was the result of
      converting error_type into a string, so using directly 'error_type'
      below should be mostly backward compatible thx to the <json name>
      annotations in error_type. To be fully backward compatible, we actually
      introduced the PatternParseError0 and IncompatibleRule0 cases in
      error_type.
    *);
  rule_id: rule_id option;
  message: string option (** contains error location *);
  path: fpath option;
  long_msg: string option (** for invalid rules, for ErrorWithSpan *);
  short_msg: string option;
  spans: error_span list option;
  help: string option
}

type cli_output = {
  version: version option (** since: 0.92 *);
  results: cli_match list;
  errors: cli_error list;
  paths: scanned_and_skipped (** targeting information *);
  time: profile option (** profiling information *);
  explanations: matching_explanation list option
    (**
      debugging (rule writing) information. Note that as opposed to the
      dataflow trace, the explanations are not embedded inside a match
      because we give also explanations when things are not matching.
      EXPERIMENTAL: since semgrep 0.109
    *);
  rules_by_engine: rule_id_and_engine_kind list option
    (**
      These rules, classified by engine used, will let us be transparent in
      the CLI output over what rules were run with what. EXPERIMENTAL: since:
      1.11.0
    *);
  engine_requested: engine_kind option;
  interfile_languages_used: string list option
    (**
      Reporting just the requested engine isn't granular enough. We want to
      know what languages had rules that invoked interfile. This is
      particularly important for tracking the performance impact of new
      interfile languages EXPERIMENTAL: since 1.49.0
    *);
  skipped_rules: skipped_rule list (** EXPERIMENTAL: since: 1.37.0 *);
  subprojects: cli_output_subproject_info list option
    (**
      SCA subproject resolution results. Note: this is only available when
      logged in. EXPERIMENTAL: since: 1.125.0
    *);
  mcp_scan_results: mcp_scan_results option (** MCP scan results. *);
  profiling_results: profiling_entry list
    (**
      How long it took to execute this or that piece of code in semgrep-core
    *)
}

type apply_fixes_params = { dryrun: bool; edits: edit list }

type function_call = [
    `CallContributions
  | `CallApplyFixes of apply_fixes_params
  | `CallFormatter of (output_format * format_context * cli_output)
  | `CallSarifFormat of (sarif_format * format_context * cli_output)
  | `CallValidate of fpath
      (**
        NOTE: fpath is most likely a temporary file that contains all the
        rules in JSON format. In the future, we could send the rules via a
        big string through the RPC pipe.
      *)
  | `CallResolveDependencies of resolve_dependencies_params
  | `CallUploadSymbolAnalysis of (string * int * symbol_analysis)
  | `CallDumpRulePartitions of dump_rule_partitions_params
  | `CallGetTargets of scanning_roots
      (**
        For now, the transitive reachability filter takes only a single
        dependency graph as input. It is up to the caller to call it several
        times, one for each subproject.
      *)
  | `CallTransitiveReachabilityFilter
      of transitive_reachability_filter_params
  | `CallMatchSubprojects of fpath list
  | `CallRunSymbolAnalysis of symbol_analysis_params
  | `CallUploadSubprojectSymbolAnalysis
      of upload_subproject_symbol_analysis_params
  | `CallShowSubprojects of subproject list
      (**
        Format human-readable text summarizing the subprojects that were
        discovered in a project. This is meant to be printed in --verbose
        mode.
      *)
]

type rpc_call = { call: function_call; parent_span_id: string option }

(** A subproject plus its resolved set of dependencies *)
type resolved_subproject = {
  info: subproject;
  resolution_method: resolution_method
    (**
      The resolution method is how we determined the dependencies from the
      dependency source. This might be lockfile parsing, dependency
      resolution, SBOM ingest, or something else.
    *);
  ecosystem: ecosystem
    (** should be similar to info.ecosystem but this time it can't be None *);
  resolved_dependencies: (dependency_child * resolved_dependency list) list
    (**
      We use this mapping to efficiently find child dependencies from a
      FoundDependency. We need to store multiple FoundDependencies per
      package/version pair because a package might come from multiple places
      in a lockfile
    *);
  errors: sca_error list
}

(**
  Resolution can either succeed or fail, but in either case errors can be
  produced (e.g. one resolution method might fail while a worse one succeeds,
  lockfile parsing might partially fail but recover and still produce
  results).
  
  Resolution can optionally include a [downloaded_dependency] alongside each
  [found_dependency]. This should be included if the source code for the
  dependency was downloaded and is available to scan later.
*)
type resolution_result = [
    `ResolutionOk of (resolved_dependency list * resolution_error_kind list)
  | `ResolutionError of resolution_error_kind list
]

(** Recommendations for subsequent requests *)
type polling_information = {
  recommended_wait_seconds: int;
  seconds_until_timeout: int
}

type parsing_stats = {
  targets_parsed: int;
  num_targets: int;
  bytes_parsed: int;
  num_bytes: int
}

(**
  The goal is to hash findings independently of their precise location so if
  a file is moved around or the line numbers change in a file, we do not
  report new findings but instead detect that the finding actually hashes to
  a previous old finding. See also match_based_id which is yet another way to
  hash a finding. See also
  https://www.notion.so/semgrep/Identifying-unique-findings-match_based_id-and-syntactic_id
*)
type finding_hashes = {
  start_line_hash: string;
  end_line_hash: string;
  code_hash: string
    (**
      hash of the syntactic_context/code contents from start_line through
      end_line
    *);
  pattern_hash: string
    (** hash of the rule pattern with metavariables substituted in *)
}

type finding = {
  check_id: rule_id;
  path: fpath;
  line: int;
  column: int;
  end_line: int;
  end_column: int;
  message: string;
  severity: Yojson.Safe.t
    (**
      int|string until minimum version exceeds 1.32.0. After 1.32.0 we're
      always using an int.
    *);
  index: int;
  commit_date: string;
  syntactic_id: string;
  match_based_id: string option
    (** since semgrep 0.98 TODO: use match_based_id option *);
  hashes: finding_hashes option (** since semgrep 1.14.0 *);
  metadata: raw_json (** metadata from the rule *);
  is_blocking: bool;
  fixed_lines: string list option;
  sca_info: sca_match option;
  dataflow_trace: match_dataflow_trace option
    (** Note that this contains code! *);
  validation_state: validation_state option
    (** Added in semgrep 1.39.0 see comments in cli_match_extra *);
  historical_info: historical_info option
    (** Added in semgrep 1.65.0 see comments in cli_match_extra *);
  engine_kind: engine_of_finding option (** Added in semgrep 1.70.0 *)
}

(** See https://semgrep.dev/docs/usage-limits *)
type contributor = {
  commit_author_name: string;
  commit_author_email: string
}

type contribution = {
  commit_hash: string;
  commit_timestamp: datetime;
  contributor: contributor
}

(**
  We keep this alias because we need to generate code to parse and write list
  of contributions.
*)
type contributions = contribution list

(**
  Scan metadata populated by the backend after receiving the scan results
  from the CLI via POST request to /scans/<int:scan_id>/results
*)
type ci_scan_metadata = {
  scan_id: int;
  deployment_id: int;
  repository_id: int (** stored as int in our app db *);
  repository_ref_id: int (** stored id for a branch or tag *);
  enabled_products: product list;
  git_commit: sha1 option;
  git_ref: string option
}

type ci_scan_dependencies = (string * found_dependency list) list

type ci_scan_results = {
  findings: finding list;
  ignores: finding list;
  token: string option;
  searched_paths: fpath list
    (**
      Files that were detected and attempted to scan. Note that some of these
      may have been skipped due to errors (see skipped_paths).
    *);
  renamed_paths: fpath list;
  skipped_paths: fpath list
    (**
      Files detected but not scanned due to errors (timeout, OOM, etc.). The
      app should NOT mark findings in these files as fixed.
    *);
  rule_ids: rule_id list;
  contributions: contributions option (** since semgrep 1.34.0 *);
  dependencies: ci_scan_dependencies option
    (**
      since semgrep 1.38.0. This data was originally sent to /complete, but
      we want to start sending it to /results
    *);
  metadata: ci_scan_metadata option
    (**
      filled in by the backend to associate scan results with the driving
      scan
    *)
}

(** Sent by the CLI to /scans/<scan_id>/error *)
type ci_scan_failure = { exit_code: int; stderr: string }

type ci_scan_complete_stats = {
  findings: int;
  errors: cli_error list;
  total_time: float;
  unsupported_exts: (string * int) list;
  lockfile_scan_info: (string * int) list;
  parse_rate: (string * parsing_stats) list;
  engine_requested: string option
    (**
      This is EngineType from python, which is different from engine_kind
      used in this file.
    *);
  findings_by_product: (string * int) list option
    (**
      Mirrors numFindingsByProduct in metrics.py See PA-3312 and GROW-104.
      
      NOTE: As of 1.56.0 the string used as the mapping key is currently a
      human-readable product name (i.e. code) vs our typed product enum
      representation (i.e. sast).
    *);
  supply_chain_stats: supply_chain_stats option
    (**
      since 1.98.0.
      
      In collaboration with the Data Science team, it was suggested that we
      start to group stats by product for organizational purposes.
      
      This field will only be defined for SCA scans.
    *)
}

(** Sent by the CLI to /complete *)
type ci_scan_complete = {
  exit_code: int;
  stats: ci_scan_complete_stats;
  dependencies: ci_scan_dependencies option;
  dependency_parser_errors: dependency_parser_error list option;
  task_id: string option (** since 1.31.0 *);
  final_attempt: bool option
}

(** Partial scans. Experimental and for internal use only. *)
type partial_scan_result = [
    `PartialScanOk of (ci_scan_results * ci_scan_complete)
  | `PartialScanError of ci_scan_failure
]

(** e.g. "ab023_1" *)
type match_based_id = string [@@deriving show, eq]

(** whether a certain feature is available for a deployment *)
type has_features = {
  has_autofix: bool;
  has_deepsemgrep: bool;
  has_triage_via_comment: bool;
  has_dependency_query: bool
}

type get_config_response_status =  Pending | Success | Failure 

(**
  Response from the backend to the CLI for GET
  /api/cli/v2/scans/<scan_request_id>/config.
*)
type get_config_response_v2 = {
  status: get_config_response_status;
  polling: polling_information option;
  config: scan_configuration option;
  engine_params: engine_configuration option
}

type apply_fixes_return = {
  modified_file_count: int (** Number of files modified *);
  fixed_lines: (int * string list) list
    (**
      Each item is a pair, where the first item is the index of the
      associated edit in the input list and the second item is the list of
      fixed lines associated with that edit.
    *)
}

type function_return = [
    `RetError of string
  | `RetApplyFixes of apply_fixes_return
  | `RetContributions of contributions
  | `RetFormatter of string
  | `RetSarifFormat of string
  | `RetValidate of core_error option
      (** rule validation error, if validation failed *)
  | `RetResolveDependencies of (dependency_source * resolution_result) list
  | `RetUploadSymbolAnalysis of string (** success msg *)
  | `RetDumpRulePartitions of bool
  | `RetTransitiveReachabilityFilter of transitive_finding list
  | `RetGetTargets of target_discovery_result
  | `RetMatchSubprojects of subproject list
  | `RetRunSymbolAnalysis of symbol_analysis
  | `RetUploadSubprojectSymbolAnalysis of string (** success msg *)
  | `RetShowSubprojects of string
      (**
        The text return here typically contains newlines but is not
        newline-terminated i.e. it is suitable to pass as an argument to a
        logger.
      *)
]

type function_result = {
  function_return: function_return;
  profiling_results: profiling_entry list
}

type features = {
  autofix: bool;
  deepsemgrep: bool;
  dependency_query: bool;
  path_to_transitivity: bool (** a.k.a. dependency path *);
  scan_all_deps_in_diff_scan: bool
    (**
      normally we resolve dependencies for changed subprojects only in diff
      scans. This flag causes all subprojects to be resolved in diff scans
    *);
  symbol_analysis: bool
    (**
      Whether to collect "symbol analysis" info from the repo being scanned
      See
      https://www.notion.so/semgrep/Semgrep-Code-Reconnaissance-Toolbox-18a3009241a880f2a439eed6b2cffe66?pvs=4
    *);
  transitive_reachability_enabled: bool
    (**
      Whether to enable transitive reachability analysis for SCA findings
    *)
}

(**
  Synthesizing from diffs (see locate_patched_functions in Synthesizing.mli).
  Was in Input_to_core.atd before.
*)
type diff_file = {
  filename: fpath;
  diffs: string list (** start_line-end_line *);
  url: string (** metadata to help SCA rule generation *)
}
  [@@deriving show]

type diff_files = { cve_diffs: diff_file list } [@@deriving show]

(**
  Response by the backend to the CLI to the POST
  api/agent/deployments/current. Some of the information in deployment_config
  is now returned directly in scan_response (e.g., the deployment_name)
*)
type deployment_config = {
  id: int;
  name: string
    (** the important piece, the deployment name (e.g., "returntocorp") *);
  organization_id: int;
  display_name: string
    (**
      All three below seem similar to 'name' mostly (e.g., "returntocorp")
    *);
  scm_name: string;
  slug: string;
  source_type: string (** e.g. "github" *);
  default_user_role: string (** e.g. "member" *);
  has_autofix: bool;
  has_deepsemgrep: bool;
  has_triage_via_comment: bool;
  has_dependency_query: bool
}
  [@@deriving show]

type deployment_response = { deployment: deployment_config }

(** Response from the backend to the CLI for POST /api/cli/v2/scans. *)
type create_scan_response_v2 = { info: scan_info }

(** Sent by the CLI to the backend in POST /api/cli/v2/scans. *)
type create_scan_request_v2 = {
  project_metadata: project_metadata;
  scan_metadata: scan_metadata;
  project_config: ci_config_from_repo option
}

(**
  For extra information to put into the `core_output` that we do not
  necessarily want to share with the cli_output.
*)
type core_output_extra = {
  symbol_analysis: symbol_analysis option (** since semgrep 1.108.0 *)
}

type core_output = {
  version: version;
  results: core_match list;
  errors: core_error list
    (** errors are guaranteed to be duplicate free; see also Report.ml *);
  paths: scanned_and_skipped (** targeting information *);
  time: profile option (** profiling information *);
  explanations: matching_explanation list option
    (**
      debugging (rule writing) information. Note that as opposed to the
      dataflow trace, the explanations are not embedded inside a match
      because we give also explanations when things are not matching.
      EXPERIMENTAL: since semgrep 0.109
    *);
  rules_by_engine: rule_id_and_engine_kind list option
    (**
      These rules, classified by engine used, will let us be transparent in
      the CLI output over what rules were run with what. EXPERIMENTAL: since:
      1.11.0
    *);
  engine_requested: engine_kind option;
  interfile_languages_used: string list option
    (**
      Reporting just the requested engine isn't granular enough. We want to
      know what languages had rules that invoked interfile. This is
      particularly important for tracking the performance impact of new
      interfile languages EXPERIMENTAL: since 1.49.0
    *);
  skipped_rules: skipped_rule list (** EXPERIMENTAL: since: 1.37.0 *);
  subprojects: cli_output_subproject_info list option
    (**
      SCA subproject resolution results. Note: this is only available when
      logged in. EXPERIMENTAL: since: 1.125.0
    *);
  mcp_scan_results: mcp_scan_results option (** MCP scan results. *);
  profiling_results: profiling_entry list
    (**
      How long it took to execute this or that piece of code in semgrep-core
    *);
  symbol_analysis: symbol_analysis option (** since semgrep 1.108.0 *)
}

type cli_output_extra = {
  paths: scanned_and_skipped (** targeting information *);
  time: profile option (** profiling information *);
  explanations: matching_explanation list option
    (**
      debugging (rule writing) information. Note that as opposed to the
      dataflow trace, the explanations are not embedded inside a match
      because we give also explanations when things are not matching.
      EXPERIMENTAL: since semgrep 0.109
    *);
  rules_by_engine: rule_id_and_engine_kind list option
    (**
      These rules, classified by engine used, will let us be transparent in
      the CLI output over what rules were run with what. EXPERIMENTAL: since:
      1.11.0
    *);
  engine_requested: engine_kind option;
  interfile_languages_used: string list option
    (**
      Reporting just the requested engine isn't granular enough. We want to
      know what languages had rules that invoked interfile. This is
      particularly important for tracking the performance impact of new
      interfile languages EXPERIMENTAL: since 1.49.0
    *);
  skipped_rules: skipped_rule list (** EXPERIMENTAL: since: 1.37.0 *);
  subprojects: cli_output_subproject_info list option
    (**
      SCA subproject resolution results. Note: this is only available when
      logged in. EXPERIMENTAL: since: 1.125.0
    *);
  mcp_scan_results: mcp_scan_results option (** MCP scan results. *);
  profiling_results: profiling_entry list
    (**
      How long it took to execute this or that piece of code in semgrep-core
    *)
}

type ci_scan_results_response_error = { message: string } [@@deriving show]

(** Response by the backend to the CLI to the POST /results *)
type ci_scan_results_response = {
  errors: ci_scan_results_response_error list;
  task_id: string option
}
  [@@deriving show]

(** Response by the backend to the CLI to the POST /complete *)
type ci_scan_complete_response = {
  success: bool;
  app_block_override: bool;
  app_block_reason: string (** only when app_block_override is true *);
  app_blocking_match_based_ids: match_based_id list
    (**
      since 1.100.0. match_based_ids of findings that semgrep-app determined
      should cause the scan to block
    *)
}
  [@@deriving show]
