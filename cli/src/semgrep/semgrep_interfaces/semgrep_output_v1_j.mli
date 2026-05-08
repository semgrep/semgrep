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
type datetime = Semgrep_output_v1_t.datetime
  [@@deriving ord]

type dependency_child = Semgrep_output_v1_t.dependency_child = {
  package: string;
  version: string
}
  [@@deriving ord]

type dependency_kind = Semgrep_output_v1_t.dependency_kind = 
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
type ecosystem = Semgrep_output_v1_t.ecosystem = 
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

type fpath = Semgrep_output_v1_t.fpath [@@deriving eq, ord, show]

type found_dependency = Semgrep_output_v1_t.found_dependency = {
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

type lockfile_kind = Semgrep_output_v1_t.lockfile_kind = 
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

type lockfile = Semgrep_output_v1_t.lockfile = {
  kind: lockfile_kind;
  path: fpath
}
  [@@deriving show, eq]

type manifest_kind = Semgrep_output_v1_t.manifest_kind = 
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

type manifest = Semgrep_output_v1_t.manifest = {
  kind: manifest_kind;
  path: fpath
}
  [@@deriving show, eq]

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
type match_severity = Semgrep_output_v1_t.match_severity
  [@@deriving eq, ord, show]

(**
  Note that this type is used in Matching_explanation.ml hence the need for
  deriving show below.
*)
type matching_operation = Semgrep_output_v1_t.matching_operation = 
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
type position = Semgrep_output_v1_t.position = {
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
type location = Semgrep_output_v1_t.location = {
  path: fpath;
  start: position;
  end_ (*atd end *): position
}
  [@@deriving ord, show]

(**
  The string attached to the location is the actual code from the file. This
  can contain sensitive information so be careful!
  
  TODO: the type seems redundant since location already specifies a range.
  maybe this saves some effort to the user of this type which do not need to
  read the file to get the content.
*)
type loc_and_content = Semgrep_output_v1_t.loc_and_content
  [@@deriving ord]

(**
  This type happens to be mostly the same as a loc_and_content for now, but
  it's split out because Iago has plans to extend this with more information
*)
type match_intermediate_var = Semgrep_output_v1_t.match_intermediate_var = {
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
type pro_feature = Semgrep_output_v1_t.pro_feature = {
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
type engine_of_finding = Semgrep_output_v1_t.engine_of_finding
  [@@deriving ord, show]

(** escape hatch *)
type raw_json = JSON.Yojson.t [@@deriving eq, ord, show]

(** e.g., "javascript.security.do-not-use-eval" *)
type rule_id = Semgrep_output_v1_t.rule_id
  [@@deriving show, eq, ord]

type sbom_kind = Semgrep_output_v1_t.sbom_kind = 
  CycloneDXJson (** cyclonedx json - https://cyclonedx.org/docs/1.4/json/ *)

  [@@deriving show { with_path = false }, eq]

type sbom = Semgrep_output_v1_t.sbom = {
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

type sca_pattern = Semgrep_output_v1_t.sca_pattern = {
  ecosystem: ecosystem;
  package: string;
  semver_range: string
}
  [@@deriving ord]

type dependency_match = Semgrep_output_v1_t.dependency_match = {
  dependency_pattern: sca_pattern;
  found_dependency: found_dependency;
  lockfile: fpath
}
  [@@deriving ord]

type sha1 = Semgrep_output_v1_t.sha1 [@@deriving ord]

(** part of cli_match_extra *)
type historical_info = Semgrep_output_v1_t.historical_info = {
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

type svalue_value = Semgrep_output_v1_t.svalue_value = {
  svalue_start: position option;
  svalue_end: position option;
  svalue_abstract_content: string (** value? *)
}
  [@@deriving ord]

type metavar_value = Semgrep_output_v1_t.metavar_value = {
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
type metavars = Semgrep_output_v1_t.metavars
  [@@deriving ord]

type transitive_undetermined = Semgrep_output_v1_t.transitive_undetermined = {
  explanation: string option
}
  [@@deriving ord]

type transitive_unreachable = Semgrep_output_v1_t.transitive_unreachable = {
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
type validation_state = Semgrep_output_v1_t.validation_state
  [@@deriving eq, ord, show]

type dependency_source = Semgrep_output_v1_t.dependency_source = 
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

type match_call_trace = Semgrep_output_v1_t.match_call_trace = 
    CliLoc of loc_and_content
  | CliCall
      of (loc_and_content * match_intermediate_var list * match_call_trace)

  [@@deriving ord]

type match_dataflow_trace = Semgrep_output_v1_t.match_dataflow_trace = {
  taint_source: match_call_trace option;
  intermediate_vars: match_intermediate_var list option
    (**
      Intermediate variables which are involved in the dataflow. This
      explains how the taint flows from the source to the sink.
    *);
  taint_sink: match_call_trace option
}
  [@@deriving ord]

type cli_match = Semgrep_output_v1_t.cli_match = {
  check_id: rule_id;
  path: fpath;
  start: position;
  end_ (*atd end *): position;
  extra: cli_match_extra
}

and cli_match_extra = Semgrep_output_v1_t.cli_match_extra = {
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
and sca_match = Semgrep_output_v1_t.sca_match = {
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
and sca_match_kind = Semgrep_output_v1_t.sca_match_kind = 
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

and transitive_reachable = Semgrep_output_v1_t.transitive_reachable = {
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
type core_match_extra = Semgrep_output_v1_t.core_match_extra = {
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

type core_match = Semgrep_output_v1_t.core_match = {
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
type matching_explanation_extra =
  Semgrep_output_v1_t.matching_explanation_extra = {
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
type matching_explanation = Semgrep_output_v1_t.matching_explanation = {
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
type very_slow_stats = Semgrep_output_v1_t.very_slow_stats = {
  time_ratio: float (** Ratio "sum of very slow time" / "total time" *);
  count_ratio: float (** Ratio "very slow count" / "total count" *)
}

(** e.g., '1.1.0' *)
type version = Semgrep_output_v1_t.version
  [@@deriving show]

type uuid = Semgrep_output_v1_t.uuid [@@deriving ord]

type uri = Semgrep_output_v1_t.uri [@@deriving ord]

(** A symbol is a FQN. *)
type symbol = Semgrep_output_v1_t.symbol = { fqn: string list }
  [@@deriving show]

(**
  We store the location of the usage, because we may want to be able to know
  how many uses of the symbol there are, and where.
*)
type symbol_usage = Semgrep_output_v1_t.symbol_usage = {
  symbol: symbol;
  locs: location list
}
  [@@deriving show]

type symbol_analysis = Semgrep_output_v1_t.symbol_analysis [@@deriving show]

type upload_subproject_symbol_analysis_params =
  Semgrep_output_v1_t.upload_subproject_symbol_analysis_params = {
  token: string;
  scan_id: int;
  manifest: fpath option;
  lockfile: fpath option;
  symbol_analysis: symbol_analysis
}

type unresolved_reason = Semgrep_output_v1_t.unresolved_reason = 
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
type subproject = Semgrep_output_v1_t.subproject = {
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
type sca_parser_name = Semgrep_output_v1_t.sca_parser_name = 
    PGemfile_lock | PGo_mod | PGo_sum | PGradle_lockfile | PGradle_build
  | PJsondoc | PPipfile | PPnpm_lock | PPoetry_lock | PPyproject_toml
  | PRequirements | PYarn_1 | PYarn_2 | PPomtree | PCargo_parser
  | PComposer_lock | PPubspec_lock | PPackage_swift | PPodfile_lock
  | PPackage_resolved | PMix_lock

  [@@deriving show]

type resource_inaccessible = Semgrep_output_v1_t.resource_inaccessible = {
  command: string;
  registry_url: string option
    (**
      we attempt to parse out the actual registry URL that we tried to access
    *);
  message: string
    (** and just include the entire error message too, just in case *)
}
  [@@deriving show]

type resolution_cmd_failed = Semgrep_output_v1_t.resolution_cmd_failed = {
  command: string;
  message: string
}
  [@@deriving show]

type resolution_error_kind = Semgrep_output_v1_t.resolution_error_kind = 
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
type sca_resolution_error = Semgrep_output_v1_t.sca_resolution_error = {
  type_: resolution_error_kind;
  dependency_source_file: fpath
}

type dependency_parser_error = Semgrep_output_v1_t.dependency_parser_error = {
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

type sca_error = Semgrep_output_v1_t.sca_error = 
    SCAParse of dependency_parser_error
  | SCAResol of sca_resolution_error


type unresolved_subproject = Semgrep_output_v1_t.unresolved_subproject = {
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
type snippet = Semgrep_output_v1_t.snippet = { line: int; text: string }

type killing_parent_kind = Semgrep_output_v1_t.killing_parent_kind

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
type killing_parent = Semgrep_output_v1_t.killing_parent = {
  killing_parent_kind: killing_parent_kind;
  snippet: snippet
}

type unexpected_no_match_diagnosis_kind =
  Semgrep_output_v1_t.unexpected_no_match_diagnosis_kind

type unexpected_no_match_diagnosis =
  Semgrep_output_v1_t.unexpected_no_match_diagnosis = {
  line: int;
  kind: unexpected_no_match_diagnosis_kind
}

type originating_node_kind = Semgrep_output_v1_t.originating_node_kind

type unexpected_match_diagnosis =
  Semgrep_output_v1_t.unexpected_match_diagnosis = {
  matched_text: snippet;
  originating_kind: originating_node_kind
    (**
      information about the originating pattern in the rule file. This is
      where the unexpected match came from.
    *);
  originating_text: snippet;
  killing_parents: killing_parent list
}

type triage_ignored = Semgrep_output_v1_t.triage_ignored = {
  triage_ignored_syntactic_ids: string list;
  triage_ignored_match_based_ids: string list
}

type transitive_finding = Semgrep_output_v1_t.transitive_finding = {
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
type downloaded_dependency = Semgrep_output_v1_t.downloaded_dependency = {
  source_paths: fpath list
}

type resolved_dependency = Semgrep_output_v1_t.resolved_dependency

type transitive_reachability_filter_params =
  Semgrep_output_v1_t.transitive_reachability_filter_params = {
  rules_path: fpath;
  findings: transitive_finding list;
  dependencies: resolved_dependency list;
  write_to_cache: bool
}

(** The "value" *)
type tr_cache_match_result = Semgrep_output_v1_t.tr_cache_match_result = {
  matches: cli_match list
}

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
type tr_cache_key = Semgrep_output_v1_t.tr_cache_key = {
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
type tr_query_cache_response = Semgrep_output_v1_t.tr_query_cache_response = {
  cached: (tr_cache_key * tr_cache_match_result) list
}

(** Sent by the CLI to the POST /api/cli/tr_cache/lookup *)
type tr_query_cache_request = Semgrep_output_v1_t.tr_query_cache_request = {
  entries: tr_cache_key list
}

(** Sent by the CLI to the POST /api/cli/tr_cache *)
type tr_add_cache_request = Semgrep_output_v1_t.tr_add_cache_request = {
  new_entries: (tr_cache_key * tr_cache_match_result) list
}

type todo = Semgrep_output_v1_t.todo

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
type matching_diagnosis = Semgrep_output_v1_t.matching_diagnosis = {
  target: fpath (** specifically, the test target *);
  unexpected_match_diagnoses: unexpected_match_diagnosis list;
  unexpected_no_match_diagnoses: unexpected_no_match_diagnosis list
}

type expected_reported = Semgrep_output_v1_t.expected_reported = {
  expected_lines: int list;
  reported_lines: int list
}

type rule_result = Semgrep_output_v1_t.rule_result = {
  passed: bool;
  matches: (string * expected_reported) list
    (** (target filename, expected_reported) list *);
  errors: todo list;
  diagnosis: matching_diagnosis option (** NEW: since 1.79 *)
}

type fixtest_result = Semgrep_output_v1_t.fixtest_result = { passed: bool }

type config_error_reason = Semgrep_output_v1_t.config_error_reason = 
  UnparsableRule


type config_error = Semgrep_output_v1_t.config_error = {
  file: fpath;
  reason: config_error_reason
}

type checks = Semgrep_output_v1_t.checks = {
  checks: (string * rule_result) list (** (rule ID, rule_result) list *)
}

type tests_result = Semgrep_output_v1_t.tests_result = {
  results: (string * checks) list (** (rule file, checks) list *);
  fixtest_results: (string * fixtest_result) list
    (** (target file, fixtest_result) list *);
  config_missing_tests: fpath list;
  config_missing_fixtests: fpath list;
  config_with_errors: config_error list
}

(** See Scan_CLI.ml on how to convert command-line options to this *)
type project_root = Semgrep_output_v1_t.project_root
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
type targeting_conf = Semgrep_output_v1_t.targeting_conf = {
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

type product = Semgrep_output_v1_t.product [@@deriving eq, ord, show]

type ppath = Semgrep_output_v1_t.ppath [@@deriving show, eq]

(**
  Same as Fppath.t: a nice filesystem path + the path relative to the project
  root provided for pattern-based filtering purposes.
*)
type fppath = Semgrep_output_v1_t.fppath = { fpath: fpath; ppath: ppath }
  [@@deriving show, eq]

type analyzer = Semgrep_output_v1_t.analyzer [@@deriving show]

(**
  A normal semgrep target, optionally with an associated \[lockfile\] The
  lockfile means: the code in this file has its dependencies specified by
  this lockfile We don't want to commit to a specific way of associating
  these in semgrep-core, so we leave it up to the caller (pysemgrep or
  osemgrep) to do it.
*)
type code_target = Semgrep_output_v1_t.code_target = {
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
type target = Semgrep_output_v1_t.target
  [@@deriving show]

type scanning_roots = Semgrep_output_v1_t.scanning_roots = {
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
type targets = Semgrep_output_v1_t.targets
  [@@deriving show]

type target_times = Semgrep_output_v1_t.target_times = {
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
type skip_reason = Semgrep_output_v1_t.skip_reason = 
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
type skipped_target = Semgrep_output_v1_t.skipped_target = {
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

type incompatible_rule = Semgrep_output_v1_t.incompatible_rule = {
  rule_id: rule_id;
  this_version: version;
  min_version: version option;
  max_version: version option
}
  [@@deriving show]

type error_type = Semgrep_output_v1_t.error_type = 
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
type error_severity = Semgrep_output_v1_t.error_severity
  [@@deriving show, eq]

(** See Semgrep_error_code.ml *)
type core_error = Semgrep_output_v1_t.core_error = {
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
type target_discovery_result = Semgrep_output_v1_t.target_discovery_result = {
  target_paths: fppath list;
  errors: core_error list;
  skipped: skipped_target list
}

(** EXPERIMENTAL *)
type summary_stats = Semgrep_output_v1_t.summary_stats = {
  mean: float;
  std_dev: float
}

(** EXPERIMENTAL *)
type def_rule_time = Semgrep_output_v1_t.def_rule_time = {
  fpath: fpath;
  fline: int;
  rule_id: rule_id;
  time: float
}
  [@@deriving show]

(** EXPERIMENTAL *)
type tainting_time = Semgrep_output_v1_t.tainting_time = {
  total_time: float;
  per_def_and_rule_time: summary_stats;
  very_slow_stats: very_slow_stats;
  very_slow_rules_on_defs: def_rule_time list (** ascending order *)
}

(** e.g. "webapp" *)
type tag = Semgrep_output_v1_t.tag

type symbol_analysis_upload_response =
  Semgrep_output_v1_t.symbol_analysis_upload_response = {
  upload_url: uri (** Presigned AWS URL for uploading symbol analysis data *)
}

type symbol_analysis_params = Semgrep_output_v1_t.symbol_analysis_params = {
  root_path: fpath;
  lang: string option;
  files: fpath list
}

type resolution_method = Semgrep_output_v1_t.resolution_method
  [@@deriving show]

type dependency_source_file_kind =
  Semgrep_output_v1_t.dependency_source_file_kind
  [@@deriving show]

type dependency_source_file = Semgrep_output_v1_t.dependency_source_file = {
  kind: dependency_source_file_kind;
  path: fpath
}

type dependency_resolution_stats =
  Semgrep_output_v1_t.dependency_resolution_stats = {
  resolution_method: resolution_method;
  dependency_count: int;
  ecosystem: ecosystem
}

type subproject_stats = Semgrep_output_v1_t.subproject_stats = {
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

type supply_chain_stats = Semgrep_output_v1_t.supply_chain_stats = {
  subprojects_stats: subproject_stats list
}

(**
  Sent by the CLI to the POST
  /api/agent/scans/\{scan_id\}/subproject_symbols_upload_url/
*)
type subproject_symbol_analysis_url_request =
  Semgrep_output_v1_t.subproject_symbol_analysis_url_request = {
  manifest_path: fpath option;
  lockfile_path: fpath option
}

type single_subproject_plan = Semgrep_output_v1_t.single_subproject_plan = {
  subproject_id: string;
  root_dir: fpath;
  resolution_planned: bool
}

(** Subproject dump output. Experimental and for internal use only. *)
type subproject_resolution_plan =
  Semgrep_output_v1_t.subproject_resolution_plan = {
  subprojects: single_subproject_plan list
}

type skipped_rule = Semgrep_output_v1_t.skipped_rule = {
  rule_id: rule_id;
  details: string;
  position: position (** position of the error in the rule file *)
}

(** EXPERIMENTAL *)
type file_time = Semgrep_output_v1_t.file_time = {
  fpath: fpath;
  ftime: float
}
  [@@deriving show]

(** Scanning time (includes matching and tainting) EXPERIMENTAL *)
type scanning_time = Semgrep_output_v1_t.scanning_time = {
  total_time: float;
  per_file_time: summary_stats;
  very_slow_stats: very_slow_stats;
  very_slow_files: file_time list (** ascending order *)
}

type scanned_and_skipped = Semgrep_output_v1_t.scanned_and_skipped = {
  scanned: fpath list;
  skipped: skipped_target list option
}

(** meta info about the scan *)
type scan_info = Semgrep_output_v1_t.scan_info = {
  id: int option (** the scan id, null for dry-runs *);
  enabled_products: product list;
  deployment_id: int;
  deployment_name: string
}

(** config specific to the scan *)
type scan_configuration = Semgrep_output_v1_t.scan_configuration = {
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

type glob = Semgrep_output_v1_t.glob

type product_ignored_files = Semgrep_output_v1_t.product_ignored_files

(**
  configuration for scanning version control history, e.g., looking back at
  past git commits for committed credentials which may have been removed
*)
type historical_configuration =
  Semgrep_output_v1_t.historical_configuration = {
  enabled: bool;
  lookback_days: int option
}

(** settings for the cli *)
type engine_configuration = Semgrep_output_v1_t.engine_configuration = {
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
type scan_response = Semgrep_output_v1_t.scan_response = {
  info: scan_info;
  config: scan_configuration;
  engine_params: engine_configuration
}

(** Scan metadata generated by the CLI during the scan process. *)
type scan_metadata = Semgrep_output_v1_t.scan_metadata = {
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
type project_metadata = Semgrep_output_v1_t.project_metadata = {
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
type ci_config_from_repo = Semgrep_output_v1_t.ci_config_from_repo = {
  version: version
    (** version of the .semgrepconfig.yml format. "v1" right now (useful?) *);
  tags: tag list option
}

(**
  Sent by the CLI to the (deprecated) POST /api/cli/scans to create a scan.
*)
type scan_request = Semgrep_output_v1_t.scan_request = {
  project_metadata: project_metadata;
  scan_metadata: scan_metadata;
  project_config: ci_config_from_repo option
}

type ci_env = Semgrep_output_v1_t.ci_env

(**
  Note that we should use very simple types below for the configuration of
  Semgrep: booleans or small enums. No int, as people often don't understand
  how to set values. For example even if we documented very well the
  --timeout option in Semgrep, people still didn't know which value to use.
*)
type ci_config = Semgrep_output_v1_t.ci_config = {
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
type action = Semgrep_output_v1_t.action

(** Semgrep config from the WebApp *)
type ci_config_from_cloud = Semgrep_output_v1_t.ci_config_from_cloud = {
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
type scan_config = Semgrep_output_v1_t.scan_config = {
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

type sarif_format = Semgrep_output_v1_t.sarif_format = {
  rules: fpath
    (**
      Path to the rules file. We need it because rules can't be reconstructed
      from cli_output (which is one of the other param of CallSarifFormat)
    *);
  is_pro: bool;
  show_dataflow_traces: bool
}

type engine_kind = Semgrep_output_v1_t.engine_kind [@@deriving ord, show]

type rule_id_and_engine_kind = Semgrep_output_v1_t.rule_id_and_engine_kind

type resolve_dependencies_params =
  Semgrep_output_v1_t.resolve_dependencies_params = {
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
type profiling_entry = Semgrep_output_v1_t.profiling_entry = {
  name: string
    (**
      The name given to piece of code for which we measured how long it took.
    *);
  total_time: float
    (** Total clock time in seconds. Divide by the count to get the mean. *);
  count: int
}

type prefiltering_stats = Semgrep_output_v1_t.prefiltering_stats = {
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
type parsing_time = Semgrep_output_v1_t.parsing_time = {
  total_time: float;
  per_file_time: summary_stats;
  very_slow_stats: very_slow_stats option;
  very_slow_files: file_time list (** ascending order *)
}

(** EXPERIMENTAL *)
type file_rule_time = Semgrep_output_v1_t.file_rule_time = {
  fpath: fpath;
  rule_id: rule_id;
  time: float
}
  [@@deriving show]

(** EXPERIMENTAL *)
type matching_time = Semgrep_output_v1_t.matching_time = {
  total_time: float;
  per_file_and_rule_time: summary_stats;
  very_slow_stats: very_slow_stats;
  very_slow_rules_on_files: file_rule_time list (** ascending order *)
}

(** Run locally $ ./run-benchmarks --dummy --upload *)
type profile = Semgrep_output_v1_t.profile = {
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

type output_format = Semgrep_output_v1_t.output_format = 
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

type mcp_scan_results = Semgrep_output_v1_t.mcp_scan_results = {
  rules: string list;
  total_bytes_scanned: int
}

type format_context = Semgrep_output_v1_t.format_context = {
  is_ci_invocation: bool;
  is_logged_in: bool;
  is_using_registry: bool
}
  [@@deriving show]

type error_span = Semgrep_output_v1_t.error_span = {
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

type edit = Semgrep_output_v1_t.edit = {
  path: fpath;
  start_offset: int;
  end_offset: int;
  replacement_text: string
}

type dump_rule_partitions_params =
  Semgrep_output_v1_t.dump_rule_partitions_params = {
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
type cli_output_subproject_info =
  Semgrep_output_v1_t.cli_output_subproject_info = {
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
type cli_error = Semgrep_output_v1_t.cli_error = {
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

type cli_output = Semgrep_output_v1_t.cli_output = {
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

type apply_fixes_params = Semgrep_output_v1_t.apply_fixes_params = {
  dryrun: bool;
  edits: edit list
}

type function_call = Semgrep_output_v1_t.function_call

type rpc_call = Semgrep_output_v1_t.rpc_call = {
  call: function_call;
  parent_span_id: string option
}

(** A subproject plus its resolved set of dependencies *)
type resolved_subproject = Semgrep_output_v1_t.resolved_subproject = {
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
type resolution_result = Semgrep_output_v1_t.resolution_result

(** Recommendations for subsequent requests *)
type polling_information = Semgrep_output_v1_t.polling_information = {
  recommended_wait_seconds: int;
  seconds_until_timeout: int
}

type parsing_stats = Semgrep_output_v1_t.parsing_stats = {
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
type finding_hashes = Semgrep_output_v1_t.finding_hashes = {
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

type finding = Semgrep_output_v1_t.finding = {
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
type contributor = Semgrep_output_v1_t.contributor = {
  commit_author_name: string;
  commit_author_email: string
}

type contribution = Semgrep_output_v1_t.contribution = {
  commit_hash: string;
  commit_timestamp: datetime;
  contributor: contributor
}

(**
  We keep this alias because we need to generate code to parse and write list
  of contributions.
*)
type contributions = Semgrep_output_v1_t.contributions

(**
  Scan metadata populated by the backend after receiving the scan results
  from the CLI via POST request to /scans/<int:scan_id>/results
*)
type ci_scan_metadata = Semgrep_output_v1_t.ci_scan_metadata = {
  scan_id: int;
  deployment_id: int;
  repository_id: int (** stored as int in our app db *);
  repository_ref_id: int (** stored id for a branch or tag *);
  enabled_products: product list;
  git_commit: sha1 option;
  git_ref: string option
}

type ci_scan_dependencies = Semgrep_output_v1_t.ci_scan_dependencies

type ci_scan_results = Semgrep_output_v1_t.ci_scan_results = {
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
type ci_scan_failure = Semgrep_output_v1_t.ci_scan_failure = {
  exit_code: int;
  stderr: string
}

type ci_scan_complete_stats = Semgrep_output_v1_t.ci_scan_complete_stats = {
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
type ci_scan_complete = Semgrep_output_v1_t.ci_scan_complete = {
  exit_code: int;
  stats: ci_scan_complete_stats;
  dependencies: ci_scan_dependencies option;
  dependency_parser_errors: dependency_parser_error list option;
  task_id: string option (** since 1.31.0 *);
  final_attempt: bool option
}

(** Partial scans. Experimental and for internal use only. *)
type partial_scan_result = Semgrep_output_v1_t.partial_scan_result

(** e.g. "ab023_1" *)
type match_based_id = Semgrep_output_v1_t.match_based_id
  [@@deriving show, eq]

(** whether a certain feature is available for a deployment *)
type has_features = Semgrep_output_v1_t.has_features = {
  has_autofix: bool;
  has_deepsemgrep: bool;
  has_triage_via_comment: bool;
  has_dependency_query: bool
}

type get_config_response_status =
  Semgrep_output_v1_t.get_config_response_status = 
    Pending | Success | Failure


(**
  Response from the backend to the CLI for GET
  /api/cli/v2/scans/<scan_request_id>/config.
*)
type get_config_response_v2 = Semgrep_output_v1_t.get_config_response_v2 = {
  status: get_config_response_status;
  polling: polling_information option;
  config: scan_configuration option;
  engine_params: engine_configuration option
}

type apply_fixes_return = Semgrep_output_v1_t.apply_fixes_return = {
  modified_file_count: int (** Number of files modified *);
  fixed_lines: (int * string list) list
    (**
      Each item is a pair, where the first item is the index of the
      associated edit in the input list and the second item is the list of
      fixed lines associated with that edit.
    *)
}

type function_return = Semgrep_output_v1_t.function_return

type function_result = Semgrep_output_v1_t.function_result = {
  function_return: function_return;
  profiling_results: profiling_entry list
}

type features = Semgrep_output_v1_t.features = {
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
type diff_file = Semgrep_output_v1_t.diff_file = {
  filename: fpath;
  diffs: string list (** start_line-end_line *);
  url: string (** metadata to help SCA rule generation *)
}
  [@@deriving show]

type diff_files = Semgrep_output_v1_t.diff_files = {
  cve_diffs: diff_file list
}
  [@@deriving show]

(**
  Response by the backend to the CLI to the POST
  api/agent/deployments/current. Some of the information in deployment_config
  is now returned directly in scan_response (e.g., the deployment_name)
*)
type deployment_config = Semgrep_output_v1_t.deployment_config = {
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

type deployment_response = Semgrep_output_v1_t.deployment_response = {
  deployment: deployment_config
}

(** Response from the backend to the CLI for POST /api/cli/v2/scans. *)
type create_scan_response_v2 = Semgrep_output_v1_t.create_scan_response_v2 = {
  info: scan_info
}

(** Sent by the CLI to the backend in POST /api/cli/v2/scans. *)
type create_scan_request_v2 = Semgrep_output_v1_t.create_scan_request_v2 = {
  project_metadata: project_metadata;
  scan_metadata: scan_metadata;
  project_config: ci_config_from_repo option
}

(**
  For extra information to put into the `core_output` that we do not
  necessarily want to share with the cli_output.
*)
type core_output_extra = Semgrep_output_v1_t.core_output_extra = {
  symbol_analysis: symbol_analysis option (** since semgrep 1.108.0 *)
}

type core_output = Semgrep_output_v1_t.core_output = {
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

type cli_output_extra = Semgrep_output_v1_t.cli_output_extra = {
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

type ci_scan_results_response_error =
  Semgrep_output_v1_t.ci_scan_results_response_error = {
  message: string
}
  [@@deriving show]

(** Response by the backend to the CLI to the POST /results *)
type ci_scan_results_response =
  Semgrep_output_v1_t.ci_scan_results_response = {
  errors: ci_scan_results_response_error list;
  task_id: string option
}
  [@@deriving show]

(** Response by the backend to the CLI to the POST /complete *)
type ci_scan_complete_response =
  Semgrep_output_v1_t.ci_scan_complete_response = {
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

val write_datetime :
  Buffer.t -> datetime -> unit
  (** Output a JSON value of type {!type:datetime}. *)

val string_of_datetime :
  ?len:int -> datetime -> string
  (** Serialize a value of type {!type:datetime}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_datetime :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> datetime
  (** Input JSON data of type {!type:datetime}. *)

val datetime_of_string :
  string -> datetime
  (** Deserialize JSON data of type {!type:datetime}. *)

val write_dependency_child :
  Buffer.t -> dependency_child -> unit
  (** Output a JSON value of type {!type:dependency_child}. *)

val string_of_dependency_child :
  ?len:int -> dependency_child -> string
  (** Serialize a value of type {!type:dependency_child}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_child :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_child
  (** Input JSON data of type {!type:dependency_child}. *)

val dependency_child_of_string :
  string -> dependency_child
  (** Deserialize JSON data of type {!type:dependency_child}. *)

val write_dependency_kind :
  Buffer.t -> dependency_kind -> unit
  (** Output a JSON value of type {!type:dependency_kind}. *)

val string_of_dependency_kind :
  ?len:int -> dependency_kind -> string
  (** Serialize a value of type {!type:dependency_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_kind
  (** Input JSON data of type {!type:dependency_kind}. *)

val dependency_kind_of_string :
  string -> dependency_kind
  (** Deserialize JSON data of type {!type:dependency_kind}. *)

val write_ecosystem :
  Buffer.t -> ecosystem -> unit
  (** Output a JSON value of type {!type:ecosystem}. *)

val string_of_ecosystem :
  ?len:int -> ecosystem -> string
  (** Serialize a value of type {!type:ecosystem}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ecosystem :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ecosystem
  (** Input JSON data of type {!type:ecosystem}. *)

val ecosystem_of_string :
  string -> ecosystem
  (** Deserialize JSON data of type {!type:ecosystem}. *)

val write_fpath :
  Buffer.t -> fpath -> unit
  (** Output a JSON value of type {!type:fpath}. *)

val string_of_fpath :
  ?len:int -> fpath -> string
  (** Serialize a value of type {!type:fpath}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_fpath :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> fpath
  (** Input JSON data of type {!type:fpath}. *)

val fpath_of_string :
  string -> fpath
  (** Deserialize JSON data of type {!type:fpath}. *)

val write_found_dependency :
  Buffer.t -> found_dependency -> unit
  (** Output a JSON value of type {!type:found_dependency}. *)

val string_of_found_dependency :
  ?len:int -> found_dependency -> string
  (** Serialize a value of type {!type:found_dependency}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_found_dependency :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> found_dependency
  (** Input JSON data of type {!type:found_dependency}. *)

val found_dependency_of_string :
  string -> found_dependency
  (** Deserialize JSON data of type {!type:found_dependency}. *)

val write_lockfile_kind :
  Buffer.t -> lockfile_kind -> unit
  (** Output a JSON value of type {!type:lockfile_kind}. *)

val string_of_lockfile_kind :
  ?len:int -> lockfile_kind -> string
  (** Serialize a value of type {!type:lockfile_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_lockfile_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> lockfile_kind
  (** Input JSON data of type {!type:lockfile_kind}. *)

val lockfile_kind_of_string :
  string -> lockfile_kind
  (** Deserialize JSON data of type {!type:lockfile_kind}. *)

val write_lockfile :
  Buffer.t -> lockfile -> unit
  (** Output a JSON value of type {!type:lockfile}. *)

val string_of_lockfile :
  ?len:int -> lockfile -> string
  (** Serialize a value of type {!type:lockfile}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_lockfile :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> lockfile
  (** Input JSON data of type {!type:lockfile}. *)

val lockfile_of_string :
  string -> lockfile
  (** Deserialize JSON data of type {!type:lockfile}. *)

val write_manifest_kind :
  Buffer.t -> manifest_kind -> unit
  (** Output a JSON value of type {!type:manifest_kind}. *)

val string_of_manifest_kind :
  ?len:int -> manifest_kind -> string
  (** Serialize a value of type {!type:manifest_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_manifest_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> manifest_kind
  (** Input JSON data of type {!type:manifest_kind}. *)

val manifest_kind_of_string :
  string -> manifest_kind
  (** Deserialize JSON data of type {!type:manifest_kind}. *)

val write_manifest :
  Buffer.t -> manifest -> unit
  (** Output a JSON value of type {!type:manifest}. *)

val string_of_manifest :
  ?len:int -> manifest -> string
  (** Serialize a value of type {!type:manifest}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_manifest :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> manifest
  (** Input JSON data of type {!type:manifest}. *)

val manifest_of_string :
  string -> manifest
  (** Deserialize JSON data of type {!type:manifest}. *)

val write_match_severity :
  Buffer.t -> match_severity -> unit
  (** Output a JSON value of type {!type:match_severity}. *)

val string_of_match_severity :
  ?len:int -> match_severity -> string
  (** Serialize a value of type {!type:match_severity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_match_severity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> match_severity
  (** Input JSON data of type {!type:match_severity}. *)

val match_severity_of_string :
  string -> match_severity
  (** Deserialize JSON data of type {!type:match_severity}. *)

val write_matching_operation :
  Buffer.t -> matching_operation -> unit
  (** Output a JSON value of type {!type:matching_operation}. *)

val string_of_matching_operation :
  ?len:int -> matching_operation -> string
  (** Serialize a value of type {!type:matching_operation}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_matching_operation :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> matching_operation
  (** Input JSON data of type {!type:matching_operation}. *)

val matching_operation_of_string :
  string -> matching_operation
  (** Deserialize JSON data of type {!type:matching_operation}. *)

val write_position :
  Buffer.t -> position -> unit
  (** Output a JSON value of type {!type:position}. *)

val string_of_position :
  ?len:int -> position -> string
  (** Serialize a value of type {!type:position}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_position :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> position
  (** Input JSON data of type {!type:position}. *)

val position_of_string :
  string -> position
  (** Deserialize JSON data of type {!type:position}. *)

val write_location :
  Buffer.t -> location -> unit
  (** Output a JSON value of type {!type:location}. *)

val string_of_location :
  ?len:int -> location -> string
  (** Serialize a value of type {!type:location}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_location :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> location
  (** Input JSON data of type {!type:location}. *)

val location_of_string :
  string -> location
  (** Deserialize JSON data of type {!type:location}. *)

val write_loc_and_content :
  Buffer.t -> loc_and_content -> unit
  (** Output a JSON value of type {!type:loc_and_content}. *)

val string_of_loc_and_content :
  ?len:int -> loc_and_content -> string
  (** Serialize a value of type {!type:loc_and_content}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_loc_and_content :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> loc_and_content
  (** Input JSON data of type {!type:loc_and_content}. *)

val loc_and_content_of_string :
  string -> loc_and_content
  (** Deserialize JSON data of type {!type:loc_and_content}. *)

val write_match_intermediate_var :
  Buffer.t -> match_intermediate_var -> unit
  (** Output a JSON value of type {!type:match_intermediate_var}. *)

val string_of_match_intermediate_var :
  ?len:int -> match_intermediate_var -> string
  (** Serialize a value of type {!type:match_intermediate_var}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_match_intermediate_var :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> match_intermediate_var
  (** Input JSON data of type {!type:match_intermediate_var}. *)

val match_intermediate_var_of_string :
  string -> match_intermediate_var
  (** Deserialize JSON data of type {!type:match_intermediate_var}. *)

val write_pro_feature :
  Buffer.t -> pro_feature -> unit
  (** Output a JSON value of type {!type:pro_feature}. *)

val string_of_pro_feature :
  ?len:int -> pro_feature -> string
  (** Serialize a value of type {!type:pro_feature}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pro_feature :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pro_feature
  (** Input JSON data of type {!type:pro_feature}. *)

val pro_feature_of_string :
  string -> pro_feature
  (** Deserialize JSON data of type {!type:pro_feature}. *)

val write_engine_of_finding :
  Buffer.t -> engine_of_finding -> unit
  (** Output a JSON value of type {!type:engine_of_finding}. *)

val string_of_engine_of_finding :
  ?len:int -> engine_of_finding -> string
  (** Serialize a value of type {!type:engine_of_finding}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_engine_of_finding :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> engine_of_finding
  (** Input JSON data of type {!type:engine_of_finding}. *)

val engine_of_finding_of_string :
  string -> engine_of_finding
  (** Deserialize JSON data of type {!type:engine_of_finding}. *)

val write_raw_json :
  Buffer.t -> raw_json -> unit
  (** Output a JSON value of type {!type:raw_json}. *)

val string_of_raw_json :
  ?len:int -> raw_json -> string
  (** Serialize a value of type {!type:raw_json}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_raw_json :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> raw_json
  (** Input JSON data of type {!type:raw_json}. *)

val raw_json_of_string :
  string -> raw_json
  (** Deserialize JSON data of type {!type:raw_json}. *)

val write_rule_id :
  Buffer.t -> rule_id -> unit
  (** Output a JSON value of type {!type:rule_id}. *)

val string_of_rule_id :
  ?len:int -> rule_id -> string
  (** Serialize a value of type {!type:rule_id}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rule_id :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rule_id
  (** Input JSON data of type {!type:rule_id}. *)

val rule_id_of_string :
  string -> rule_id
  (** Deserialize JSON data of type {!type:rule_id}. *)

val write_sbom_kind :
  Buffer.t -> sbom_kind -> unit
  (** Output a JSON value of type {!type:sbom_kind}. *)

val string_of_sbom_kind :
  ?len:int -> sbom_kind -> string
  (** Serialize a value of type {!type:sbom_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sbom_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sbom_kind
  (** Input JSON data of type {!type:sbom_kind}. *)

val sbom_kind_of_string :
  string -> sbom_kind
  (** Deserialize JSON data of type {!type:sbom_kind}. *)

val write_sbom :
  Buffer.t -> sbom -> unit
  (** Output a JSON value of type {!type:sbom}. *)

val string_of_sbom :
  ?len:int -> sbom -> string
  (** Serialize a value of type {!type:sbom}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sbom :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sbom
  (** Input JSON data of type {!type:sbom}. *)

val sbom_of_string :
  string -> sbom
  (** Deserialize JSON data of type {!type:sbom}. *)

val write_sca_pattern :
  Buffer.t -> sca_pattern -> unit
  (** Output a JSON value of type {!type:sca_pattern}. *)

val string_of_sca_pattern :
  ?len:int -> sca_pattern -> string
  (** Serialize a value of type {!type:sca_pattern}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sca_pattern :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sca_pattern
  (** Input JSON data of type {!type:sca_pattern}. *)

val sca_pattern_of_string :
  string -> sca_pattern
  (** Deserialize JSON data of type {!type:sca_pattern}. *)

val write_dependency_match :
  Buffer.t -> dependency_match -> unit
  (** Output a JSON value of type {!type:dependency_match}. *)

val string_of_dependency_match :
  ?len:int -> dependency_match -> string
  (** Serialize a value of type {!type:dependency_match}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_match :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_match
  (** Input JSON data of type {!type:dependency_match}. *)

val dependency_match_of_string :
  string -> dependency_match
  (** Deserialize JSON data of type {!type:dependency_match}. *)

val write_sha1 :
  Buffer.t -> sha1 -> unit
  (** Output a JSON value of type {!type:sha1}. *)

val string_of_sha1 :
  ?len:int -> sha1 -> string
  (** Serialize a value of type {!type:sha1}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sha1 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sha1
  (** Input JSON data of type {!type:sha1}. *)

val sha1_of_string :
  string -> sha1
  (** Deserialize JSON data of type {!type:sha1}. *)

val write_historical_info :
  Buffer.t -> historical_info -> unit
  (** Output a JSON value of type {!type:historical_info}. *)

val string_of_historical_info :
  ?len:int -> historical_info -> string
  (** Serialize a value of type {!type:historical_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_historical_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> historical_info
  (** Input JSON data of type {!type:historical_info}. *)

val historical_info_of_string :
  string -> historical_info
  (** Deserialize JSON data of type {!type:historical_info}. *)

val write_svalue_value :
  Buffer.t -> svalue_value -> unit
  (** Output a JSON value of type {!type:svalue_value}. *)

val string_of_svalue_value :
  ?len:int -> svalue_value -> string
  (** Serialize a value of type {!type:svalue_value}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_svalue_value :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> svalue_value
  (** Input JSON data of type {!type:svalue_value}. *)

val svalue_value_of_string :
  string -> svalue_value
  (** Deserialize JSON data of type {!type:svalue_value}. *)

val write_metavar_value :
  Buffer.t -> metavar_value -> unit
  (** Output a JSON value of type {!type:metavar_value}. *)

val string_of_metavar_value :
  ?len:int -> metavar_value -> string
  (** Serialize a value of type {!type:metavar_value}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_metavar_value :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> metavar_value
  (** Input JSON data of type {!type:metavar_value}. *)

val metavar_value_of_string :
  string -> metavar_value
  (** Deserialize JSON data of type {!type:metavar_value}. *)

val write_metavars :
  Buffer.t -> metavars -> unit
  (** Output a JSON value of type {!type:metavars}. *)

val string_of_metavars :
  ?len:int -> metavars -> string
  (** Serialize a value of type {!type:metavars}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_metavars :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> metavars
  (** Input JSON data of type {!type:metavars}. *)

val metavars_of_string :
  string -> metavars
  (** Deserialize JSON data of type {!type:metavars}. *)

val write_transitive_undetermined :
  Buffer.t -> transitive_undetermined -> unit
  (** Output a JSON value of type {!type:transitive_undetermined}. *)

val string_of_transitive_undetermined :
  ?len:int -> transitive_undetermined -> string
  (** Serialize a value of type {!type:transitive_undetermined}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_transitive_undetermined :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> transitive_undetermined
  (** Input JSON data of type {!type:transitive_undetermined}. *)

val transitive_undetermined_of_string :
  string -> transitive_undetermined
  (** Deserialize JSON data of type {!type:transitive_undetermined}. *)

val write_transitive_unreachable :
  Buffer.t -> transitive_unreachable -> unit
  (** Output a JSON value of type {!type:transitive_unreachable}. *)

val string_of_transitive_unreachable :
  ?len:int -> transitive_unreachable -> string
  (** Serialize a value of type {!type:transitive_unreachable}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_transitive_unreachable :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> transitive_unreachable
  (** Input JSON data of type {!type:transitive_unreachable}. *)

val transitive_unreachable_of_string :
  string -> transitive_unreachable
  (** Deserialize JSON data of type {!type:transitive_unreachable}. *)

val write_validation_state :
  Buffer.t -> validation_state -> unit
  (** Output a JSON value of type {!type:validation_state}. *)

val string_of_validation_state :
  ?len:int -> validation_state -> string
  (** Serialize a value of type {!type:validation_state}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_validation_state :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> validation_state
  (** Input JSON data of type {!type:validation_state}. *)

val validation_state_of_string :
  string -> validation_state
  (** Deserialize JSON data of type {!type:validation_state}. *)

val write_dependency_source :
  Buffer.t -> dependency_source -> unit
  (** Output a JSON value of type {!type:dependency_source}. *)

val string_of_dependency_source :
  ?len:int -> dependency_source -> string
  (** Serialize a value of type {!type:dependency_source}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_source :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_source
  (** Input JSON data of type {!type:dependency_source}. *)

val dependency_source_of_string :
  string -> dependency_source
  (** Deserialize JSON data of type {!type:dependency_source}. *)

val write_match_call_trace :
  Buffer.t -> match_call_trace -> unit
  (** Output a JSON value of type {!type:match_call_trace}. *)

val string_of_match_call_trace :
  ?len:int -> match_call_trace -> string
  (** Serialize a value of type {!type:match_call_trace}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_match_call_trace :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> match_call_trace
  (** Input JSON data of type {!type:match_call_trace}. *)

val match_call_trace_of_string :
  string -> match_call_trace
  (** Deserialize JSON data of type {!type:match_call_trace}. *)

val write_match_dataflow_trace :
  Buffer.t -> match_dataflow_trace -> unit
  (** Output a JSON value of type {!type:match_dataflow_trace}. *)

val string_of_match_dataflow_trace :
  ?len:int -> match_dataflow_trace -> string
  (** Serialize a value of type {!type:match_dataflow_trace}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_match_dataflow_trace :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> match_dataflow_trace
  (** Input JSON data of type {!type:match_dataflow_trace}. *)

val match_dataflow_trace_of_string :
  string -> match_dataflow_trace
  (** Deserialize JSON data of type {!type:match_dataflow_trace}. *)

val write_cli_match :
  Buffer.t -> cli_match -> unit
  (** Output a JSON value of type {!type:cli_match}. *)

val string_of_cli_match :
  ?len:int -> cli_match -> string
  (** Serialize a value of type {!type:cli_match}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cli_match :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cli_match
  (** Input JSON data of type {!type:cli_match}. *)

val cli_match_of_string :
  string -> cli_match
  (** Deserialize JSON data of type {!type:cli_match}. *)

val write_cli_match_extra :
  Buffer.t -> cli_match_extra -> unit
  (** Output a JSON value of type {!type:cli_match_extra}. *)

val string_of_cli_match_extra :
  ?len:int -> cli_match_extra -> string
  (** Serialize a value of type {!type:cli_match_extra}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cli_match_extra :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cli_match_extra
  (** Input JSON data of type {!type:cli_match_extra}. *)

val cli_match_extra_of_string :
  string -> cli_match_extra
  (** Deserialize JSON data of type {!type:cli_match_extra}. *)

val write_sca_match :
  Buffer.t -> sca_match -> unit
  (** Output a JSON value of type {!type:sca_match}. *)

val string_of_sca_match :
  ?len:int -> sca_match -> string
  (** Serialize a value of type {!type:sca_match}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sca_match :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sca_match
  (** Input JSON data of type {!type:sca_match}. *)

val sca_match_of_string :
  string -> sca_match
  (** Deserialize JSON data of type {!type:sca_match}. *)

val write_sca_match_kind :
  Buffer.t -> sca_match_kind -> unit
  (** Output a JSON value of type {!type:sca_match_kind}. *)

val string_of_sca_match_kind :
  ?len:int -> sca_match_kind -> string
  (** Serialize a value of type {!type:sca_match_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sca_match_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sca_match_kind
  (** Input JSON data of type {!type:sca_match_kind}. *)

val sca_match_kind_of_string :
  string -> sca_match_kind
  (** Deserialize JSON data of type {!type:sca_match_kind}. *)

val write_transitive_reachable :
  Buffer.t -> transitive_reachable -> unit
  (** Output a JSON value of type {!type:transitive_reachable}. *)

val string_of_transitive_reachable :
  ?len:int -> transitive_reachable -> string
  (** Serialize a value of type {!type:transitive_reachable}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_transitive_reachable :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> transitive_reachable
  (** Input JSON data of type {!type:transitive_reachable}. *)

val transitive_reachable_of_string :
  string -> transitive_reachable
  (** Deserialize JSON data of type {!type:transitive_reachable}. *)

val write_core_match_extra :
  Buffer.t -> core_match_extra -> unit
  (** Output a JSON value of type {!type:core_match_extra}. *)

val string_of_core_match_extra :
  ?len:int -> core_match_extra -> string
  (** Serialize a value of type {!type:core_match_extra}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_core_match_extra :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> core_match_extra
  (** Input JSON data of type {!type:core_match_extra}. *)

val core_match_extra_of_string :
  string -> core_match_extra
  (** Deserialize JSON data of type {!type:core_match_extra}. *)

val write_core_match :
  Buffer.t -> core_match -> unit
  (** Output a JSON value of type {!type:core_match}. *)

val string_of_core_match :
  ?len:int -> core_match -> string
  (** Serialize a value of type {!type:core_match}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_core_match :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> core_match
  (** Input JSON data of type {!type:core_match}. *)

val core_match_of_string :
  string -> core_match
  (** Deserialize JSON data of type {!type:core_match}. *)

val write_matching_explanation_extra :
  Buffer.t -> matching_explanation_extra -> unit
  (** Output a JSON value of type {!type:matching_explanation_extra}. *)

val string_of_matching_explanation_extra :
  ?len:int -> matching_explanation_extra -> string
  (** Serialize a value of type {!type:matching_explanation_extra}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_matching_explanation_extra :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> matching_explanation_extra
  (** Input JSON data of type {!type:matching_explanation_extra}. *)

val matching_explanation_extra_of_string :
  string -> matching_explanation_extra
  (** Deserialize JSON data of type {!type:matching_explanation_extra}. *)

val write_matching_explanation :
  Buffer.t -> matching_explanation -> unit
  (** Output a JSON value of type {!type:matching_explanation}. *)

val string_of_matching_explanation :
  ?len:int -> matching_explanation -> string
  (** Serialize a value of type {!type:matching_explanation}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_matching_explanation :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> matching_explanation
  (** Input JSON data of type {!type:matching_explanation}. *)

val matching_explanation_of_string :
  string -> matching_explanation
  (** Deserialize JSON data of type {!type:matching_explanation}. *)

val write_very_slow_stats :
  Buffer.t -> very_slow_stats -> unit
  (** Output a JSON value of type {!type:very_slow_stats}. *)

val string_of_very_slow_stats :
  ?len:int -> very_slow_stats -> string
  (** Serialize a value of type {!type:very_slow_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_very_slow_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> very_slow_stats
  (** Input JSON data of type {!type:very_slow_stats}. *)

val very_slow_stats_of_string :
  string -> very_slow_stats
  (** Deserialize JSON data of type {!type:very_slow_stats}. *)

val write_version :
  Buffer.t -> version -> unit
  (** Output a JSON value of type {!type:version}. *)

val string_of_version :
  ?len:int -> version -> string
  (** Serialize a value of type {!type:version}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_version :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> version
  (** Input JSON data of type {!type:version}. *)

val version_of_string :
  string -> version
  (** Deserialize JSON data of type {!type:version}. *)

val write_uuid :
  Buffer.t -> uuid -> unit
  (** Output a JSON value of type {!type:uuid}. *)

val string_of_uuid :
  ?len:int -> uuid -> string
  (** Serialize a value of type {!type:uuid}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_uuid :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> uuid
  (** Input JSON data of type {!type:uuid}. *)

val uuid_of_string :
  string -> uuid
  (** Deserialize JSON data of type {!type:uuid}. *)

val write_uri :
  Buffer.t -> uri -> unit
  (** Output a JSON value of type {!type:uri}. *)

val string_of_uri :
  ?len:int -> uri -> string
  (** Serialize a value of type {!type:uri}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_uri :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> uri
  (** Input JSON data of type {!type:uri}. *)

val uri_of_string :
  string -> uri
  (** Deserialize JSON data of type {!type:uri}. *)

val write_symbol :
  Buffer.t -> symbol -> unit
  (** Output a JSON value of type {!type:symbol}. *)

val string_of_symbol :
  ?len:int -> symbol -> string
  (** Serialize a value of type {!type:symbol}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_symbol :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> symbol
  (** Input JSON data of type {!type:symbol}. *)

val symbol_of_string :
  string -> symbol
  (** Deserialize JSON data of type {!type:symbol}. *)

val write_symbol_usage :
  Buffer.t -> symbol_usage -> unit
  (** Output a JSON value of type {!type:symbol_usage}. *)

val string_of_symbol_usage :
  ?len:int -> symbol_usage -> string
  (** Serialize a value of type {!type:symbol_usage}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_symbol_usage :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> symbol_usage
  (** Input JSON data of type {!type:symbol_usage}. *)

val symbol_usage_of_string :
  string -> symbol_usage
  (** Deserialize JSON data of type {!type:symbol_usage}. *)

val write_symbol_analysis :
  Buffer.t -> symbol_analysis -> unit
  (** Output a JSON value of type {!type:symbol_analysis}. *)

val string_of_symbol_analysis :
  ?len:int -> symbol_analysis -> string
  (** Serialize a value of type {!type:symbol_analysis}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_symbol_analysis :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> symbol_analysis
  (** Input JSON data of type {!type:symbol_analysis}. *)

val symbol_analysis_of_string :
  string -> symbol_analysis
  (** Deserialize JSON data of type {!type:symbol_analysis}. *)

val write_upload_subproject_symbol_analysis_params :
  Buffer.t -> upload_subproject_symbol_analysis_params -> unit
  (** Output a JSON value of type {!type:upload_subproject_symbol_analysis_params}. *)

val string_of_upload_subproject_symbol_analysis_params :
  ?len:int -> upload_subproject_symbol_analysis_params -> string
  (** Serialize a value of type {!type:upload_subproject_symbol_analysis_params}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_upload_subproject_symbol_analysis_params :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> upload_subproject_symbol_analysis_params
  (** Input JSON data of type {!type:upload_subproject_symbol_analysis_params}. *)

val upload_subproject_symbol_analysis_params_of_string :
  string -> upload_subproject_symbol_analysis_params
  (** Deserialize JSON data of type {!type:upload_subproject_symbol_analysis_params}. *)

val write_unresolved_reason :
  Buffer.t -> unresolved_reason -> unit
  (** Output a JSON value of type {!type:unresolved_reason}. *)

val string_of_unresolved_reason :
  ?len:int -> unresolved_reason -> string
  (** Serialize a value of type {!type:unresolved_reason}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unresolved_reason :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unresolved_reason
  (** Input JSON data of type {!type:unresolved_reason}. *)

val unresolved_reason_of_string :
  string -> unresolved_reason
  (** Deserialize JSON data of type {!type:unresolved_reason}. *)

val write_subproject :
  Buffer.t -> subproject -> unit
  (** Output a JSON value of type {!type:subproject}. *)

val string_of_subproject :
  ?len:int -> subproject -> string
  (** Serialize a value of type {!type:subproject}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_subproject :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> subproject
  (** Input JSON data of type {!type:subproject}. *)

val subproject_of_string :
  string -> subproject
  (** Deserialize JSON data of type {!type:subproject}. *)

val write_sca_parser_name :
  Buffer.t -> sca_parser_name -> unit
  (** Output a JSON value of type {!type:sca_parser_name}. *)

val string_of_sca_parser_name :
  ?len:int -> sca_parser_name -> string
  (** Serialize a value of type {!type:sca_parser_name}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sca_parser_name :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sca_parser_name
  (** Input JSON data of type {!type:sca_parser_name}. *)

val sca_parser_name_of_string :
  string -> sca_parser_name
  (** Deserialize JSON data of type {!type:sca_parser_name}. *)

val write_resource_inaccessible :
  Buffer.t -> resource_inaccessible -> unit
  (** Output a JSON value of type {!type:resource_inaccessible}. *)

val string_of_resource_inaccessible :
  ?len:int -> resource_inaccessible -> string
  (** Serialize a value of type {!type:resource_inaccessible}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resource_inaccessible :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resource_inaccessible
  (** Input JSON data of type {!type:resource_inaccessible}. *)

val resource_inaccessible_of_string :
  string -> resource_inaccessible
  (** Deserialize JSON data of type {!type:resource_inaccessible}. *)

val write_resolution_cmd_failed :
  Buffer.t -> resolution_cmd_failed -> unit
  (** Output a JSON value of type {!type:resolution_cmd_failed}. *)

val string_of_resolution_cmd_failed :
  ?len:int -> resolution_cmd_failed -> string
  (** Serialize a value of type {!type:resolution_cmd_failed}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolution_cmd_failed :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolution_cmd_failed
  (** Input JSON data of type {!type:resolution_cmd_failed}. *)

val resolution_cmd_failed_of_string :
  string -> resolution_cmd_failed
  (** Deserialize JSON data of type {!type:resolution_cmd_failed}. *)

val write_resolution_error_kind :
  Buffer.t -> resolution_error_kind -> unit
  (** Output a JSON value of type {!type:resolution_error_kind}. *)

val string_of_resolution_error_kind :
  ?len:int -> resolution_error_kind -> string
  (** Serialize a value of type {!type:resolution_error_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolution_error_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolution_error_kind
  (** Input JSON data of type {!type:resolution_error_kind}. *)

val resolution_error_kind_of_string :
  string -> resolution_error_kind
  (** Deserialize JSON data of type {!type:resolution_error_kind}. *)

val write_sca_resolution_error :
  Buffer.t -> sca_resolution_error -> unit
  (** Output a JSON value of type {!type:sca_resolution_error}. *)

val string_of_sca_resolution_error :
  ?len:int -> sca_resolution_error -> string
  (** Serialize a value of type {!type:sca_resolution_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sca_resolution_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sca_resolution_error
  (** Input JSON data of type {!type:sca_resolution_error}. *)

val sca_resolution_error_of_string :
  string -> sca_resolution_error
  (** Deserialize JSON data of type {!type:sca_resolution_error}. *)

val write_dependency_parser_error :
  Buffer.t -> dependency_parser_error -> unit
  (** Output a JSON value of type {!type:dependency_parser_error}. *)

val string_of_dependency_parser_error :
  ?len:int -> dependency_parser_error -> string
  (** Serialize a value of type {!type:dependency_parser_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_parser_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_parser_error
  (** Input JSON data of type {!type:dependency_parser_error}. *)

val dependency_parser_error_of_string :
  string -> dependency_parser_error
  (** Deserialize JSON data of type {!type:dependency_parser_error}. *)

val write_sca_error :
  Buffer.t -> sca_error -> unit
  (** Output a JSON value of type {!type:sca_error}. *)

val string_of_sca_error :
  ?len:int -> sca_error -> string
  (** Serialize a value of type {!type:sca_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sca_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sca_error
  (** Input JSON data of type {!type:sca_error}. *)

val sca_error_of_string :
  string -> sca_error
  (** Deserialize JSON data of type {!type:sca_error}. *)

val write_unresolved_subproject :
  Buffer.t -> unresolved_subproject -> unit
  (** Output a JSON value of type {!type:unresolved_subproject}. *)

val string_of_unresolved_subproject :
  ?len:int -> unresolved_subproject -> string
  (** Serialize a value of type {!type:unresolved_subproject}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unresolved_subproject :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unresolved_subproject
  (** Input JSON data of type {!type:unresolved_subproject}. *)

val unresolved_subproject_of_string :
  string -> unresolved_subproject
  (** Deserialize JSON data of type {!type:unresolved_subproject}. *)

val write_snippet :
  Buffer.t -> snippet -> unit
  (** Output a JSON value of type {!type:snippet}. *)

val string_of_snippet :
  ?len:int -> snippet -> string
  (** Serialize a value of type {!type:snippet}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_snippet :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> snippet
  (** Input JSON data of type {!type:snippet}. *)

val snippet_of_string :
  string -> snippet
  (** Deserialize JSON data of type {!type:snippet}. *)

val write_killing_parent_kind :
  Buffer.t -> killing_parent_kind -> unit
  (** Output a JSON value of type {!type:killing_parent_kind}. *)

val string_of_killing_parent_kind :
  ?len:int -> killing_parent_kind -> string
  (** Serialize a value of type {!type:killing_parent_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_killing_parent_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> killing_parent_kind
  (** Input JSON data of type {!type:killing_parent_kind}. *)

val killing_parent_kind_of_string :
  string -> killing_parent_kind
  (** Deserialize JSON data of type {!type:killing_parent_kind}. *)

val write_killing_parent :
  Buffer.t -> killing_parent -> unit
  (** Output a JSON value of type {!type:killing_parent}. *)

val string_of_killing_parent :
  ?len:int -> killing_parent -> string
  (** Serialize a value of type {!type:killing_parent}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_killing_parent :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> killing_parent
  (** Input JSON data of type {!type:killing_parent}. *)

val killing_parent_of_string :
  string -> killing_parent
  (** Deserialize JSON data of type {!type:killing_parent}. *)

val write_unexpected_no_match_diagnosis_kind :
  Buffer.t -> unexpected_no_match_diagnosis_kind -> unit
  (** Output a JSON value of type {!type:unexpected_no_match_diagnosis_kind}. *)

val string_of_unexpected_no_match_diagnosis_kind :
  ?len:int -> unexpected_no_match_diagnosis_kind -> string
  (** Serialize a value of type {!type:unexpected_no_match_diagnosis_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unexpected_no_match_diagnosis_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unexpected_no_match_diagnosis_kind
  (** Input JSON data of type {!type:unexpected_no_match_diagnosis_kind}. *)

val unexpected_no_match_diagnosis_kind_of_string :
  string -> unexpected_no_match_diagnosis_kind
  (** Deserialize JSON data of type {!type:unexpected_no_match_diagnosis_kind}. *)

val write_unexpected_no_match_diagnosis :
  Buffer.t -> unexpected_no_match_diagnosis -> unit
  (** Output a JSON value of type {!type:unexpected_no_match_diagnosis}. *)

val string_of_unexpected_no_match_diagnosis :
  ?len:int -> unexpected_no_match_diagnosis -> string
  (** Serialize a value of type {!type:unexpected_no_match_diagnosis}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unexpected_no_match_diagnosis :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unexpected_no_match_diagnosis
  (** Input JSON data of type {!type:unexpected_no_match_diagnosis}. *)

val unexpected_no_match_diagnosis_of_string :
  string -> unexpected_no_match_diagnosis
  (** Deserialize JSON data of type {!type:unexpected_no_match_diagnosis}. *)

val write_originating_node_kind :
  Buffer.t -> originating_node_kind -> unit
  (** Output a JSON value of type {!type:originating_node_kind}. *)

val string_of_originating_node_kind :
  ?len:int -> originating_node_kind -> string
  (** Serialize a value of type {!type:originating_node_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_originating_node_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> originating_node_kind
  (** Input JSON data of type {!type:originating_node_kind}. *)

val originating_node_kind_of_string :
  string -> originating_node_kind
  (** Deserialize JSON data of type {!type:originating_node_kind}. *)

val write_unexpected_match_diagnosis :
  Buffer.t -> unexpected_match_diagnosis -> unit
  (** Output a JSON value of type {!type:unexpected_match_diagnosis}. *)

val string_of_unexpected_match_diagnosis :
  ?len:int -> unexpected_match_diagnosis -> string
  (** Serialize a value of type {!type:unexpected_match_diagnosis}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_unexpected_match_diagnosis :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> unexpected_match_diagnosis
  (** Input JSON data of type {!type:unexpected_match_diagnosis}. *)

val unexpected_match_diagnosis_of_string :
  string -> unexpected_match_diagnosis
  (** Deserialize JSON data of type {!type:unexpected_match_diagnosis}. *)

val write_triage_ignored :
  Buffer.t -> triage_ignored -> unit
  (** Output a JSON value of type {!type:triage_ignored}. *)

val string_of_triage_ignored :
  ?len:int -> triage_ignored -> string
  (** Serialize a value of type {!type:triage_ignored}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_triage_ignored :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> triage_ignored
  (** Input JSON data of type {!type:triage_ignored}. *)

val triage_ignored_of_string :
  string -> triage_ignored
  (** Deserialize JSON data of type {!type:triage_ignored}. *)

val write_transitive_finding :
  Buffer.t -> transitive_finding -> unit
  (** Output a JSON value of type {!type:transitive_finding}. *)

val string_of_transitive_finding :
  ?len:int -> transitive_finding -> string
  (** Serialize a value of type {!type:transitive_finding}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_transitive_finding :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> transitive_finding
  (** Input JSON data of type {!type:transitive_finding}. *)

val transitive_finding_of_string :
  string -> transitive_finding
  (** Deserialize JSON data of type {!type:transitive_finding}. *)

val write_downloaded_dependency :
  Buffer.t -> downloaded_dependency -> unit
  (** Output a JSON value of type {!type:downloaded_dependency}. *)

val string_of_downloaded_dependency :
  ?len:int -> downloaded_dependency -> string
  (** Serialize a value of type {!type:downloaded_dependency}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_downloaded_dependency :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> downloaded_dependency
  (** Input JSON data of type {!type:downloaded_dependency}. *)

val downloaded_dependency_of_string :
  string -> downloaded_dependency
  (** Deserialize JSON data of type {!type:downloaded_dependency}. *)

val write_resolved_dependency :
  Buffer.t -> resolved_dependency -> unit
  (** Output a JSON value of type {!type:resolved_dependency}. *)

val string_of_resolved_dependency :
  ?len:int -> resolved_dependency -> string
  (** Serialize a value of type {!type:resolved_dependency}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolved_dependency :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolved_dependency
  (** Input JSON data of type {!type:resolved_dependency}. *)

val resolved_dependency_of_string :
  string -> resolved_dependency
  (** Deserialize JSON data of type {!type:resolved_dependency}. *)

val write_transitive_reachability_filter_params :
  Buffer.t -> transitive_reachability_filter_params -> unit
  (** Output a JSON value of type {!type:transitive_reachability_filter_params}. *)

val string_of_transitive_reachability_filter_params :
  ?len:int -> transitive_reachability_filter_params -> string
  (** Serialize a value of type {!type:transitive_reachability_filter_params}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_transitive_reachability_filter_params :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> transitive_reachability_filter_params
  (** Input JSON data of type {!type:transitive_reachability_filter_params}. *)

val transitive_reachability_filter_params_of_string :
  string -> transitive_reachability_filter_params
  (** Deserialize JSON data of type {!type:transitive_reachability_filter_params}. *)

val write_tr_cache_match_result :
  Buffer.t -> tr_cache_match_result -> unit
  (** Output a JSON value of type {!type:tr_cache_match_result}. *)

val string_of_tr_cache_match_result :
  ?len:int -> tr_cache_match_result -> string
  (** Serialize a value of type {!type:tr_cache_match_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tr_cache_match_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tr_cache_match_result
  (** Input JSON data of type {!type:tr_cache_match_result}. *)

val tr_cache_match_result_of_string :
  string -> tr_cache_match_result
  (** Deserialize JSON data of type {!type:tr_cache_match_result}. *)

val write_tr_cache_key :
  Buffer.t -> tr_cache_key -> unit
  (** Output a JSON value of type {!type:tr_cache_key}. *)

val string_of_tr_cache_key :
  ?len:int -> tr_cache_key -> string
  (** Serialize a value of type {!type:tr_cache_key}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tr_cache_key :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tr_cache_key
  (** Input JSON data of type {!type:tr_cache_key}. *)

val tr_cache_key_of_string :
  string -> tr_cache_key
  (** Deserialize JSON data of type {!type:tr_cache_key}. *)

val write_tr_query_cache_response :
  Buffer.t -> tr_query_cache_response -> unit
  (** Output a JSON value of type {!type:tr_query_cache_response}. *)

val string_of_tr_query_cache_response :
  ?len:int -> tr_query_cache_response -> string
  (** Serialize a value of type {!type:tr_query_cache_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tr_query_cache_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tr_query_cache_response
  (** Input JSON data of type {!type:tr_query_cache_response}. *)

val tr_query_cache_response_of_string :
  string -> tr_query_cache_response
  (** Deserialize JSON data of type {!type:tr_query_cache_response}. *)

val write_tr_query_cache_request :
  Buffer.t -> tr_query_cache_request -> unit
  (** Output a JSON value of type {!type:tr_query_cache_request}. *)

val string_of_tr_query_cache_request :
  ?len:int -> tr_query_cache_request -> string
  (** Serialize a value of type {!type:tr_query_cache_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tr_query_cache_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tr_query_cache_request
  (** Input JSON data of type {!type:tr_query_cache_request}. *)

val tr_query_cache_request_of_string :
  string -> tr_query_cache_request
  (** Deserialize JSON data of type {!type:tr_query_cache_request}. *)

val write_tr_add_cache_request :
  Buffer.t -> tr_add_cache_request -> unit
  (** Output a JSON value of type {!type:tr_add_cache_request}. *)

val string_of_tr_add_cache_request :
  ?len:int -> tr_add_cache_request -> string
  (** Serialize a value of type {!type:tr_add_cache_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tr_add_cache_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tr_add_cache_request
  (** Input JSON data of type {!type:tr_add_cache_request}. *)

val tr_add_cache_request_of_string :
  string -> tr_add_cache_request
  (** Deserialize JSON data of type {!type:tr_add_cache_request}. *)

val write_todo :
  Buffer.t -> todo -> unit
  (** Output a JSON value of type {!type:todo}. *)

val string_of_todo :
  ?len:int -> todo -> string
  (** Serialize a value of type {!type:todo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_todo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> todo
  (** Input JSON data of type {!type:todo}. *)

val todo_of_string :
  string -> todo
  (** Deserialize JSON data of type {!type:todo}. *)

val write_matching_diagnosis :
  Buffer.t -> matching_diagnosis -> unit
  (** Output a JSON value of type {!type:matching_diagnosis}. *)

val string_of_matching_diagnosis :
  ?len:int -> matching_diagnosis -> string
  (** Serialize a value of type {!type:matching_diagnosis}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_matching_diagnosis :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> matching_diagnosis
  (** Input JSON data of type {!type:matching_diagnosis}. *)

val matching_diagnosis_of_string :
  string -> matching_diagnosis
  (** Deserialize JSON data of type {!type:matching_diagnosis}. *)

val write_expected_reported :
  Buffer.t -> expected_reported -> unit
  (** Output a JSON value of type {!type:expected_reported}. *)

val string_of_expected_reported :
  ?len:int -> expected_reported -> string
  (** Serialize a value of type {!type:expected_reported}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_expected_reported :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> expected_reported
  (** Input JSON data of type {!type:expected_reported}. *)

val expected_reported_of_string :
  string -> expected_reported
  (** Deserialize JSON data of type {!type:expected_reported}. *)

val write_rule_result :
  Buffer.t -> rule_result -> unit
  (** Output a JSON value of type {!type:rule_result}. *)

val string_of_rule_result :
  ?len:int -> rule_result -> string
  (** Serialize a value of type {!type:rule_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rule_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rule_result
  (** Input JSON data of type {!type:rule_result}. *)

val rule_result_of_string :
  string -> rule_result
  (** Deserialize JSON data of type {!type:rule_result}. *)

val write_fixtest_result :
  Buffer.t -> fixtest_result -> unit
  (** Output a JSON value of type {!type:fixtest_result}. *)

val string_of_fixtest_result :
  ?len:int -> fixtest_result -> string
  (** Serialize a value of type {!type:fixtest_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_fixtest_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> fixtest_result
  (** Input JSON data of type {!type:fixtest_result}. *)

val fixtest_result_of_string :
  string -> fixtest_result
  (** Deserialize JSON data of type {!type:fixtest_result}. *)

val write_config_error_reason :
  Buffer.t -> config_error_reason -> unit
  (** Output a JSON value of type {!type:config_error_reason}. *)

val string_of_config_error_reason :
  ?len:int -> config_error_reason -> string
  (** Serialize a value of type {!type:config_error_reason}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_config_error_reason :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> config_error_reason
  (** Input JSON data of type {!type:config_error_reason}. *)

val config_error_reason_of_string :
  string -> config_error_reason
  (** Deserialize JSON data of type {!type:config_error_reason}. *)

val write_config_error :
  Buffer.t -> config_error -> unit
  (** Output a JSON value of type {!type:config_error}. *)

val string_of_config_error :
  ?len:int -> config_error -> string
  (** Serialize a value of type {!type:config_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_config_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> config_error
  (** Input JSON data of type {!type:config_error}. *)

val config_error_of_string :
  string -> config_error
  (** Deserialize JSON data of type {!type:config_error}. *)

val write_checks :
  Buffer.t -> checks -> unit
  (** Output a JSON value of type {!type:checks}. *)

val string_of_checks :
  ?len:int -> checks -> string
  (** Serialize a value of type {!type:checks}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_checks :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> checks
  (** Input JSON data of type {!type:checks}. *)

val checks_of_string :
  string -> checks
  (** Deserialize JSON data of type {!type:checks}. *)

val write_tests_result :
  Buffer.t -> tests_result -> unit
  (** Output a JSON value of type {!type:tests_result}. *)

val string_of_tests_result :
  ?len:int -> tests_result -> string
  (** Serialize a value of type {!type:tests_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tests_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tests_result
  (** Input JSON data of type {!type:tests_result}. *)

val tests_result_of_string :
  string -> tests_result
  (** Deserialize JSON data of type {!type:tests_result}. *)

val write_project_root :
  Buffer.t -> project_root -> unit
  (** Output a JSON value of type {!type:project_root}. *)

val string_of_project_root :
  ?len:int -> project_root -> string
  (** Serialize a value of type {!type:project_root}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_project_root :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> project_root
  (** Input JSON data of type {!type:project_root}. *)

val project_root_of_string :
  string -> project_root
  (** Deserialize JSON data of type {!type:project_root}. *)

val write_targeting_conf :
  Buffer.t -> targeting_conf -> unit
  (** Output a JSON value of type {!type:targeting_conf}. *)

val string_of_targeting_conf :
  ?len:int -> targeting_conf -> string
  (** Serialize a value of type {!type:targeting_conf}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_targeting_conf :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> targeting_conf
  (** Input JSON data of type {!type:targeting_conf}. *)

val targeting_conf_of_string :
  string -> targeting_conf
  (** Deserialize JSON data of type {!type:targeting_conf}. *)

val write_product :
  Buffer.t -> product -> unit
  (** Output a JSON value of type {!type:product}. *)

val string_of_product :
  ?len:int -> product -> string
  (** Serialize a value of type {!type:product}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_product :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> product
  (** Input JSON data of type {!type:product}. *)

val product_of_string :
  string -> product
  (** Deserialize JSON data of type {!type:product}. *)

val write_ppath :
  Buffer.t -> ppath -> unit
  (** Output a JSON value of type {!type:ppath}. *)

val string_of_ppath :
  ?len:int -> ppath -> string
  (** Serialize a value of type {!type:ppath}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ppath :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ppath
  (** Input JSON data of type {!type:ppath}. *)

val ppath_of_string :
  string -> ppath
  (** Deserialize JSON data of type {!type:ppath}. *)

val write_fppath :
  Buffer.t -> fppath -> unit
  (** Output a JSON value of type {!type:fppath}. *)

val string_of_fppath :
  ?len:int -> fppath -> string
  (** Serialize a value of type {!type:fppath}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_fppath :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> fppath
  (** Input JSON data of type {!type:fppath}. *)

val fppath_of_string :
  string -> fppath
  (** Deserialize JSON data of type {!type:fppath}. *)

val write_analyzer :
  Buffer.t -> analyzer -> unit
  (** Output a JSON value of type {!type:analyzer}. *)

val string_of_analyzer :
  ?len:int -> analyzer -> string
  (** Serialize a value of type {!type:analyzer}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_analyzer :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> analyzer
  (** Input JSON data of type {!type:analyzer}. *)

val analyzer_of_string :
  string -> analyzer
  (** Deserialize JSON data of type {!type:analyzer}. *)

val write_code_target :
  Buffer.t -> code_target -> unit
  (** Output a JSON value of type {!type:code_target}. *)

val string_of_code_target :
  ?len:int -> code_target -> string
  (** Serialize a value of type {!type:code_target}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_code_target :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> code_target
  (** Input JSON data of type {!type:code_target}. *)

val code_target_of_string :
  string -> code_target
  (** Deserialize JSON data of type {!type:code_target}. *)

val write_target :
  Buffer.t -> target -> unit
  (** Output a JSON value of type {!type:target}. *)

val string_of_target :
  ?len:int -> target -> string
  (** Serialize a value of type {!type:target}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_target :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> target
  (** Input JSON data of type {!type:target}. *)

val target_of_string :
  string -> target
  (** Deserialize JSON data of type {!type:target}. *)

val write_scanning_roots :
  Buffer.t -> scanning_roots -> unit
  (** Output a JSON value of type {!type:scanning_roots}. *)

val string_of_scanning_roots :
  ?len:int -> scanning_roots -> string
  (** Serialize a value of type {!type:scanning_roots}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scanning_roots :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scanning_roots
  (** Input JSON data of type {!type:scanning_roots}. *)

val scanning_roots_of_string :
  string -> scanning_roots
  (** Deserialize JSON data of type {!type:scanning_roots}. *)

val write_targets :
  Buffer.t -> targets -> unit
  (** Output a JSON value of type {!type:targets}. *)

val string_of_targets :
  ?len:int -> targets -> string
  (** Serialize a value of type {!type:targets}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_targets :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> targets
  (** Input JSON data of type {!type:targets}. *)

val targets_of_string :
  string -> targets
  (** Deserialize JSON data of type {!type:targets}. *)

val write_target_times :
  Buffer.t -> target_times -> unit
  (** Output a JSON value of type {!type:target_times}. *)

val string_of_target_times :
  ?len:int -> target_times -> string
  (** Serialize a value of type {!type:target_times}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_target_times :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> target_times
  (** Input JSON data of type {!type:target_times}. *)

val target_times_of_string :
  string -> target_times
  (** Deserialize JSON data of type {!type:target_times}. *)

val write_skip_reason :
  Buffer.t -> skip_reason -> unit
  (** Output a JSON value of type {!type:skip_reason}. *)

val string_of_skip_reason :
  ?len:int -> skip_reason -> string
  (** Serialize a value of type {!type:skip_reason}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_skip_reason :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> skip_reason
  (** Input JSON data of type {!type:skip_reason}. *)

val skip_reason_of_string :
  string -> skip_reason
  (** Deserialize JSON data of type {!type:skip_reason}. *)

val write_skipped_target :
  Buffer.t -> skipped_target -> unit
  (** Output a JSON value of type {!type:skipped_target}. *)

val string_of_skipped_target :
  ?len:int -> skipped_target -> string
  (** Serialize a value of type {!type:skipped_target}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_skipped_target :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> skipped_target
  (** Input JSON data of type {!type:skipped_target}. *)

val skipped_target_of_string :
  string -> skipped_target
  (** Deserialize JSON data of type {!type:skipped_target}. *)

val write_incompatible_rule :
  Buffer.t -> incompatible_rule -> unit
  (** Output a JSON value of type {!type:incompatible_rule}. *)

val string_of_incompatible_rule :
  ?len:int -> incompatible_rule -> string
  (** Serialize a value of type {!type:incompatible_rule}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_incompatible_rule :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> incompatible_rule
  (** Input JSON data of type {!type:incompatible_rule}. *)

val incompatible_rule_of_string :
  string -> incompatible_rule
  (** Deserialize JSON data of type {!type:incompatible_rule}. *)

val write_error_type :
  Buffer.t -> error_type -> unit
  (** Output a JSON value of type {!type:error_type}. *)

val string_of_error_type :
  ?len:int -> error_type -> string
  (** Serialize a value of type {!type:error_type}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_error_type :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> error_type
  (** Input JSON data of type {!type:error_type}. *)

val error_type_of_string :
  string -> error_type
  (** Deserialize JSON data of type {!type:error_type}. *)

val write_error_severity :
  Buffer.t -> error_severity -> unit
  (** Output a JSON value of type {!type:error_severity}. *)

val string_of_error_severity :
  ?len:int -> error_severity -> string
  (** Serialize a value of type {!type:error_severity}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_error_severity :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> error_severity
  (** Input JSON data of type {!type:error_severity}. *)

val error_severity_of_string :
  string -> error_severity
  (** Deserialize JSON data of type {!type:error_severity}. *)

val write_core_error :
  Buffer.t -> core_error -> unit
  (** Output a JSON value of type {!type:core_error}. *)

val string_of_core_error :
  ?len:int -> core_error -> string
  (** Serialize a value of type {!type:core_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_core_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> core_error
  (** Input JSON data of type {!type:core_error}. *)

val core_error_of_string :
  string -> core_error
  (** Deserialize JSON data of type {!type:core_error}. *)

val write_target_discovery_result :
  Buffer.t -> target_discovery_result -> unit
  (** Output a JSON value of type {!type:target_discovery_result}. *)

val string_of_target_discovery_result :
  ?len:int -> target_discovery_result -> string
  (** Serialize a value of type {!type:target_discovery_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_target_discovery_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> target_discovery_result
  (** Input JSON data of type {!type:target_discovery_result}. *)

val target_discovery_result_of_string :
  string -> target_discovery_result
  (** Deserialize JSON data of type {!type:target_discovery_result}. *)

val write_summary_stats :
  Buffer.t -> summary_stats -> unit
  (** Output a JSON value of type {!type:summary_stats}. *)

val string_of_summary_stats :
  ?len:int -> summary_stats -> string
  (** Serialize a value of type {!type:summary_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_summary_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> summary_stats
  (** Input JSON data of type {!type:summary_stats}. *)

val summary_stats_of_string :
  string -> summary_stats
  (** Deserialize JSON data of type {!type:summary_stats}. *)

val write_def_rule_time :
  Buffer.t -> def_rule_time -> unit
  (** Output a JSON value of type {!type:def_rule_time}. *)

val string_of_def_rule_time :
  ?len:int -> def_rule_time -> string
  (** Serialize a value of type {!type:def_rule_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_def_rule_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> def_rule_time
  (** Input JSON data of type {!type:def_rule_time}. *)

val def_rule_time_of_string :
  string -> def_rule_time
  (** Deserialize JSON data of type {!type:def_rule_time}. *)

val write_tainting_time :
  Buffer.t -> tainting_time -> unit
  (** Output a JSON value of type {!type:tainting_time}. *)

val string_of_tainting_time :
  ?len:int -> tainting_time -> string
  (** Serialize a value of type {!type:tainting_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tainting_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tainting_time
  (** Input JSON data of type {!type:tainting_time}. *)

val tainting_time_of_string :
  string -> tainting_time
  (** Deserialize JSON data of type {!type:tainting_time}. *)

val write_tag :
  Buffer.t -> tag -> unit
  (** Output a JSON value of type {!type:tag}. *)

val string_of_tag :
  ?len:int -> tag -> string
  (** Serialize a value of type {!type:tag}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_tag :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> tag
  (** Input JSON data of type {!type:tag}. *)

val tag_of_string :
  string -> tag
  (** Deserialize JSON data of type {!type:tag}. *)

val write_symbol_analysis_upload_response :
  Buffer.t -> symbol_analysis_upload_response -> unit
  (** Output a JSON value of type {!type:symbol_analysis_upload_response}. *)

val string_of_symbol_analysis_upload_response :
  ?len:int -> symbol_analysis_upload_response -> string
  (** Serialize a value of type {!type:symbol_analysis_upload_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_symbol_analysis_upload_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> symbol_analysis_upload_response
  (** Input JSON data of type {!type:symbol_analysis_upload_response}. *)

val symbol_analysis_upload_response_of_string :
  string -> symbol_analysis_upload_response
  (** Deserialize JSON data of type {!type:symbol_analysis_upload_response}. *)

val write_symbol_analysis_params :
  Buffer.t -> symbol_analysis_params -> unit
  (** Output a JSON value of type {!type:symbol_analysis_params}. *)

val string_of_symbol_analysis_params :
  ?len:int -> symbol_analysis_params -> string
  (** Serialize a value of type {!type:symbol_analysis_params}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_symbol_analysis_params :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> symbol_analysis_params
  (** Input JSON data of type {!type:symbol_analysis_params}. *)

val symbol_analysis_params_of_string :
  string -> symbol_analysis_params
  (** Deserialize JSON data of type {!type:symbol_analysis_params}. *)

val write_resolution_method :
  Buffer.t -> resolution_method -> unit
  (** Output a JSON value of type {!type:resolution_method}. *)

val string_of_resolution_method :
  ?len:int -> resolution_method -> string
  (** Serialize a value of type {!type:resolution_method}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolution_method :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolution_method
  (** Input JSON data of type {!type:resolution_method}. *)

val resolution_method_of_string :
  string -> resolution_method
  (** Deserialize JSON data of type {!type:resolution_method}. *)

val write_dependency_source_file_kind :
  Buffer.t -> dependency_source_file_kind -> unit
  (** Output a JSON value of type {!type:dependency_source_file_kind}. *)

val string_of_dependency_source_file_kind :
  ?len:int -> dependency_source_file_kind -> string
  (** Serialize a value of type {!type:dependency_source_file_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_source_file_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_source_file_kind
  (** Input JSON data of type {!type:dependency_source_file_kind}. *)

val dependency_source_file_kind_of_string :
  string -> dependency_source_file_kind
  (** Deserialize JSON data of type {!type:dependency_source_file_kind}. *)

val write_dependency_source_file :
  Buffer.t -> dependency_source_file -> unit
  (** Output a JSON value of type {!type:dependency_source_file}. *)

val string_of_dependency_source_file :
  ?len:int -> dependency_source_file -> string
  (** Serialize a value of type {!type:dependency_source_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_source_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_source_file
  (** Input JSON data of type {!type:dependency_source_file}. *)

val dependency_source_file_of_string :
  string -> dependency_source_file
  (** Deserialize JSON data of type {!type:dependency_source_file}. *)

val write_dependency_resolution_stats :
  Buffer.t -> dependency_resolution_stats -> unit
  (** Output a JSON value of type {!type:dependency_resolution_stats}. *)

val string_of_dependency_resolution_stats :
  ?len:int -> dependency_resolution_stats -> string
  (** Serialize a value of type {!type:dependency_resolution_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dependency_resolution_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dependency_resolution_stats
  (** Input JSON data of type {!type:dependency_resolution_stats}. *)

val dependency_resolution_stats_of_string :
  string -> dependency_resolution_stats
  (** Deserialize JSON data of type {!type:dependency_resolution_stats}. *)

val write_subproject_stats :
  Buffer.t -> subproject_stats -> unit
  (** Output a JSON value of type {!type:subproject_stats}. *)

val string_of_subproject_stats :
  ?len:int -> subproject_stats -> string
  (** Serialize a value of type {!type:subproject_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_subproject_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> subproject_stats
  (** Input JSON data of type {!type:subproject_stats}. *)

val subproject_stats_of_string :
  string -> subproject_stats
  (** Deserialize JSON data of type {!type:subproject_stats}. *)

val write_supply_chain_stats :
  Buffer.t -> supply_chain_stats -> unit
  (** Output a JSON value of type {!type:supply_chain_stats}. *)

val string_of_supply_chain_stats :
  ?len:int -> supply_chain_stats -> string
  (** Serialize a value of type {!type:supply_chain_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_supply_chain_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> supply_chain_stats
  (** Input JSON data of type {!type:supply_chain_stats}. *)

val supply_chain_stats_of_string :
  string -> supply_chain_stats
  (** Deserialize JSON data of type {!type:supply_chain_stats}. *)

val write_subproject_symbol_analysis_url_request :
  Buffer.t -> subproject_symbol_analysis_url_request -> unit
  (** Output a JSON value of type {!type:subproject_symbol_analysis_url_request}. *)

val string_of_subproject_symbol_analysis_url_request :
  ?len:int -> subproject_symbol_analysis_url_request -> string
  (** Serialize a value of type {!type:subproject_symbol_analysis_url_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_subproject_symbol_analysis_url_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> subproject_symbol_analysis_url_request
  (** Input JSON data of type {!type:subproject_symbol_analysis_url_request}. *)

val subproject_symbol_analysis_url_request_of_string :
  string -> subproject_symbol_analysis_url_request
  (** Deserialize JSON data of type {!type:subproject_symbol_analysis_url_request}. *)

val write_single_subproject_plan :
  Buffer.t -> single_subproject_plan -> unit
  (** Output a JSON value of type {!type:single_subproject_plan}. *)

val string_of_single_subproject_plan :
  ?len:int -> single_subproject_plan -> string
  (** Serialize a value of type {!type:single_subproject_plan}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_single_subproject_plan :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> single_subproject_plan
  (** Input JSON data of type {!type:single_subproject_plan}. *)

val single_subproject_plan_of_string :
  string -> single_subproject_plan
  (** Deserialize JSON data of type {!type:single_subproject_plan}. *)

val write_subproject_resolution_plan :
  Buffer.t -> subproject_resolution_plan -> unit
  (** Output a JSON value of type {!type:subproject_resolution_plan}. *)

val string_of_subproject_resolution_plan :
  ?len:int -> subproject_resolution_plan -> string
  (** Serialize a value of type {!type:subproject_resolution_plan}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_subproject_resolution_plan :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> subproject_resolution_plan
  (** Input JSON data of type {!type:subproject_resolution_plan}. *)

val subproject_resolution_plan_of_string :
  string -> subproject_resolution_plan
  (** Deserialize JSON data of type {!type:subproject_resolution_plan}. *)

val write_skipped_rule :
  Buffer.t -> skipped_rule -> unit
  (** Output a JSON value of type {!type:skipped_rule}. *)

val string_of_skipped_rule :
  ?len:int -> skipped_rule -> string
  (** Serialize a value of type {!type:skipped_rule}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_skipped_rule :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> skipped_rule
  (** Input JSON data of type {!type:skipped_rule}. *)

val skipped_rule_of_string :
  string -> skipped_rule
  (** Deserialize JSON data of type {!type:skipped_rule}. *)

val write_file_time :
  Buffer.t -> file_time -> unit
  (** Output a JSON value of type {!type:file_time}. *)

val string_of_file_time :
  ?len:int -> file_time -> string
  (** Serialize a value of type {!type:file_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_file_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> file_time
  (** Input JSON data of type {!type:file_time}. *)

val file_time_of_string :
  string -> file_time
  (** Deserialize JSON data of type {!type:file_time}. *)

val write_scanning_time :
  Buffer.t -> scanning_time -> unit
  (** Output a JSON value of type {!type:scanning_time}. *)

val string_of_scanning_time :
  ?len:int -> scanning_time -> string
  (** Serialize a value of type {!type:scanning_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scanning_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scanning_time
  (** Input JSON data of type {!type:scanning_time}. *)

val scanning_time_of_string :
  string -> scanning_time
  (** Deserialize JSON data of type {!type:scanning_time}. *)

val write_scanned_and_skipped :
  Buffer.t -> scanned_and_skipped -> unit
  (** Output a JSON value of type {!type:scanned_and_skipped}. *)

val string_of_scanned_and_skipped :
  ?len:int -> scanned_and_skipped -> string
  (** Serialize a value of type {!type:scanned_and_skipped}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scanned_and_skipped :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scanned_and_skipped
  (** Input JSON data of type {!type:scanned_and_skipped}. *)

val scanned_and_skipped_of_string :
  string -> scanned_and_skipped
  (** Deserialize JSON data of type {!type:scanned_and_skipped}. *)

val write_scan_info :
  Buffer.t -> scan_info -> unit
  (** Output a JSON value of type {!type:scan_info}. *)

val string_of_scan_info :
  ?len:int -> scan_info -> string
  (** Serialize a value of type {!type:scan_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scan_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scan_info
  (** Input JSON data of type {!type:scan_info}. *)

val scan_info_of_string :
  string -> scan_info
  (** Deserialize JSON data of type {!type:scan_info}. *)

val write_scan_configuration :
  Buffer.t -> scan_configuration -> unit
  (** Output a JSON value of type {!type:scan_configuration}. *)

val string_of_scan_configuration :
  ?len:int -> scan_configuration -> string
  (** Serialize a value of type {!type:scan_configuration}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scan_configuration :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scan_configuration
  (** Input JSON data of type {!type:scan_configuration}. *)

val scan_configuration_of_string :
  string -> scan_configuration
  (** Deserialize JSON data of type {!type:scan_configuration}. *)

val write_glob :
  Buffer.t -> glob -> unit
  (** Output a JSON value of type {!type:glob}. *)

val string_of_glob :
  ?len:int -> glob -> string
  (** Serialize a value of type {!type:glob}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_glob :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> glob
  (** Input JSON data of type {!type:glob}. *)

val glob_of_string :
  string -> glob
  (** Deserialize JSON data of type {!type:glob}. *)

val write_product_ignored_files :
  Buffer.t -> product_ignored_files -> unit
  (** Output a JSON value of type {!type:product_ignored_files}. *)

val string_of_product_ignored_files :
  ?len:int -> product_ignored_files -> string
  (** Serialize a value of type {!type:product_ignored_files}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_product_ignored_files :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> product_ignored_files
  (** Input JSON data of type {!type:product_ignored_files}. *)

val product_ignored_files_of_string :
  string -> product_ignored_files
  (** Deserialize JSON data of type {!type:product_ignored_files}. *)

val write_historical_configuration :
  Buffer.t -> historical_configuration -> unit
  (** Output a JSON value of type {!type:historical_configuration}. *)

val string_of_historical_configuration :
  ?len:int -> historical_configuration -> string
  (** Serialize a value of type {!type:historical_configuration}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_historical_configuration :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> historical_configuration
  (** Input JSON data of type {!type:historical_configuration}. *)

val historical_configuration_of_string :
  string -> historical_configuration
  (** Deserialize JSON data of type {!type:historical_configuration}. *)

val write_engine_configuration :
  Buffer.t -> engine_configuration -> unit
  (** Output a JSON value of type {!type:engine_configuration}. *)

val string_of_engine_configuration :
  ?len:int -> engine_configuration -> string
  (** Serialize a value of type {!type:engine_configuration}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_engine_configuration :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> engine_configuration
  (** Input JSON data of type {!type:engine_configuration}. *)

val engine_configuration_of_string :
  string -> engine_configuration
  (** Deserialize JSON data of type {!type:engine_configuration}. *)

val write_scan_response :
  Buffer.t -> scan_response -> unit
  (** Output a JSON value of type {!type:scan_response}. *)

val string_of_scan_response :
  ?len:int -> scan_response -> string
  (** Serialize a value of type {!type:scan_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scan_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scan_response
  (** Input JSON data of type {!type:scan_response}. *)

val scan_response_of_string :
  string -> scan_response
  (** Deserialize JSON data of type {!type:scan_response}. *)

val write_scan_metadata :
  Buffer.t -> scan_metadata -> unit
  (** Output a JSON value of type {!type:scan_metadata}. *)

val string_of_scan_metadata :
  ?len:int -> scan_metadata -> string
  (** Serialize a value of type {!type:scan_metadata}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scan_metadata :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scan_metadata
  (** Input JSON data of type {!type:scan_metadata}. *)

val scan_metadata_of_string :
  string -> scan_metadata
  (** Deserialize JSON data of type {!type:scan_metadata}. *)

val write_project_metadata :
  Buffer.t -> project_metadata -> unit
  (** Output a JSON value of type {!type:project_metadata}. *)

val string_of_project_metadata :
  ?len:int -> project_metadata -> string
  (** Serialize a value of type {!type:project_metadata}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_project_metadata :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> project_metadata
  (** Input JSON data of type {!type:project_metadata}. *)

val project_metadata_of_string :
  string -> project_metadata
  (** Deserialize JSON data of type {!type:project_metadata}. *)

val write_ci_config_from_repo :
  Buffer.t -> ci_config_from_repo -> unit
  (** Output a JSON value of type {!type:ci_config_from_repo}. *)

val string_of_ci_config_from_repo :
  ?len:int -> ci_config_from_repo -> string
  (** Serialize a value of type {!type:ci_config_from_repo}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_config_from_repo :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_config_from_repo
  (** Input JSON data of type {!type:ci_config_from_repo}. *)

val ci_config_from_repo_of_string :
  string -> ci_config_from_repo
  (** Deserialize JSON data of type {!type:ci_config_from_repo}. *)

val write_scan_request :
  Buffer.t -> scan_request -> unit
  (** Output a JSON value of type {!type:scan_request}. *)

val string_of_scan_request :
  ?len:int -> scan_request -> string
  (** Serialize a value of type {!type:scan_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scan_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scan_request
  (** Input JSON data of type {!type:scan_request}. *)

val scan_request_of_string :
  string -> scan_request
  (** Deserialize JSON data of type {!type:scan_request}. *)

val write_ci_env :
  Buffer.t -> ci_env -> unit
  (** Output a JSON value of type {!type:ci_env}. *)

val string_of_ci_env :
  ?len:int -> ci_env -> string
  (** Serialize a value of type {!type:ci_env}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_env :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_env
  (** Input JSON data of type {!type:ci_env}. *)

val ci_env_of_string :
  string -> ci_env
  (** Deserialize JSON data of type {!type:ci_env}. *)

val write_ci_config :
  Buffer.t -> ci_config -> unit
  (** Output a JSON value of type {!type:ci_config}. *)

val string_of_ci_config :
  ?len:int -> ci_config -> string
  (** Serialize a value of type {!type:ci_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_config
  (** Input JSON data of type {!type:ci_config}. *)

val ci_config_of_string :
  string -> ci_config
  (** Deserialize JSON data of type {!type:ci_config}. *)

val write_action :
  Buffer.t -> action -> unit
  (** Output a JSON value of type {!type:action}. *)

val string_of_action :
  ?len:int -> action -> string
  (** Serialize a value of type {!type:action}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_action :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> action
  (** Input JSON data of type {!type:action}. *)

val action_of_string :
  string -> action
  (** Deserialize JSON data of type {!type:action}. *)

val write_ci_config_from_cloud :
  Buffer.t -> ci_config_from_cloud -> unit
  (** Output a JSON value of type {!type:ci_config_from_cloud}. *)

val string_of_ci_config_from_cloud :
  ?len:int -> ci_config_from_cloud -> string
  (** Serialize a value of type {!type:ci_config_from_cloud}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_config_from_cloud :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_config_from_cloud
  (** Input JSON data of type {!type:ci_config_from_cloud}. *)

val ci_config_from_cloud_of_string :
  string -> ci_config_from_cloud
  (** Deserialize JSON data of type {!type:ci_config_from_cloud}. *)

val write_scan_config :
  Buffer.t -> scan_config -> unit
  (** Output a JSON value of type {!type:scan_config}. *)

val string_of_scan_config :
  ?len:int -> scan_config -> string
  (** Serialize a value of type {!type:scan_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_scan_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> scan_config
  (** Input JSON data of type {!type:scan_config}. *)

val scan_config_of_string :
  string -> scan_config
  (** Deserialize JSON data of type {!type:scan_config}. *)

val write_sarif_format :
  Buffer.t -> sarif_format -> unit
  (** Output a JSON value of type {!type:sarif_format}. *)

val string_of_sarif_format :
  ?len:int -> sarif_format -> string
  (** Serialize a value of type {!type:sarif_format}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_sarif_format :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> sarif_format
  (** Input JSON data of type {!type:sarif_format}. *)

val sarif_format_of_string :
  string -> sarif_format
  (** Deserialize JSON data of type {!type:sarif_format}. *)

val write_engine_kind :
  Buffer.t -> engine_kind -> unit
  (** Output a JSON value of type {!type:engine_kind}. *)

val string_of_engine_kind :
  ?len:int -> engine_kind -> string
  (** Serialize a value of type {!type:engine_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_engine_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> engine_kind
  (** Input JSON data of type {!type:engine_kind}. *)

val engine_kind_of_string :
  string -> engine_kind
  (** Deserialize JSON data of type {!type:engine_kind}. *)

val write_rule_id_and_engine_kind :
  Buffer.t -> rule_id_and_engine_kind -> unit
  (** Output a JSON value of type {!type:rule_id_and_engine_kind}. *)

val string_of_rule_id_and_engine_kind :
  ?len:int -> rule_id_and_engine_kind -> string
  (** Serialize a value of type {!type:rule_id_and_engine_kind}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rule_id_and_engine_kind :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rule_id_and_engine_kind
  (** Input JSON data of type {!type:rule_id_and_engine_kind}. *)

val rule_id_and_engine_kind_of_string :
  string -> rule_id_and_engine_kind
  (** Deserialize JSON data of type {!type:rule_id_and_engine_kind}. *)

val write_resolve_dependencies_params :
  Buffer.t -> resolve_dependencies_params -> unit
  (** Output a JSON value of type {!type:resolve_dependencies_params}. *)

val string_of_resolve_dependencies_params :
  ?len:int -> resolve_dependencies_params -> string
  (** Serialize a value of type {!type:resolve_dependencies_params}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolve_dependencies_params :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolve_dependencies_params
  (** Input JSON data of type {!type:resolve_dependencies_params}. *)

val resolve_dependencies_params_of_string :
  string -> resolve_dependencies_params
  (** Deserialize JSON data of type {!type:resolve_dependencies_params}. *)

val write_profiling_entry :
  Buffer.t -> profiling_entry -> unit
  (** Output a JSON value of type {!type:profiling_entry}. *)

val string_of_profiling_entry :
  ?len:int -> profiling_entry -> string
  (** Serialize a value of type {!type:profiling_entry}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_profiling_entry :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> profiling_entry
  (** Input JSON data of type {!type:profiling_entry}. *)

val profiling_entry_of_string :
  string -> profiling_entry
  (** Deserialize JSON data of type {!type:profiling_entry}. *)

val write_prefiltering_stats :
  Buffer.t -> prefiltering_stats -> unit
  (** Output a JSON value of type {!type:prefiltering_stats}. *)

val string_of_prefiltering_stats :
  ?len:int -> prefiltering_stats -> string
  (** Serialize a value of type {!type:prefiltering_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_prefiltering_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> prefiltering_stats
  (** Input JSON data of type {!type:prefiltering_stats}. *)

val prefiltering_stats_of_string :
  string -> prefiltering_stats
  (** Deserialize JSON data of type {!type:prefiltering_stats}. *)

val write_parsing_time :
  Buffer.t -> parsing_time -> unit
  (** Output a JSON value of type {!type:parsing_time}. *)

val string_of_parsing_time :
  ?len:int -> parsing_time -> string
  (** Serialize a value of type {!type:parsing_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_parsing_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> parsing_time
  (** Input JSON data of type {!type:parsing_time}. *)

val parsing_time_of_string :
  string -> parsing_time
  (** Deserialize JSON data of type {!type:parsing_time}. *)

val write_file_rule_time :
  Buffer.t -> file_rule_time -> unit
  (** Output a JSON value of type {!type:file_rule_time}. *)

val string_of_file_rule_time :
  ?len:int -> file_rule_time -> string
  (** Serialize a value of type {!type:file_rule_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_file_rule_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> file_rule_time
  (** Input JSON data of type {!type:file_rule_time}. *)

val file_rule_time_of_string :
  string -> file_rule_time
  (** Deserialize JSON data of type {!type:file_rule_time}. *)

val write_matching_time :
  Buffer.t -> matching_time -> unit
  (** Output a JSON value of type {!type:matching_time}. *)

val string_of_matching_time :
  ?len:int -> matching_time -> string
  (** Serialize a value of type {!type:matching_time}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_matching_time :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> matching_time
  (** Input JSON data of type {!type:matching_time}. *)

val matching_time_of_string :
  string -> matching_time
  (** Deserialize JSON data of type {!type:matching_time}. *)

val write_profile :
  Buffer.t -> profile -> unit
  (** Output a JSON value of type {!type:profile}. *)

val string_of_profile :
  ?len:int -> profile -> string
  (** Serialize a value of type {!type:profile}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_profile :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> profile
  (** Input JSON data of type {!type:profile}. *)

val profile_of_string :
  string -> profile
  (** Deserialize JSON data of type {!type:profile}. *)

val write_output_format :
  Buffer.t -> output_format -> unit
  (** Output a JSON value of type {!type:output_format}. *)

val string_of_output_format :
  ?len:int -> output_format -> string
  (** Serialize a value of type {!type:output_format}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_output_format :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> output_format
  (** Input JSON data of type {!type:output_format}. *)

val output_format_of_string :
  string -> output_format
  (** Deserialize JSON data of type {!type:output_format}. *)

val write_mcp_scan_results :
  Buffer.t -> mcp_scan_results -> unit
  (** Output a JSON value of type {!type:mcp_scan_results}. *)

val string_of_mcp_scan_results :
  ?len:int -> mcp_scan_results -> string
  (** Serialize a value of type {!type:mcp_scan_results}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_mcp_scan_results :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> mcp_scan_results
  (** Input JSON data of type {!type:mcp_scan_results}. *)

val mcp_scan_results_of_string :
  string -> mcp_scan_results
  (** Deserialize JSON data of type {!type:mcp_scan_results}. *)

val write_format_context :
  Buffer.t -> format_context -> unit
  (** Output a JSON value of type {!type:format_context}. *)

val string_of_format_context :
  ?len:int -> format_context -> string
  (** Serialize a value of type {!type:format_context}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_format_context :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> format_context
  (** Input JSON data of type {!type:format_context}. *)

val format_context_of_string :
  string -> format_context
  (** Deserialize JSON data of type {!type:format_context}. *)

val write_error_span :
  Buffer.t -> error_span -> unit
  (** Output a JSON value of type {!type:error_span}. *)

val string_of_error_span :
  ?len:int -> error_span -> string
  (** Serialize a value of type {!type:error_span}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_error_span :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> error_span
  (** Input JSON data of type {!type:error_span}. *)

val error_span_of_string :
  string -> error_span
  (** Deserialize JSON data of type {!type:error_span}. *)

val write_edit :
  Buffer.t -> edit -> unit
  (** Output a JSON value of type {!type:edit}. *)

val string_of_edit :
  ?len:int -> edit -> string
  (** Serialize a value of type {!type:edit}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_edit :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> edit
  (** Input JSON data of type {!type:edit}. *)

val edit_of_string :
  string -> edit
  (** Deserialize JSON data of type {!type:edit}. *)

val write_dump_rule_partitions_params :
  Buffer.t -> dump_rule_partitions_params -> unit
  (** Output a JSON value of type {!type:dump_rule_partitions_params}. *)

val string_of_dump_rule_partitions_params :
  ?len:int -> dump_rule_partitions_params -> string
  (** Serialize a value of type {!type:dump_rule_partitions_params}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dump_rule_partitions_params :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dump_rule_partitions_params
  (** Input JSON data of type {!type:dump_rule_partitions_params}. *)

val dump_rule_partitions_params_of_string :
  string -> dump_rule_partitions_params
  (** Deserialize JSON data of type {!type:dump_rule_partitions_params}. *)

val write_cli_output_subproject_info :
  Buffer.t -> cli_output_subproject_info -> unit
  (** Output a JSON value of type {!type:cli_output_subproject_info}. *)

val string_of_cli_output_subproject_info :
  ?len:int -> cli_output_subproject_info -> string
  (** Serialize a value of type {!type:cli_output_subproject_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cli_output_subproject_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cli_output_subproject_info
  (** Input JSON data of type {!type:cli_output_subproject_info}. *)

val cli_output_subproject_info_of_string :
  string -> cli_output_subproject_info
  (** Deserialize JSON data of type {!type:cli_output_subproject_info}. *)

val write_cli_error :
  Buffer.t -> cli_error -> unit
  (** Output a JSON value of type {!type:cli_error}. *)

val string_of_cli_error :
  ?len:int -> cli_error -> string
  (** Serialize a value of type {!type:cli_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cli_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cli_error
  (** Input JSON data of type {!type:cli_error}. *)

val cli_error_of_string :
  string -> cli_error
  (** Deserialize JSON data of type {!type:cli_error}. *)

val write_cli_output :
  Buffer.t -> cli_output -> unit
  (** Output a JSON value of type {!type:cli_output}. *)

val string_of_cli_output :
  ?len:int -> cli_output -> string
  (** Serialize a value of type {!type:cli_output}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cli_output :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cli_output
  (** Input JSON data of type {!type:cli_output}. *)

val cli_output_of_string :
  string -> cli_output
  (** Deserialize JSON data of type {!type:cli_output}. *)

val write_apply_fixes_params :
  Buffer.t -> apply_fixes_params -> unit
  (** Output a JSON value of type {!type:apply_fixes_params}. *)

val string_of_apply_fixes_params :
  ?len:int -> apply_fixes_params -> string
  (** Serialize a value of type {!type:apply_fixes_params}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_apply_fixes_params :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> apply_fixes_params
  (** Input JSON data of type {!type:apply_fixes_params}. *)

val apply_fixes_params_of_string :
  string -> apply_fixes_params
  (** Deserialize JSON data of type {!type:apply_fixes_params}. *)

val write_function_call :
  Buffer.t -> function_call -> unit
  (** Output a JSON value of type {!type:function_call}. *)

val string_of_function_call :
  ?len:int -> function_call -> string
  (** Serialize a value of type {!type:function_call}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_function_call :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> function_call
  (** Input JSON data of type {!type:function_call}. *)

val function_call_of_string :
  string -> function_call
  (** Deserialize JSON data of type {!type:function_call}. *)

val write_rpc_call :
  Buffer.t -> rpc_call -> unit
  (** Output a JSON value of type {!type:rpc_call}. *)

val string_of_rpc_call :
  ?len:int -> rpc_call -> string
  (** Serialize a value of type {!type:rpc_call}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_rpc_call :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> rpc_call
  (** Input JSON data of type {!type:rpc_call}. *)

val rpc_call_of_string :
  string -> rpc_call
  (** Deserialize JSON data of type {!type:rpc_call}. *)

val write_resolved_subproject :
  Buffer.t -> resolved_subproject -> unit
  (** Output a JSON value of type {!type:resolved_subproject}. *)

val string_of_resolved_subproject :
  ?len:int -> resolved_subproject -> string
  (** Serialize a value of type {!type:resolved_subproject}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolved_subproject :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolved_subproject
  (** Input JSON data of type {!type:resolved_subproject}. *)

val resolved_subproject_of_string :
  string -> resolved_subproject
  (** Deserialize JSON data of type {!type:resolved_subproject}. *)

val write_resolution_result :
  Buffer.t -> resolution_result -> unit
  (** Output a JSON value of type {!type:resolution_result}. *)

val string_of_resolution_result :
  ?len:int -> resolution_result -> string
  (** Serialize a value of type {!type:resolution_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_resolution_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> resolution_result
  (** Input JSON data of type {!type:resolution_result}. *)

val resolution_result_of_string :
  string -> resolution_result
  (** Deserialize JSON data of type {!type:resolution_result}. *)

val write_polling_information :
  Buffer.t -> polling_information -> unit
  (** Output a JSON value of type {!type:polling_information}. *)

val string_of_polling_information :
  ?len:int -> polling_information -> string
  (** Serialize a value of type {!type:polling_information}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_polling_information :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> polling_information
  (** Input JSON data of type {!type:polling_information}. *)

val polling_information_of_string :
  string -> polling_information
  (** Deserialize JSON data of type {!type:polling_information}. *)

val write_parsing_stats :
  Buffer.t -> parsing_stats -> unit
  (** Output a JSON value of type {!type:parsing_stats}. *)

val string_of_parsing_stats :
  ?len:int -> parsing_stats -> string
  (** Serialize a value of type {!type:parsing_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_parsing_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> parsing_stats
  (** Input JSON data of type {!type:parsing_stats}. *)

val parsing_stats_of_string :
  string -> parsing_stats
  (** Deserialize JSON data of type {!type:parsing_stats}. *)

val write_finding_hashes :
  Buffer.t -> finding_hashes -> unit
  (** Output a JSON value of type {!type:finding_hashes}. *)

val string_of_finding_hashes :
  ?len:int -> finding_hashes -> string
  (** Serialize a value of type {!type:finding_hashes}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_finding_hashes :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> finding_hashes
  (** Input JSON data of type {!type:finding_hashes}. *)

val finding_hashes_of_string :
  string -> finding_hashes
  (** Deserialize JSON data of type {!type:finding_hashes}. *)

val write_finding :
  Buffer.t -> finding -> unit
  (** Output a JSON value of type {!type:finding}. *)

val string_of_finding :
  ?len:int -> finding -> string
  (** Serialize a value of type {!type:finding}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_finding :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> finding
  (** Input JSON data of type {!type:finding}. *)

val finding_of_string :
  string -> finding
  (** Deserialize JSON data of type {!type:finding}. *)

val write_contributor :
  Buffer.t -> contributor -> unit
  (** Output a JSON value of type {!type:contributor}. *)

val string_of_contributor :
  ?len:int -> contributor -> string
  (** Serialize a value of type {!type:contributor}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contributor :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contributor
  (** Input JSON data of type {!type:contributor}. *)

val contributor_of_string :
  string -> contributor
  (** Deserialize JSON data of type {!type:contributor}. *)

val write_contribution :
  Buffer.t -> contribution -> unit
  (** Output a JSON value of type {!type:contribution}. *)

val string_of_contribution :
  ?len:int -> contribution -> string
  (** Serialize a value of type {!type:contribution}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contribution :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contribution
  (** Input JSON data of type {!type:contribution}. *)

val contribution_of_string :
  string -> contribution
  (** Deserialize JSON data of type {!type:contribution}. *)

val write_contributions :
  Buffer.t -> contributions -> unit
  (** Output a JSON value of type {!type:contributions}. *)

val string_of_contributions :
  ?len:int -> contributions -> string
  (** Serialize a value of type {!type:contributions}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_contributions :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> contributions
  (** Input JSON data of type {!type:contributions}. *)

val contributions_of_string :
  string -> contributions
  (** Deserialize JSON data of type {!type:contributions}. *)

val write_ci_scan_metadata :
  Buffer.t -> ci_scan_metadata -> unit
  (** Output a JSON value of type {!type:ci_scan_metadata}. *)

val string_of_ci_scan_metadata :
  ?len:int -> ci_scan_metadata -> string
  (** Serialize a value of type {!type:ci_scan_metadata}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_metadata :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_metadata
  (** Input JSON data of type {!type:ci_scan_metadata}. *)

val ci_scan_metadata_of_string :
  string -> ci_scan_metadata
  (** Deserialize JSON data of type {!type:ci_scan_metadata}. *)

val write_ci_scan_dependencies :
  Buffer.t -> ci_scan_dependencies -> unit
  (** Output a JSON value of type {!type:ci_scan_dependencies}. *)

val string_of_ci_scan_dependencies :
  ?len:int -> ci_scan_dependencies -> string
  (** Serialize a value of type {!type:ci_scan_dependencies}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_dependencies :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_dependencies
  (** Input JSON data of type {!type:ci_scan_dependencies}. *)

val ci_scan_dependencies_of_string :
  string -> ci_scan_dependencies
  (** Deserialize JSON data of type {!type:ci_scan_dependencies}. *)

val write_ci_scan_results :
  Buffer.t -> ci_scan_results -> unit
  (** Output a JSON value of type {!type:ci_scan_results}. *)

val string_of_ci_scan_results :
  ?len:int -> ci_scan_results -> string
  (** Serialize a value of type {!type:ci_scan_results}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_results :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_results
  (** Input JSON data of type {!type:ci_scan_results}. *)

val ci_scan_results_of_string :
  string -> ci_scan_results
  (** Deserialize JSON data of type {!type:ci_scan_results}. *)

val write_ci_scan_failure :
  Buffer.t -> ci_scan_failure -> unit
  (** Output a JSON value of type {!type:ci_scan_failure}. *)

val string_of_ci_scan_failure :
  ?len:int -> ci_scan_failure -> string
  (** Serialize a value of type {!type:ci_scan_failure}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_failure :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_failure
  (** Input JSON data of type {!type:ci_scan_failure}. *)

val ci_scan_failure_of_string :
  string -> ci_scan_failure
  (** Deserialize JSON data of type {!type:ci_scan_failure}. *)

val write_ci_scan_complete_stats :
  Buffer.t -> ci_scan_complete_stats -> unit
  (** Output a JSON value of type {!type:ci_scan_complete_stats}. *)

val string_of_ci_scan_complete_stats :
  ?len:int -> ci_scan_complete_stats -> string
  (** Serialize a value of type {!type:ci_scan_complete_stats}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_complete_stats :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_complete_stats
  (** Input JSON data of type {!type:ci_scan_complete_stats}. *)

val ci_scan_complete_stats_of_string :
  string -> ci_scan_complete_stats
  (** Deserialize JSON data of type {!type:ci_scan_complete_stats}. *)

val write_ci_scan_complete :
  Buffer.t -> ci_scan_complete -> unit
  (** Output a JSON value of type {!type:ci_scan_complete}. *)

val string_of_ci_scan_complete :
  ?len:int -> ci_scan_complete -> string
  (** Serialize a value of type {!type:ci_scan_complete}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_complete :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_complete
  (** Input JSON data of type {!type:ci_scan_complete}. *)

val ci_scan_complete_of_string :
  string -> ci_scan_complete
  (** Deserialize JSON data of type {!type:ci_scan_complete}. *)

val write_partial_scan_result :
  Buffer.t -> partial_scan_result -> unit
  (** Output a JSON value of type {!type:partial_scan_result}. *)

val string_of_partial_scan_result :
  ?len:int -> partial_scan_result -> string
  (** Serialize a value of type {!type:partial_scan_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_partial_scan_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> partial_scan_result
  (** Input JSON data of type {!type:partial_scan_result}. *)

val partial_scan_result_of_string :
  string -> partial_scan_result
  (** Deserialize JSON data of type {!type:partial_scan_result}. *)

val write_match_based_id :
  Buffer.t -> match_based_id -> unit
  (** Output a JSON value of type {!type:match_based_id}. *)

val string_of_match_based_id :
  ?len:int -> match_based_id -> string
  (** Serialize a value of type {!type:match_based_id}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_match_based_id :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> match_based_id
  (** Input JSON data of type {!type:match_based_id}. *)

val match_based_id_of_string :
  string -> match_based_id
  (** Deserialize JSON data of type {!type:match_based_id}. *)

val write_has_features :
  Buffer.t -> has_features -> unit
  (** Output a JSON value of type {!type:has_features}. *)

val string_of_has_features :
  ?len:int -> has_features -> string
  (** Serialize a value of type {!type:has_features}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_has_features :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> has_features
  (** Input JSON data of type {!type:has_features}. *)

val has_features_of_string :
  string -> has_features
  (** Deserialize JSON data of type {!type:has_features}. *)

val write_get_config_response_status :
  Buffer.t -> get_config_response_status -> unit
  (** Output a JSON value of type {!type:get_config_response_status}. *)

val string_of_get_config_response_status :
  ?len:int -> get_config_response_status -> string
  (** Serialize a value of type {!type:get_config_response_status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_get_config_response_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> get_config_response_status
  (** Input JSON data of type {!type:get_config_response_status}. *)

val get_config_response_status_of_string :
  string -> get_config_response_status
  (** Deserialize JSON data of type {!type:get_config_response_status}. *)

val write_get_config_response_v2 :
  Buffer.t -> get_config_response_v2 -> unit
  (** Output a JSON value of type {!type:get_config_response_v2}. *)

val string_of_get_config_response_v2 :
  ?len:int -> get_config_response_v2 -> string
  (** Serialize a value of type {!type:get_config_response_v2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_get_config_response_v2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> get_config_response_v2
  (** Input JSON data of type {!type:get_config_response_v2}. *)

val get_config_response_v2_of_string :
  string -> get_config_response_v2
  (** Deserialize JSON data of type {!type:get_config_response_v2}. *)

val write_apply_fixes_return :
  Buffer.t -> apply_fixes_return -> unit
  (** Output a JSON value of type {!type:apply_fixes_return}. *)

val string_of_apply_fixes_return :
  ?len:int -> apply_fixes_return -> string
  (** Serialize a value of type {!type:apply_fixes_return}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_apply_fixes_return :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> apply_fixes_return
  (** Input JSON data of type {!type:apply_fixes_return}. *)

val apply_fixes_return_of_string :
  string -> apply_fixes_return
  (** Deserialize JSON data of type {!type:apply_fixes_return}. *)

val write_function_return :
  Buffer.t -> function_return -> unit
  (** Output a JSON value of type {!type:function_return}. *)

val string_of_function_return :
  ?len:int -> function_return -> string
  (** Serialize a value of type {!type:function_return}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_function_return :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> function_return
  (** Input JSON data of type {!type:function_return}. *)

val function_return_of_string :
  string -> function_return
  (** Deserialize JSON data of type {!type:function_return}. *)

val write_function_result :
  Buffer.t -> function_result -> unit
  (** Output a JSON value of type {!type:function_result}. *)

val string_of_function_result :
  ?len:int -> function_result -> string
  (** Serialize a value of type {!type:function_result}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_function_result :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> function_result
  (** Input JSON data of type {!type:function_result}. *)

val function_result_of_string :
  string -> function_result
  (** Deserialize JSON data of type {!type:function_result}. *)

val write_features :
  Buffer.t -> features -> unit
  (** Output a JSON value of type {!type:features}. *)

val string_of_features :
  ?len:int -> features -> string
  (** Serialize a value of type {!type:features}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_features :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> features
  (** Input JSON data of type {!type:features}. *)

val features_of_string :
  string -> features
  (** Deserialize JSON data of type {!type:features}. *)

val write_diff_file :
  Buffer.t -> diff_file -> unit
  (** Output a JSON value of type {!type:diff_file}. *)

val string_of_diff_file :
  ?len:int -> diff_file -> string
  (** Serialize a value of type {!type:diff_file}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_diff_file :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> diff_file
  (** Input JSON data of type {!type:diff_file}. *)

val diff_file_of_string :
  string -> diff_file
  (** Deserialize JSON data of type {!type:diff_file}. *)

val write_diff_files :
  Buffer.t -> diff_files -> unit
  (** Output a JSON value of type {!type:diff_files}. *)

val string_of_diff_files :
  ?len:int -> diff_files -> string
  (** Serialize a value of type {!type:diff_files}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_diff_files :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> diff_files
  (** Input JSON data of type {!type:diff_files}. *)

val diff_files_of_string :
  string -> diff_files
  (** Deserialize JSON data of type {!type:diff_files}. *)

val write_deployment_config :
  Buffer.t -> deployment_config -> unit
  (** Output a JSON value of type {!type:deployment_config}. *)

val string_of_deployment_config :
  ?len:int -> deployment_config -> string
  (** Serialize a value of type {!type:deployment_config}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_deployment_config :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> deployment_config
  (** Input JSON data of type {!type:deployment_config}. *)

val deployment_config_of_string :
  string -> deployment_config
  (** Deserialize JSON data of type {!type:deployment_config}. *)

val write_deployment_response :
  Buffer.t -> deployment_response -> unit
  (** Output a JSON value of type {!type:deployment_response}. *)

val string_of_deployment_response :
  ?len:int -> deployment_response -> string
  (** Serialize a value of type {!type:deployment_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_deployment_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> deployment_response
  (** Input JSON data of type {!type:deployment_response}. *)

val deployment_response_of_string :
  string -> deployment_response
  (** Deserialize JSON data of type {!type:deployment_response}. *)

val write_create_scan_response_v2 :
  Buffer.t -> create_scan_response_v2 -> unit
  (** Output a JSON value of type {!type:create_scan_response_v2}. *)

val string_of_create_scan_response_v2 :
  ?len:int -> create_scan_response_v2 -> string
  (** Serialize a value of type {!type:create_scan_response_v2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_create_scan_response_v2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> create_scan_response_v2
  (** Input JSON data of type {!type:create_scan_response_v2}. *)

val create_scan_response_v2_of_string :
  string -> create_scan_response_v2
  (** Deserialize JSON data of type {!type:create_scan_response_v2}. *)

val write_create_scan_request_v2 :
  Buffer.t -> create_scan_request_v2 -> unit
  (** Output a JSON value of type {!type:create_scan_request_v2}. *)

val string_of_create_scan_request_v2 :
  ?len:int -> create_scan_request_v2 -> string
  (** Serialize a value of type {!type:create_scan_request_v2}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_create_scan_request_v2 :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> create_scan_request_v2
  (** Input JSON data of type {!type:create_scan_request_v2}. *)

val create_scan_request_v2_of_string :
  string -> create_scan_request_v2
  (** Deserialize JSON data of type {!type:create_scan_request_v2}. *)

val write_core_output_extra :
  Buffer.t -> core_output_extra -> unit
  (** Output a JSON value of type {!type:core_output_extra}. *)

val string_of_core_output_extra :
  ?len:int -> core_output_extra -> string
  (** Serialize a value of type {!type:core_output_extra}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_core_output_extra :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> core_output_extra
  (** Input JSON data of type {!type:core_output_extra}. *)

val core_output_extra_of_string :
  string -> core_output_extra
  (** Deserialize JSON data of type {!type:core_output_extra}. *)

val write_core_output :
  Buffer.t -> core_output -> unit
  (** Output a JSON value of type {!type:core_output}. *)

val string_of_core_output :
  ?len:int -> core_output -> string
  (** Serialize a value of type {!type:core_output}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_core_output :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> core_output
  (** Input JSON data of type {!type:core_output}. *)

val core_output_of_string :
  string -> core_output
  (** Deserialize JSON data of type {!type:core_output}. *)

val write_cli_output_extra :
  Buffer.t -> cli_output_extra -> unit
  (** Output a JSON value of type {!type:cli_output_extra}. *)

val string_of_cli_output_extra :
  ?len:int -> cli_output_extra -> string
  (** Serialize a value of type {!type:cli_output_extra}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_cli_output_extra :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> cli_output_extra
  (** Input JSON data of type {!type:cli_output_extra}. *)

val cli_output_extra_of_string :
  string -> cli_output_extra
  (** Deserialize JSON data of type {!type:cli_output_extra}. *)

val write_ci_scan_results_response_error :
  Buffer.t -> ci_scan_results_response_error -> unit
  (** Output a JSON value of type {!type:ci_scan_results_response_error}. *)

val string_of_ci_scan_results_response_error :
  ?len:int -> ci_scan_results_response_error -> string
  (** Serialize a value of type {!type:ci_scan_results_response_error}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_results_response_error :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_results_response_error
  (** Input JSON data of type {!type:ci_scan_results_response_error}. *)

val ci_scan_results_response_error_of_string :
  string -> ci_scan_results_response_error
  (** Deserialize JSON data of type {!type:ci_scan_results_response_error}. *)

val write_ci_scan_results_response :
  Buffer.t -> ci_scan_results_response -> unit
  (** Output a JSON value of type {!type:ci_scan_results_response}. *)

val string_of_ci_scan_results_response :
  ?len:int -> ci_scan_results_response -> string
  (** Serialize a value of type {!type:ci_scan_results_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_results_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_results_response
  (** Input JSON data of type {!type:ci_scan_results_response}. *)

val ci_scan_results_response_of_string :
  string -> ci_scan_results_response
  (** Deserialize JSON data of type {!type:ci_scan_results_response}. *)

val write_ci_scan_complete_response :
  Buffer.t -> ci_scan_complete_response -> unit
  (** Output a JSON value of type {!type:ci_scan_complete_response}. *)

val string_of_ci_scan_complete_response :
  ?len:int -> ci_scan_complete_response -> string
  (** Serialize a value of type {!type:ci_scan_complete_response}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_ci_scan_complete_response :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> ci_scan_complete_response
  (** Input JSON data of type {!type:ci_scan_complete_response}. *)

val ci_scan_complete_response_of_string :
  string -> ci_scan_complete_response
  (** Deserialize JSON data of type {!type:ci_scan_complete_response}. *)

