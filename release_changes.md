## [1.181.0](https://github.com/semgrep/semgrep/releases/tag/v1.181.0) - 2026-10-14

### ### Changed

- `semgrep scan` now uses the same asynchronous configuration generation and retries as `semgrep ci` when fetching platform scan configurations. (CODE-9930)
- Python only: Supply Chain now reports every package declared in a `requirements.txt`, not only the ones pinned with `==`. A requirement with a version range (`requests>=2.0`, `numpy~=1.26`, `django==2.2.*`, `requests (>=2.0)`) is reported with the declared specifier as its version, and a bare name (`flask`) is reported with an empty version, so they appear in the dependency inventory and SBOM instead of being dropped with a "not pinned to an exact version" parse error. These dependencies have no definitive installed version, so for now they produce no Supply Chain findings. When there is no `requirements.in` manifest to consult, a range or bare-name line is reported as a direct dependency, since it was declared by hand rather than written by a resolver; exact pins keep their previous unknown transitivity. Local path and URL lines are no longer reported as parse errors, while a line whose leading token is not a valid package name (`requests$foo>=1`) is now reported as a parse error instead of being dropped silently. Other ecosystems and other Python manifest formats are unchanged. (SC-3487)
- pro: Various performance improvements for historical secrets scanning. (SCRT-983-historical-performance)
- go grammar update (v0.23.4):

  - Go target parsing now supports composite type-set constraints and shorthand union constraints on type parameters, preserving underlying-type markers and all union members. Literal generic rule-pattern syntax remains unchanged.
  - Go target parsing now supports embedded generic struct fields, including pointer and package-qualified embeddings, while retaining their type arguments.
  - Go target parsing now supports standalone generic function instantiations with multiple or composite type arguments, such as Pair[int, string] and Identity[[]int], including qualified function names.
  - Go target parsing now accepts Unicode decimal digits after the first character of an identifier, including Arabic-Indic and Devanagari digits.

  (go-v0.23.4)
- Hack grammar update:

  - Hack target parsing now supports capability lists on functions, methods, function types, lambdas, and anonymous functions, including empty lists and context-dependent capabilities.
  - Hack target parsing now supports context constants with capability bounds or initializers, preserving the distinction between super bounds, as bounds, and assigned capabilities. Unbounded abstract context constants retain their existing representation limitation.
  - Hack target parsing now supports concrete and abstract enum classes with typed members, plus qualified and unqualified enum class labels.
  - Hack target parsing now supports function and static-method references with empty or explicit type arguments, such as foo<> and MyClass::bar<int>.
  - Hack target parsing now preserves type arguments on calls and constructors such as foo<int>(), $obj->bar<int>(...), and new MyClass<int>(...), matching other languages' typed-call representation.
  - Hack target parsing now supports expression-tree literals such as Example`1 + 2`, preserving the visitor and body text. Embedded DSL bodies remain opaque rather than being parsed as Hack expressions.
  - Hack target parsing now supports file-level attributes such as <<file: SomeAttribute(...)>>, preserving attribute names and arguments.
  - Hack target parsing now supports internal visibility on methods and properties, keeping it distinct from public, protected, and private visibility.
  - Hack target parsing now accepts namespace-relative names such as namespace\run() and include/require member names in piped expressions.

  (hack-1a7ded90288189746c54861ac144ede97df95081)

### ### Fixed

- pro: Historical secrets scanning no longer aborts when preparatory `git gc`
  fails. (SCRT-982-historical-gc-nonfatal)
- Supply Chain: lockfileless Gradle resolution now tells the dependency graph
  plugin where to write its report, so builds that relocate their build
  directory resolve instead of failing. When the plugin writes no report, the
  scan reports a resolution error that includes the Gradle log instead of
  crashing on the missing file. (code-9953)
- Supply Chain: lockfileless Gradle and Maven scans now report incomplete
  dependency resolution when dependencies or transitive metadata cannot be
  resolved. Registry HTTP failures include the affected URL when available. (sc-3358)

### ### Infra/Release Changes

- Forward optional job context when registering CLI scans. (SMS-3333)
