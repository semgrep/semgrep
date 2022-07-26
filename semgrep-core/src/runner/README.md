# Notes

## semgrep CLI vs semgrep core

Officially, `semgrep-core` is never run on its own. External users run
`semgrep`, which invokes `semgrep-core` with the appropriate rules and targets.
However, for development purposes it can be convenient to skip the wrapper.
Therefore, we also maintain some code paths that allow `semgrep-core` to take
in rules or patterns and perform its own file targeting. These will not always
return the same results as the equivalent `semgrep` run. To see valid inputs to
`semgrep-core`, see `semgrep-core --help`.

When invoked by `semgrep`, `semgrep-core` will always be passed `-rules` and
`-targets`. All the code relevant to `semgrep` runs will be found in branches
where the rules file and the targets file are not `""`.

While the `rules` file is just the collection of rules, the `targets` file
describes the mapping of targets to rules. See `targets` in `Input_to_core.atd`
for a description of its schema. `semgrep-core` follows the target-to-rule
mappings without validation or filtering.

## Performance

The main components of performance can generally be broken down into:

- rule parse time
- target parse time
- match time
  - pattern match time
  - formula evaluation time

The `-json_time` output includes timings for the three broad components. In
general (at least at the time this was written), target parsing takes the
longest. Matching does not usually take long, though it can when there are
many general patterns (e.g. `$X`) that require going deep (e.g. multiple `...`
or `<... $X ...>`). Also, getting the range of a large sub-AST can be slow
because it requires collecting all the tokens in that sub-AST. Most of the
rules that take longest are generic rules, however, because they run on
every file.

## Memory usage

Semgrep uses memory liberally. The main components of memory usage can
generally be broken down into:

- rule parsing
- targets file parsing
  - in addition to saving the targets, we seem to keep around the
    buffer used to parse it
- running each target (this should not accumulate)
  - parsing the target
  - saving the pattern results
  - collecting the output for each target
  - other memory used while matching
- saving the output for each target
- some unknown accumulation that happens while running targets but goes
  away if we run `Gc.full_major` after each target
- creating the final output

Of these, the memory used by rule parsing and targets file parsing has an
outsized impact on the memory used by multi-process runs because those two
structures are used by every process. (They are only read from, but they
end up getting copied into the process during garbage collection.) In
particular, the targets file is O(num_rules \* num_targets).

If we need to reduce memory further, some options include:

- Reduce memory used by rule parsing? (Have not investigated but from my
  personal profiling this seems too high)
- Further streamline the targets file and fix the problem with the buffer
- Investigate the unknown accumulation
- Compress non-necessary output (namely, `-json_time` is passed by default)
- Stream the outputs instead of collecting them and outputting them at the
  end

For profiling, a useful module is `mem_usage`. See physical memory used in
total and by each object with:

```ocaml
let phys_mem () = Mem_usage.prettify_bytes (Mem_usage.((info ()).process_physical_memory))
let obj_size o = Mem_usage.prettify_bytes (Obj.reachable_words (Obj.repr o) * 8)
```

Note that `obj_size` may inadvertently prevent an object from being freed.

Additionally, OCaml provides `Spacetime` for certain compilers. It works best
on Linux machines.

We also have had stack overflows. To avoid this, we use `Common.map`, which
is tail-recursive, instead of `List.map`
