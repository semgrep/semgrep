# DRuby [![Travis](https://img.shields.io/travis/stereobooster/diamondback-ruby.svg)](https://travis-ci.org/stereobooster/diamondback-ruby)

## Update

If you are looking for static type checker checkout [steep](https://github.com/soutaro/steep). It is written in Ruby which makes it much more approachable for Ruby developers.

Update 2: checkout [sorbet](https://sorbet.run/), a project by Stripe.

### Other resources

- [Academic writing on the Ruby programming language](http://rubybib.org/)
- [Types and Ruby Programming Language](https://speakerdeck.com/soutaro/types-and-ruby-programming-language)
- [rdl - type checking and contracts for Ruby](https://github.com/plum-umd/rdl),  [video](https://www.youtube.com/watch?v=buY54I7mEjA)
- [Automated Type Contracts Generation](https://github.com/JetBrains/ruby-type-inference), [video](https://www.youtube.com/watch?v=JS6m2gke0Ic)
- [Occurrence Typing Modulo Theories](https://arxiv.org/abs/1511.07033)

## Vision
- It is supposed to be a practical project rather than purely academical. To do this it should provide easy to install ruby gems (without requiring OCaml or C libs). This is doable, see how Flow compiles binaries for each platform and uploads to GitHub and npm.
- Everything that can be checked statically should be checked statically without dynamic check duplications. Statical type check should not have any negative impact on performance like this happens in [rdl](https://github.com/plum-umd/rdl) without `wrap: false`.
- Type-checker should be usable without modifying code like Flow does. Thought it can be not that effective in finding type errors without provided type signatures.
- There should be type signatures for standard libraries and top gems. Signatures should live in separate repository, so they can be reused by other projects, like [rdl](https://github.com/plum-umd/rdl) or [typed.rb](https://github.com/antoniogarrote/typed.rb) or [ruby 3 ;)](https://bugs.ruby-lang.org/issues/9999). Also, we can hope that community will contribute type signature, like in [DefinitelyTyped](https://github.com/DefinitelyTyped/DefinitelyTyped).
- Errors should be clear and helpful, [like in elm](http://elm-lang.org/blog/compiler-errors-for-humans).

## TODO
- fix tests
- write installation instruction for syck on Linux.
- fix ruby parser to support ruby 2.3, it works with 1.8 now
- setup Appveyor
- fix warnings and reverse `-warn-error` flag
- install to local dir instead of global
- setup distribution of gems from continuous integration, like Flow, does
- update dypgen, use [opam package](http://opam.ocaml.org/packages/dypgen/) instead of embedded version
- create an opam package file
- can we reuse [ruby/spec](ruby/spec) to generate type signatures?
- can we get type signatures from [ActiveRecord schema file](http://api.rubyonrails.org/classes/ActiveRecord/Schema.html), or [virtus](https://github.com/solnic/virtus), or [dry-types](http://dry-rb.org/gems/dry-types/)?
- can we reuse type signatures to improve speed by providing this info to JIT compiler?
- Replace Syck with other YAML implementation [camlyaml](https://github.com/Kakadu/camlyaml), [oyaml](https://github.com/mk270/oyaml).

## 1 Introduction

Ruby is a dynamically typed, object-oriented scripting language. Dynamic typing keeps the language flexible, allowing small, expressive programs to be written with little effort. However, software never stands still. As these programs evolve, the lack of static typing makes it increasingly difficult to maintain, extend, and reason about. Diamondback (DRuby) is a tool that augments Ruby with a static type system. DRuby’s type system was designed to handle common idioms used by Ruby programmers so that programs would not need to be changed in order to gain the benefits of static checks.

## 2 Building DRuby

DRuby is written in OCaml (OCaml 4.03.0 or higher is required). You can install OCaml on Mac OS X and Linux by following the instructions at [ocaml.org](https://ocaml.org/docs/install.html).

For example, on Ubuntu 16.04 and similar systems:

```
sudo apt-get install opam
eval `opam config env` # and add it to ~/.bashrc or ~/.zshrc
opam init --comp 4.03.0
opam switch 4.03.0
# TODO: fix this
# sudo apt-get install libsyck0-dev
```

On OS X, using the [brew package manager](http://brew.sh/):

```
brew install opam
eval `opam config env` # and add it to ~/.bashrc or ~/.zshrc
opam init --comp 4.03.0
opam switch 4.03.0
brew install syck
```

Then, restart your shell and install these additional libraries:

```
opam update
opam install -y ocamlfind ounit.1.1.2 omake getopt ocamlgraph
```

DRuby uses the OMake build system to build it from source. OMake is a make-like build system that includes both configuration and build rules. Thus, the build is broken into three steps: configuration, compilation, and installation. However, these steps can also be combined into a single command:

```
$ omake –config VAR1=arg1 VAR2=arg2 install
```

Here, we call `omake` with the `–config` flag to set the variables `VAR1` and `VAR2` and tell it to build the install target. The install target depends on the configuration and compilation targets and will, therefore, do all the necessary work. The call to sudo is needed if you do not have write permissions to your Ruby installation. Alternatively, DRuby can be installed outside of the system directories using the variables below. The current list of supported build variables are:

- `PREFIX` - The path used to provide defaults for other DRuby paths such as where to install the DRuby binary. Defaults to `/usr/local/`
- `BINDIR` - The install location of the DRuby binary. Defaults to PREFIX/bin
- `SYSCONFDIR` - The install location of the DRuby global configuration file, `druby.conf`. Defaults to `PREFIX`/etc
- `DESTDIR` - A staging directory for building packages of DRuby. Any necessary paths will be computed based on `PREFIX`, but the actual installation step will stage the installation into DESTDIR/PREFIX/....
- `RUBYLIB` - The location of your Ruby installation (library files). For example, `/usr/lib/ruby/`. You should not have to specify this under normal circumstances as it will be determined by the ’ruby’ executable found in your PATH.
- `RUBYSITEDIR` - The location of your Ruby installation’s site-lib directory. For example `/usr/lib/ruby/site-lib`. Like `RUBYLIB`, this should be automatically computed for you.
- `DRUBYSITELIB` - The install location for DRuby’s runtime ruby files. For example, `/usr/lib/ruby/site-lib/2.3/druby/`. Automatically computed based on `RUBYSITEDIR`
- `DRUBYLIB` - The install location for DRuby’s non-ruby files. For example, `/usr/lib/ruby/druby/`. Automatically computed based on `RUBYLIB`
- `SYCK` - The base location of the syck C library. For example, this should be specified as `/opt/local` if libsyck* is in `/opt/local/lib/`

**Note**: if you need to change these variables after attempting to build DRuby, you must specify the command line argument: `–config`. As an example, a user on OS X with brew installed would build and install DRuby with:

```
$ omake --config SYCK=`brew --cellar syck`/0.70 build_gem
```

DRuby has a couple of other useful targets:

- `config` - this target simply prints out the current configuration variables
- `.DEFAULT` - build the entire DRuby project and run the test suite. This is also the what is built if no target is specified.

## 3 Running DRuby

In order to make using DRuby as easy as possible, the DRuby binary can be used as a drop-in replacement for the Ruby interpreter. However, instead of interpreting the ruby code, it performs its static analysis. DRuby accepts a superset of the command line arguments that Ruby accepts. For instance, one can invoke:

```
$ druby -I my_dir -rmylib filename.rb
```

And DRuby will act appropriately (adding `my_dir` to its search path, and preloading the `mylib.rb` file). It is also possible to have DRuby invoke the regular Ruby interpreter after it has finished its analysis by using `--dr-run-ruby` option.

```
$ druby --dr-run-ruby filename -- args
```

All of DRuby’s command line arguments are prefixed by `–dr-`. To ensure any program arguments do not conflict with these arguments, it is recommended that you pass any arguments intended for your script after the `--` delimiter.

DRuby’s command line arguments are **FIXME**

These arguments can also be specified via configuration files. DRuby looks for options in the following order. A later declaration overrides a previous one:

- The global configuration file: `SYSCONFDIR/druby.conf`
- A per-user configuration file: `$HOME/.druby.conf`
- A file called `druby.conf` in the current working directory
- Arguments passed on the command line to druby

## 4 A small example

```ruby
args = ARGV
sum = 0
args.each do |v|
  sum += v
end
puts sum
```

Consider the example shown above which prints the sum of the command-line arguments. Try DRuby on this program to see whether this code is well-typed or not. You will get a similar error message as the following:

```
$ druby --dr-run-ruby first.rb
[ERROR] instance String used where Numeric expected
          It does not support methods ~, |, zero?, truncate, to_int, times, step,
          singleton_method_added, rpower, round, remainder, rdiv, quo, prec_i, prec_f,
          prec, power!, numerator, nonzero?, nan?, modulo, lcm, integer, infinite?,
          id2name, gcdlcm, gcd, floor, finite?, downto, divmod, div, denominator, coerce,
          chr, ceil, abs, ^, >>, /, -@, -, +@, **, &
  in typing method call sum.+ at ./first.rb:4 in typing method call sum.+ at ./first.rb:4
  in typing method call args.each at ./first.rb:3
  in assignment to ::ARGV at /opt/local/lib/ruby/druby/2.3/base_types.rb:2993
  in creating instance of String at /opt/local/lib/ruby/druby/2.3/base_types.rb:2993
  in typing expression %{args} at /opt/local/lib/ruby/druby/2.3/base_types.rb:2993
  in typing actual argument %{args} at /opt/local/lib/ruby/druby/2.3/base_types.rb:2993
  in typing ::Array.new at /opt/local/lib/ruby/druby/2.3/base_types.rb:2993
DRuby analysis complete.
now running Ruby...
ruby  first.rb  0
```

The short description (the top most sentence) of the error message says, `“instance String used where Numeric expected,”` and the context information (filenames and line numbers) first points to `sum.+` method at line 4 of `first.rb`. By inspecting the code, we realize that the command line arguments stored in `ARGV` are strings, not numbers and therefore trying to add a `String` to a `Fixnum` (sum) is the cause of the error. Change `sum += v` to `sum += v.to_i`, and run DRuby again. You will see that the program is now accepted. This program is now well-typed because to_i method converts the receiver to an integer.

The long description of the message (the message beginning from “It does not....” to right before the first context information), tells us more details about the error. Since Ruby uses structural typing (duck typing), DRuby models types in a similar manner. Despite having different class names (`String` vs. `Numeric`), DRuby tries to treat a String as a `Numeric`. It is safe to use a String in place of a Numeric if and only if String responds to at least as many methods as `Numeric`. However, this is not the case, and so DRuby reports each missing method.

Following this line is a list operations that DRuby performed or observed that it believes contributed to the error. As DRuby walks over a program, it generates structural constraints on objects and methods in order to discover any potential errors. These constraints form a graph, and an error represents some inconsistent path through this graph. Unfortunately, it can be fairly difficult to construct a reasonable error message from this graph in general, and DRuby currently uses a heuristic to attempt to produce a useful error message. However, this process is not perfect. Sometimes DRuby will print the same information twice, or print a series of constraints that are seemingly unrelated. If you encounter such a situation, please feel free to email us with the code that caused the error so we can try to improve the heuristic.

You may be wondering how DRuby is capable of typing `ARGV`, `Fixnum#+`, or `String#to_i` in the first place. Because Ruby core library is mostly written in C, we cannot directly analyze it. Instead, DRuby uses a stub file called, `base_types.rb`, which defines stub classes and modules along with appropriate type annotations. These annotations are essential to our analysis because they define the types of the classes and modules that are built into the interpreter. `base_types.rb` can be found in `druby/RUBY_VERSION` directory inside Ruby’s library directory (usually `/usr/lib/ruby`).

For more details on the type annotation language, refer to [Section 5](http://www.cs.umd.edu/projects/PL/druby/manual/manual.html#x1-100005).
