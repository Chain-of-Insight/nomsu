# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

## Dependencies

Nomsu's dependencies are [Lua 5.2 or later](https://www.lua.org/) (tested with version 5.2.4) (or [Luajit](http://luajit.org/) (tested with version 2.1.0)), [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) (`luarocks install lpeg`), and [Lua-Immutable](https://bitbucket.org/spilt/lua-immutable). Nomsu's compiler was written in [Moonscript](http://moonscript.org/), but all of the .moon files have been compiled into lua for convenience, so Moonscript is not a dependency.

## Usage

* To get a nomsu [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop), simply run `lua nomsu.lua`.
* To run a .nom file with nomsu code, run `lua nomsu.lua your_file.nom`. Or `lua nomsu.lua -` if reading from stdin.
* (Advanced/optional) To precompile a .nom file into lua, run `lua nomsu.lua -o <output_filename> <input_file>` to precompile your file into lua (by default, saved at `<your_file>.lua`). If you run the compiler with the "-O" (for Optimized) flag, the compiler will run the precompiled lua files directly, instead of parsing and compiling the .nom files before running them. This is not necessary, but it can speed things up if you precompile a bunch of code that involves macros or compile-time activity.
* More usage options are avilable via `lua nomsu.lua --help`.

## Layout

* `nomsu.moon`/`nomsu.lua` - **The nomsu compiler**. The compiler is written in [Moonscript](http://moonscript.org/), but a lua version of the file is provided in this repository for convenience.
* `nomsu_tree.moon`/`nomsu_tree.lua` - Datastructures used for Nomsu ASTs.
* `code_obj.moon`/`code_obj.lua` - Datastructures used for incrementally building generated code, while preserving code origins.
* `utils.lua` - A set of utility actions used by nomsu.moon.
* `consolecolors.lua` - Lua module that defines ANSI color codes for colored console output (used internally in nomsu.moon).
* `examples/how_do_i.nom` - A simple walkthrough of some of the features of nomsu, written in nomsu. **This is a good place to start.**
* `core/collections.nom` - Core library definitions related to collections, like lists and dictionaries.
* `core/control_flow.nom` - Core library definitions related to control flow, like `if` statements and `for` loops.
* `core/math.nom` - Core library definitions of math actions, like `sum of %` and `sqrt %`.
* `core/metaprogramming.nom` - Core library essential functionality for metaprogramming, including macros to define macros, rules to define rules, and so on. This is where the language pulls itself up by its hair out the swamps of nothingness.
* `core/operators.nom` - Core library definitions for operators like `=` and `+`.
* `core/scopes.nom` - Core library definitions for instantiating new variable scopes.
* `core/text.nom` - Core library definitions of text actions for stuff like concatenating strings.
* `lib/object.nom` - An optional library for doing object-oriented programming.
* `lib/file_hash.nom` - An optional library for hashing files and looking up files by hash.
* `lib/training_wheels.nom` - An optional library to define some syntactic sugar that makes Nomsu resemble other programming languages more closely. 
* `tests/*.nom` - A somewhat exhaustive set of minimalist tests for almost every language feature defined in `lib/*.nom`
* `compile_lib.sh` - script to precompile lib/\*.nom so they can be loaded faster. This is optional.

## Extra

There is a vim plugin for the language available in the [Vim Nomsu repository](https://bitbucket.org/squidarms/vim-nomsu/src). It is usually kept relatively up-to-date with Nomsu, but occasionally lags behind.
