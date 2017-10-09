# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

## Dependencies

Nomsu's only dependencies are [Lua](https://www.lua.org/) (tested with version 5.2.4) (or [Luajit](http://luajit.org/) (tested with version 2.1.0)) and [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) (`luarocks install lpeg`). Nomsu's compiler was written in [Moonscript](http://moonscript.org/), but all of the .moon files have been compiled into lua for convenience, so Moonscript is not a dependency.

## Usage

* To get a nomsu [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop), simply run `lua nomsu.lua`.
* To run a .nom file with nomsu code, run `lua nomsu.lua your_file.nom`. Or `lua nomsu.lua -` if reading from stdin.
* (Advanced/optional) To precompile a .nom file into lua, either run `lua nomsu.lua your_file.nom -o output_file.lua` or simply `lua nomsu.lua -c your_file.nom` (which will default to outputting to `your_file.nom.lua`). It is not necessary to precompile .nom files, but it can speed things up if it's a file that gets loaded by `require "your_file.nom"` a lot. `require %` can either take a .nom file (it will automatically look for a .nom.lua precompiled version), or a .lua file.
* More usage options are avilable via `lua nomsu.lua --help`.

## Layout

* `nomsu.moon`/`nomsu.lua` - **The nomsu compiler**. The compiler is written in [Moonscript](http://moonscript.org/), but a lua version of the file is provided in this repository for convenience.
* `utils.moon`/`utils.lua` - A set of utility functions used by nomsu.moon. A lua version of this is also provided for convenience.
* `consolecolors.lua` - Lua module that defines ANSI color codes for colored console output (used internally in nomsu.moon).
* `examples/how_do_i.nom` - A simple walkthrough of some of the features of nomsu, written in nomsu. **This is a good place to start.**
* `examples/sample_code.nom` - Some additional sample nomsu code.
* `examples/sample_game.nom` - A sample game of Nomic, written in nomsu.
* `lib/collections.nom` - Core library definitions related to collections, like lists and dictionaries.
* `lib/control_flow.nom` - Core library definitions related to control flow, like `if` statements and `for` loops.
* `lib/core.nom` - Core library file that loads other core library files. Files are loaded in dependency order.
* `lib/metaprogramming.nom` - Core library essential functionality for metaprogramming, including macros to define macros, rules to define rules, and so on. This is where the language pulls itself up by its hair out the swamps of nothingness.
* `lib/moonscript.nom` - Core library definitions for writing in moonscript. This is optional, and requires Lua's "moon" package.
* `lib/operators.nom` - Core library definitions for operators like `=` and `+`.
* `lib/permissions.nom` - Core library definitions for rules that modify the permission levels of rules.
* `lib/plurals.nom` - Core library modulre that defines some simple rules to facilitate pluralizing and singularizing words. This is a bit frivolous, but can be useful for natural language.
* `lib/secrets.nom` - Core library module that allows for closure-like behavior in nomsu for hiding data within rule definitions that can't be accessed elsewhere.
* `lib/testing.nom` - Core library definitions for some testing facilities.
* `lib/utils.nom` - Core library definitions of common utility functions, like `sum of %` and `sqrt %`.
* `compile_lib.sh` - script to precompile lib/\*.nom so they can be loaded faster. This is optional.

## Extra

There is a vim plugin for the language available in the [Vim Nomsu repository](https://bitbucket.org/squidarms/vim-nomsu/src).
