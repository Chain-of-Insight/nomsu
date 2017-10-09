# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

## Dependencies

The language compiler was written in [Moonscript](http://moonscript.org/), using the
[LPEG library](http://www.inf.puc-rio.br/~roberto/lpeg/) for parsing. LPEG is a dependency,
so you need to install it in order to run the compiler. All of the moon files have been
compiled into lua for convenience, so Moonscript is not a dependency.

## How to use

In order to run a .nom file, run `lua nomsu.lua your_file.nom`. Code can also be compiled
into lua code directly, which still requires nomsu.lua as a dependency, but bypasses the
compilation phase when it runs. To compile, run `lua nomsu.lua your_file.nom output_file.lua`
which produces an output file which can be run with the command `lua output_file.lua`.

## Layout

* `nomsu.moon`/`nomsu.lua` - The nomsu compiler. The compiler is written in [Moonscript](http://moonscript.org/), but a lua version of the file is provided in this repository for convenience.
* `utils.moon`/`utils.lua` - A set of utility functions used by nomsu.moon. A lua version of this is also provided for convenience.
* `consolecolors.lua` - Lua module that defines ANSI color codes for colored console output (used internally in nomsu.moon).
* `examples/how_do_i.nom` - A simple walkthrough of some of the features of nomsu, written in nomsu. This is a good place to start.
* `examples/sample_code.nom` - Some additional sample nomsu code.
* `examples/sample_game.nom` - A sample game of Nomic, written in nomsu.
* `lib/collections.nom` - Core library definitions related to collections, like lists and dictionaries.
* `lib/control_flow.nom` - Core library definitions related to control flow, like `if` statements and `for` loops.
* `lib/core.nom` - Core library file that loads other core library files. Files are loaded in dependency order.
* `lib/metaprogramming.nom` - Core library essential functionality for metaprogramming, including macros to define macros, rules to define rules, and other mind-bending Escher-like paradoxes.
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
