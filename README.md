# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

## Dependencies

Nomsu's only dependencies are [Lua 5.2 or later](https://www.lua.org/) (tested with version 5.2.4) (or [Luajit](http://luajit.org/) (tested with version 2.1.0)) and [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) (`luarocks install lpeg`). Nomsu's compiler was written in [Moonscript](http://moonscript.org/), but all of the .moon files have been compiled into lua for convenience, so Moonscript is not a dependency.

## Usage

* To get a nomsu [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop), simply run `lua nomsu.lua`.
* To run a .nom file with nomsu code, run `lua nomsu.lua your_file.nom`.
* (Advanced/optional) To precompile a .nom file into lua, run `lua nomsu.lua -c <input_file>` to precompile your file into lua (by default, saved in the same location, but with a `.lua` extension instead of `.nom`). If you run the compiler with the "-O" (for Optimized) flag, the compiler will run the precompiled lua files directly, instead of parsing and compiling the .nom files before running them. This is not necessary, but it can speed things up if you precompile a bunch of code that involves macros or compile-time activity.
* More usage options are avilable via `lua nomsu.lua --help`.

## Layout

* `nomsu.moon`/`nomsu.lua` - **The nomsu compiler**. The compiler is written in [Moonscript](http://moonscript.org/), but a lua version of the file is provided in this repository for convenience.
* `nomsu_tree.moon`/`nomsu_tree.lua` - Datastructures used for Nomsu ASTs.
* `code_obj.moon`/`code_obj.lua` - Datastructures used for incrementally building generated code, while preserving code origins.
* `utils.lua` - A set of utility actions used by nomsu.moon.
* `consolecolors.lua` - Lua module that defines ANSI color codes for colored console output (used internally in nomsu.moon).
* `examples/how_do_i.nom` - A simple walkthrough of some of the features of nomsu, written in nomsu. **This is a good place to start.**
* `core/*.nom` - Core language definitions of stuff like control flow, operators, and metaprogramming, broken down into different files.
* `lib/*.nom` - Optional language libraries for stuff you might want, like interfacing with the OS, or doing Object Oriented Programming.
* `tests/*.nom` - A somewhat exhaustive set of minimalist tests for almost every language feature defined in `core/*.nom` and `lib/*.nom`
* `compile_lib.sh` - script to precompile `core/*.nom` and `lib/*.nom` so they can be loaded faster. This is optional.

## Extra

There is a vim plugin for the language available in the [Vim Nomsu repository](https://bitbucket.org/squidarms/vim-nomsu/src). It is usually kept relatively up-to-date with Nomsu, but occasionally lags behind.
