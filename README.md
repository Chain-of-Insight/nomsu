# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

## Dependencies

Nomsu's only dependencies are [Lua 5.2 or later](https://www.lua.org/) (tested with version 5.2.4) (or [Luajit](http://luajit.org/) (tested with version 2.1.0)) and [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) (`luarocks install lpeg`). Nomsu's compiler was written in [Moonscript](http://moonscript.org/), but all of the .moon files have been compiled into lua for convenience, so Moonscript is not a dependency.

## Usage

* To get a Nomsu [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop), simply run `lua nomsu.lua` (or `luajit nomsu.lua`).
* To run a .nom file with Nomsu code, run `lua nomsu.lua your_file.nom`.
* (Advanced/optional) To precompile a .nom file into lua, run `lua nomsu.lua -c <input_file>` to precompile your file into lua (by default, saved in the same location, but with a `.lua` extension instead of `.nom`). If you run the compiler with the "-O" (for Optimized) flag, the compiler will run the precompiled lua files directly, instead of parsing and compiling the .nom files before running them. This is not necessary, but it can speed things up if you precompile a bunch of code that involves macros or compile-time activity.
* More usage options are avilable via `lua nomsu.lua --help`.

## Compiling/installing

If you enjoy Nomsu so much that you'd like to tinker with it or have it in your system path, there is an included Makefile. This is entirely optional, but you can run `make` to compile any modified .moon or .nom files into .lua files and produce an executable file called `nomsu` that can be run directly. `make test` will run the suite of tests. `make install` will install the compiler on your system. By default, the `nomsu` executable will be put in `/usr/local/bin` and all the necessary support files will be put into `/usr/local/share/nomsu/*`, but you can change the location with `make PREFIX=/your/path/here install` (or `NOMSU_BIN_DIR` and `NOMSU_LIB_DIR` to specify them separately). The default build options use the system default `lua`, but this can be changed via `make LUA=luajit` or `make LUA_BIN=/path/to/lua`. To uninstall, simply `make uninstall` with the same flags you used to install.

## File Layout

All `.moon` files have been precompiled into corresponding `.lua` files, so you don't need to have [Moonscript](http://moonscript.org/) installed to run the Nomsu compiler.

* `nomsu.moon` - The Nomsu command line runner. This handles launching the compiler and running the REPL.
* `nomsu.peg` - The [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) used to define Nomsu's syntax. The format of this file is a slightly modified version of the format accepted by LPEG's `re` module.
* `nomsu_compiler.moon` - **The actual Nomsu compiler**. This file can be imported and used without going through the regular command line interface (e.g. for applications that want to embed the compiler).
* `parser.moon` - The Nomsu parser. This file can also be imported and used directly for applications that only need to *parse* Nomsu, not compile it.
* `nomsu_tree.moon` - Datastructures used for Nomsu ASTs.
* `code_obj.moon` - Datastructures used for incrementally building generated code, while preserving code origins.
* `error_handling.moon` - The logic for producing good error messages within Lua that reference the Nomsu source code that led to them.
* `utils.lua` - A set of utility actions used by nomsu.moon.
* `uuid.lua` - A simple Universally Unique Identifier implementation (RFC 4122) used internally to give each object a randomized unique ID.
* `consolecolors.lua` - Lua module that defines ANSI color codes for colored console output (used internally in nomsu.moon).
* `examples/how_do_i.nom` - A simple walkthrough of some of the features of Nomsu, written in Nomsu code. **This is a good place to start.**
* `core/*.nom` - Core language definitions of stuff like control flow, operators, and metaprogramming, broken down into different files.
* `lib/*.nom` - Optional language libraries for stuff you might want, like interfacing with the OS, or doing Object Oriented Programming.
* `tests/*.nom` - A somewhat exhaustive set of minimalist tests for almost every language feature defined in `core/*.nom` and `lib/*.nom`
* `Makefile` - Rules for building/installing the compiler.
* `LICENSE` - The software license (MIT).
* `README.md` - This file.

## Extra

There is a vim plugin for the language available in the [Vim Nomsu repository](https://bitbucket.org/squidarms/vim-nomsu/src). It is usually kept relatively up-to-date with Nomsu, but occasionally lags behind.
