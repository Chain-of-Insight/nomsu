# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

## Dependencies

Nomsu's only dependencies are [Lua 5.1 or later](https://www.lua.org/) or [Luajit](http://luajit.org/) and [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) (`luarocks install lpeg`). Nomsu's compiler was written in [Moonscript](http://moonscript.org/), but all of the .moon files have been compiled into lua for convenience, so Moonscript is not a dependency.

## Usage

* To run Nomsu interactively (a [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop)), simply run `lua nomsu.lua` (or `luajit nomsu.lua`).
* To run a .nom file with Nomsu code, run `lua nomsu.lua your_file.nom`.
* The full usage options are avilable via `lua nomsu.lua --help`.

## Compiling/installing

If you enjoy Nomsu so much that you'd like to tinker with it or have it in your system path, there is an included Makefile. This is entirely optional, but you can run `make` to compile any modified .moon or .nom files into .lua files and produce an executable file called `nomsu` that can be run directly. `make test` will run the suite of tests. `sudo make install` will install the compiler on your system. By default, the `nomsu` executable will be put in `/usr/local/bin` and all the necessary support files will be put into `/usr/local/share/nomsu/*` and `/usr/local/lib/nomsu/*`, but you can change the location during installation or via the `PREFIX=/your/path/here` flag. The default build options use the system default `lua`, but this can be changed via `make LUA=luajit` or `make LUA_BIN=/path/to/lua`. To uninstall, simply `make uninstall` and provide the same installation path you did during installation.

## File Layout

All `.moon` files have been precompiled into corresponding `.lua` files, so you don't need to have [Moonscript](http://moonscript.org/) installed to run the Nomsu compiler.

* `nomsu` - A shell script that selects between different installed versions of Nomsu (using the `-V` flag). You can use this script to, for example, run `nomsu -V 1.2 your_script.nom` to run with the latest version of Nomsu that matches `1.2.?.?`. All flags and arguments are passed along to whichever Nomsu compiler is chosen.
* `nomsu.moon` - The source code for the Nomsu command line runner. This handles launching the compiler and running the REPL.
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

## Versioning

Nomsu uses the following versioning scheme: `[syntax version].[core library API version].[compiler internal API version].[lib/ API version]`. Which means:

* Any code that parses on `Nomsu X.a.b.c.d` will also parse on Nomsu `X.p.w.r.s`
* Any code that compiles on Nomsu `X.Y.a.b.c` will also compile on Nomsu `X.Y.p.q.r` and run without any differences, as long as it only depends on the behavior of the core library functions (i.e. stuff defined in `core/*.nom`), and doesn't mess with the compiler internals at all.
* Any code that compiles on Nomsu `X.Y.Z.a.b` will also compile on Nomsu `X.Y.Z.p.q` and run without any differences, even if it messes with the compiler internals, as long as it doesn't use anything from `lib/*.nom`.
* Any code that compiles on Nomsu `X.Y.Z.W` will also compile on any other Nomsu `X.Y.Z.W` and run without any differences, even if it uses stuff from `lib/*.nom`.

When Nomsu is istalled via `make install`, all of Nomsu's lua files and `core/*.nom` and `lib/*.nom` files are stored in `$PREFIX/lib/nomsu/$NOMSU_VERSION` and the Nomsu executable is installed to `$PREFIX/bin/nomsu$NOMSU_VERSION`, along with the file `nomsu` (the version-selection script), which goes to `$PREFIX/bin/nomsu`. When `make uninstall` is run, all those files are deleted (except for `nomsu`, if there are other versions installed).

To run different versions, use the `-V` flag, which will select the latest version matching the specified pattern. For example, if you have v1.0.0.0, v1.0.2.1, and 1.1.0.0 installed, then `nomsu` will run v1.1.0.0, `nomsu -V 1.0` will run v1.0.2.1, and `nomsu -V 1.0.0.0` will run v1.0.0.0.

## Extra

There is a vim plugin for the language available in the [Vim Nomsu repository](https://bitbucket.org/squidarms/vim-nomsu/src). It is usually kept relatively up-to-date with Nomsu, but occasionally lags behind.
