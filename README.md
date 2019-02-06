# Nomsu

Nomsu (named after **Nom**ic, and its creator, Peter **Su**ber) is a programming language
designed to be used for playing games of [Nomic](https://en.wikipedia.org/wiki/Nomic), or engaging in other similar activities
revolving around natural language rule-making and self modification. More information is
available at [Nomsu's homepage](https://nomsu.org).

## Dependencies

Nomsu's only dependencies are [Lua 5.2+](https://www.lua.org/) (or [Luajit 2.0+](http://luajit.org/), compiled with `XCFLAGS=-DLUAJIT_ENABLE_LUA52COMPAT` for Lua 5.2 compatibility) and [LPEG](http://www.inf.puc-rio.br/~roberto/lpeg/) (`luarocks install lpeg`). Nomsu's compiler was written in [Moonscript](http://moonscript.org/), but all of the .moon files have been compiled into lua for convenience, so Moonscript is not a dependency. Optionally, if luafilesystem ([Lua version](https://github.com/keplerproject/luafilesystem) or [LuaJIT version](https://github.com/spacewander/luafilesystem)) is installed, it will be used. Otherwise Nomsu will fall back to using system commands (`find` and `ls`), which is slower and a bit less safe. Nomsu has been tested on Mac and Linux, but not Windows.

## Usage

* To run Nomsu interactively (a [REPL](https://en.wikipedia.org/wiki/Read-eval-print_loop)), simply run `lua nomsu.lua` (or `luajit nomsu.lua`).
* To run a .nom file with Nomsu code, run `lua nomsu.lua your_file.nom`.
* The full usage options are avilable via `lua nomsu.lua --help`.

## Examples

There's a more complete set of example code in [examples/how\_do\_i.nom](examples/how_do_i.nom), but roughly, Nomsu looks like:

```
say "Hello"

$my_nums = [5, 23, 42]
for $num in $my_nums:
    say "\$num is one of my nums"

(sing $n bottles of beer) means:
    for $i in $n to 1 by -1:
        say ("
            \$i bottle\("s" if ($i > 1) else "") of beer on the wall,
            \$i bottle\("s" if ($i > 1) else "") of beer!
            Take one down, pass it around...
        ")
    say "No bottles of beer on the wall."

sing 99 bottles of beer
```

## Compiling/installing

If you enjoy Nomsu so much that you'd like to tinker with it or have it in your system path, there is an included Makefile. This is entirely optional, but you can run `make` to compile any modified .moon or .nom files into .lua files and produce an executable file called `nomsu` that can be run directly. `make test` will run the suite of tests. `sudo make install` will install the compiler on your system. By default, the `nomsu` executable will be put in `/usr/local/bin` and all the necessary support files will be put into `/usr/local/share/nomsu/*` and `/usr/local/lib/nomsu/*`, but you can change the location during installation or via the `PREFIX=/your/path/here` flag. The default build options use the system default `lua`, but this can be changed via `make LUA=luajit` or `make LUA_BIN=/path/to/lua`. To uninstall, simply `make uninstall` and provide the same installation path you did during installation.

## File Layout

All `.moon` files have been precompiled into corresponding `.lua` files, so you don't need to have [Moonscript](http://moonscript.org/) installed to run the Nomsu compiler.

* [nomsu](nomsu) - A shell script that selects between different installed versions of Nomsu (using the `-V` flag). You can use this script to, for example, run `nomsu -V 1.2 your_script.nom` to run with the latest version of Nomsu that matches `1.2.?.?`. All flags and arguments are passed along to whichever Nomsu compiler is chosen.
* [nomsu.moon](nomsu.moon) - The source code for the Nomsu command line runner. This handles loading the nomsu environment and compiler and running the REPL.
* `nomsu.*.peg` - The [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) used to define each version of Nomsu's syntax. The format of this file is a slightly modified version of the format accepted by LPEG's `re` module.
* [parser.moon](parser.moon) - The Nomsu parser. This file can also be imported and used directly for applications that only need to *parse* Nomsu, not compile it.
* [nomsu\_compiler.moon](nomsu_compiler.moon) - **The actual Nomsu compiler**. This file defines a function that transforms a Nomsu syntax tree into runnable Lua code.
* [nomsu\_decompiler.moon](nomsu_compiler.moon) - This file defines functions that transform Nomsu syntax trees back into Nomsu code. This can be used for auto-formatting.
* [nomsu\_environment.moon](nomsu_environment.moon) - This file defines the environment in which Nomsu code runs, including some basic built-in functions.
* [bitops.moon](bitops.moon) - This is a shim for Lua 5.2 and LuaJIT that defines bitwise operations that respect metamethods.
* [bootstrap.moon](bootstrap.moon) - This file defines some bootstrapping compile rules.
* [code\_obj.moon](code_obj.moon) - Datastructures used for incrementally building generated code, while preserving code source information.
* [containers.moon](containers.moon) - A library that defines some custom containers (List and Dict) used by nomsu.
* [error\_handling.moon](error_handling.moon) - The logic for producing good error messages within Lua that reference the Nomsu source code that led to them.
* [files.moon](files.moon) - A library for interacting with the filesystem.
* [pretty_errors.moon](pretty_errors.moon) - A simple library for displaying errors in a more visually pleasing/readable way.
* [syntax\_tree.moon](syntax_tree.moon) - Datastructures used for Nomsu Abstract Syntax Trees.
* [text.moon](text.moon) - A library defining some extra functionality for strings.
* [examples/how\_do\_i.nom](examples/how_do_i.nom) - A simple walkthrough of some of the features of Nomsu, written in Nomsu code. **This is a good place to start.**
* [lib/\*/\*.nom](lib) - Language libraries, including the core language stuff like control flow, operators, and metaprogramming (in [lib/core](lib/core)) and optional language libraries for stuff you might want.
* [lib/compatibility/\*.nom](compatibility) - Code for automatically upgrading Nomsu code from old versions to the current version.
* [lib/tools/\*.nom](tools) - A set of utilities useful for doing code manipulation actions.
* [Makefile](Makefile) - Rules for building/installing the compiler.
* [LICENSE](LICENSE) - The software license (MIT).
* [README.md](README.md) - This file.

## Versioning

When Nomsu is istalled via `make install`, all of Nomsu's lua files and [lib/](lib) files are stored in `$PREFIX/share/nomsu/$NOMSU_VERSION` and the Nomsu executable is installed to `$PREFIX/bin/nomsu$NOMSU_VERSION`, along with the file `nomsu` (the version-selection script), which goes to `$PREFIX/bin/nomsu`. When `make uninstall` is run, all those files are deleted (except for `nomsu`, if there are other versions installed).

To run different versions, use the `-V` flag, which will select the latest version matching the specified pattern. For example, if you have v1.0.0, v1.0.2, and 1.1.0 installed, then `nomsu` will run v1.1.0, `nomsu -V1.0` will run v1.0.2, and `nomsu -V 1.0.0` will run v1.0.0.

## Extra

There is a vim plugin for the language available in the [Vim Nomsu repository](https://bitbucket.org/squidarms/vim-nomsu/src). It is usually kept relatively up-to-date with Nomsu, but occasionally lags behind.
