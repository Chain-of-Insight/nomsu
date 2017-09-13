Nomsub (named after **Nom**ic, and its creator, Peter **Sub**er) is a programming language
designed to be used for playing games of Nomic, or engaging in other similar activities
revolving around natural language rule-making and self modification.

The language compiler was written in [Moonscript](http://moonscript.org/), using the
[LPEG library](http://www.inf.puc-rio.br/~roberto/lpeg/) for parsing. LPEG is a dependency,
so you need to install it in order to run the compiler. All of the moon files have been
compiled into lua for convenience, so Moonscript is not a dependency.

In order to run a .nom file, run `lua nomsub.lua your_file.nom`. Code can also be compiled
into lua code directly, which still requires nomsub.lua as a dependency, but bypasses the
compilation phase when it runs. To compile, run `lua nomsub.lua your_file.nom output_file.lua`
which produces an output file which can be run with the command `lua output_file.lua`.

Example code can be found in the examples folder.

core.nom contains some *extremely* helpful and basic core functionality for the language,
written in the language itself, so I recommend adding `run file "core.nom"` to the top of
your files.
