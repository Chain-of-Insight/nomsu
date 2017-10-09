#!/bin/sh
for file in $(find lib/ -name "*.nom") ; do
    luafile="$file.lua"
    if [ ! -e "$luafile" ] || [ "$file" -nt "$luafile" ] ; then
        echo "Compiling $file into $luafile"
        ./nomsu.moon -c $file
    fi
done
