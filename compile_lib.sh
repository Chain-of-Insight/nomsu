#!/bin/sh
# This file is a script that converts the .nom files in lib/ into slightly more optimized
# precompiled versions that are only lua> ".." and =lua ".." bits which are faster to load.
set -e
moonc *.moon
rm -f core/*.lua lib/*.lua
for file in core/*.nom; do
    printf "Compiling $file ..."
    lua ./nomsu.lua -O -o "core/$(basename $file .nom).lua" $file
    echo "done."
done
for file in lib/*.nom; do
    printf "Compiling $file ..."
    lua ./nomsu.lua -O -o "lib/$(basename $file .nom).lua" $file
    echo "done."
done
