#!/bin/sh
# This file is a script that converts the .nom files in lib/ into slightly more optimized
# precompiled versions that are only lua> ".." and =lua ".." bits which are faster to load.
FLUSH=false
while getopts ":f" opt; do
  case $opt in
    f)
      FLUSH=true
      echo "flushing..."
      ;;
  esac
done
if [ "$FLUSH" = true ] ; then
    for file in $(find lib/ -name "*.lua") ; do
        rm $file
    done
fi

for file in $(cat lib/core.nom | lua -e "for filename in io.read('*a'):gmatch('require \"([^\"]*)\"') do print(filename) end") ; do
    compilefile="${file/\.nom/.lua}"
    if [ ! -e "$compilefile" ] || [ "$file" -nt "$compilefile" ] ; then
        echo "Compiling $file into $compilefile"
        ./nomsu.moon -c $file
    fi
done
