#!/bin/sh
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
    for file in $(find lib/ -name "*.nom.lua") ; do
        rm $file
    done
fi
for file in $(find lib/ -name "*.nom") ; do
    luafile="$file.lua"
    if [ ! -e "$luafile" ] || [ "$file" -nt "$luafile" ] ; then
        echo "Compiling $file into $luafile"
        ./nomsu.moon -c $file
    fi
done
