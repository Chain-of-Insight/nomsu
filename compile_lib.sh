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

echo -n "Compiling lib/core.nom ..."
./nomsu.moon -c lib/core.nom
echo "done."
for file in $(cat lib/core.nom | lua -e "for filename in io.read('*a'):gmatch('use \"([^\"]*)\"') do print(filename) end") ; do
    echo -n "Compiling $file ..."
    ./nomsu.moon -c $file
    echo "done."
done
