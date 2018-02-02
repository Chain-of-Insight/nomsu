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
    rm core/*.lua
    rm lib/*.lua
    rm tests/*.lua
fi

for file in core/*.nom; do
    printf "Compiling $file ..."
    ./nomsu.moon -c $file
    echo "done."
done
for file in lib/*.nom; do
    printf "Compiling $file ..."
    ./nomsu.moon -c $file
    echo "done."
done
for file in tests/*.nom; do
    printf "Compiling $file ..."
    ./nomsu.moon -c $file
    echo "done."
done
