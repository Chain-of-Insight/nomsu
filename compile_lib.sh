#!/bin/sh
# This file is a script that converts the .nom files in lib/ into slightly more optimized
# precompiled versions that are only lua> ".." and =lua ".." bits which are faster to load.
set -e
moonc *.moon
rm -f core/*.lua lib/*.lua
luajit ./nomsu.lua -c core lib
echo "done."
