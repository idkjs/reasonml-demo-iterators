#!/bin/sh
# var=$(pwd)
# echo "The current working directory $var."
# list="$(find . -name \*.mli)"
# echo ".mli files are $list."


# find . -name "*.mli" | while read -r filename; do bsrefmt --parse ml --print re "$filename" >"${filename%.mli}.rei"; done

# list="$(find . -name \*.rei)"
# echo "new .rei files are $list."
var=$(pwd)
echo "The current working directory $var."
list="$(find . -name \*.ml)"
echo ".ml files are $list."


find . -name "*.ml" | while read -r filename; do bsrefmt --parse ml --print re "$filename" >"${filename%.ml}.re"; done

list="$(find . -name \*.re)"
echo "new .re files are $list."
