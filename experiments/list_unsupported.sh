#!/bin/sh
if ! command -v gnat2goto > /dev/null; then
  echo >&2 "gnat2goto not on PATH!"
  exit 1
fi

if [ "$#" -ne 1 ]; then
   echo >&2 "Provide folder name to start"
   exit 2
fi

if [ "$1" = '--help' ]; then
   echo "Run GNAT2Goto on an Ada repository.\n"
   echo "The output is an ordered list of currently unsupported features"
   echo "with the number of times they occur in the input repository.\n"
   echo "The script builds a parsing program using collect_unsupported.cpp and expects"
   echo "this file to be in the same folder.\n"
   echo "Usage:\n\nlist_unsupported.sh path_to_ada_source_folder"
   exit 3
fi

path="$(cd "$(dirname "$1")"; pwd)/$(basename "$1")"
include_path=""
DIR=`dirname "$0"`
file_name=$(basename "$1")

for foldername in $(find ${path} -type d -name "*"); do
   count=`ls -1 ${foldername}/*.ads 2>/dev/null | wc -l`
   if [ $count != 0 ]
   then
      include_path="${include_path} -I ${foldername}"
   fi
done

echo "$1: Unsupported features\n" > "$file_name".txt

for filename in $(find ${path} -name *.adb); do
   gnat2goto ${include_path} "${filename}" >>"$file_name".txt 2>&1
done

sed '/^\[/ d' < "$file_name".txt > "$file_name"_redacted.txt

g++ --std=c++14 "$DIR"/collect_unsupported.cpp -o CollectUnsupported
./CollectUnsupported "$file_name"_redacted.txt
