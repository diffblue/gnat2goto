#!/bin/sh
if ! command -v gnat2goto > /dev/null; then
  echo >&2 "gnat2goto not on PATH!"
  exit 1
fi

if [ "$#" -ne 1 ]; then
   echo >&2 "Provide folder name to start"
   exit 2
fi

path="$1"
include_path=""
DIR=`dirname "$0"`
file_name=`echo "$1" | sed "s/.*\///"`

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
