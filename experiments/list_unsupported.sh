#!/bin/sh
if ! command -v gnat2goto > /dev/null; then
  echo >&2 "gnat2goto not on PATH!"
  exit 1
fi

if [ "$#" -ne 1 ]; then
   echo >&2 "Provide folder name to start"
   exit 2
fi

path=`pwd`/"$1"
include_path=""

for foldername in $(find ${path}/ -type d -name "*"); do
   count=`ls -1 ${foldername}/*.ads 2>/dev/null | wc -l`
   if [ $count != 0 ]
   then
      include_path="${include_path} -I ${foldername}"
   fi
done

echo "$1: Unsupported features\n" > "$1".txt

for filename in $(find ${path}/ -name *.adb); do
   gnat2goto ${include_path} "${filename}" >>"$1".txt 2>&1
done

sed '/^\[/ d' < "$1".txt > "$1"_redacted.txt

g++ --std=c++14 ./collect_unsupported.cpp -o CollectUnsupported
./CollectUnsupported "$1"_redacted.txt
