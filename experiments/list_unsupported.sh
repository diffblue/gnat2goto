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

if ! command -v gnat > /dev/null; then
   echo >&2 "Gnat not on PATH!"
   exit 4
fi

ADA_HOME=`which gnat`
ADA_HOME="$(dirname "$ADA_HOME")/.."
PLATFORM=`uname -m`
DEF_ADA_INCLUDE_PATH="${ADA_HOME}/lib/gcc/${PLATFORM}-pc-linux-gnu"
ADA_VERSION=$(find ${DEF_ADA_INCLUDE_PATH} -maxdepth 1 -type d -name '*.*.*' -print -quit)
DEF_ADA_INCLUDE_PATH="${ADA_VERSION}/rts-native/adainclude"
export ADA_INCLUDE_PATH="${ADA_INCLUDE_PATH:-$DEF_ADA_INCLUDE_PATH}"

export GPR_PROJECT_PATH="${GPR_PROJECT_PATH:-/opt/gnat/lib/gnat}"
if [ ! -d "$GPR_PROJECT_PATH" ]; then
   echo >&2 "GPR project path does not exists!"
   exit 6
fi

for filename in $(find ${path} -name *.adb); do
   gnat2goto ${include_path} "${filename}" >>"$file_name".txt 2>&1
   if [ "$?" -gt 0 ]; then
      echo >&2 "Errors occurred during feature collection."
      echo >&2 "See \"${file_name}.txt\" for details."
      exit 7
   fi
done

sed '/^\[/ d' < "$file_name".txt > "$file_name"_redacted.txt

g++ --std=c++14 "$DIR"/collect_unsupported.cpp -o CollectUnsupported
./CollectUnsupported "$file_name"_redacted.txt
