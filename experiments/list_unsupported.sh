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

echo >&2 "Project to build: $1"
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
PLATFORM=`${ADA_HOME}/bin/gcc -dumpmachine`
DEF_ADA_INCLUDE_PATH="${ADA_HOME}/lib/gcc/${PLATFORM}/"
ADA_GCC_VERSION=`${ADA_HOME}/bin/gcc -dumpversion`
DEF_ADA_INCLUDE_PATH+="${ADA_GCC_VERSION}/rts-native/adainclude"
export ADA_INCLUDE_PATH="${ADA_INCLUDE_PATH:-$DEF_ADA_INCLUDE_PATH}"

export GPR_PROJECT_PATH="${GPR_PROJECT_PATH:-/opt/gnat/lib/gnat}"
if [ ! -d "$GPR_PROJECT_PATH" ]; then
   echo >&2 "GPR project path does not exists!"
   exit 6
fi

# Log whether a file failed to compile due to incorrect syntax, or
# some other error that caused the compiler to exit with non-zero
# exit code
compile_error_occured=0
for filename in $(find ${path} -name '*.adb'); do
   echo >&2 "Compiling $filename..."
   gnat2goto ${include_path} "${filename}" >>"$file_name".txt 2>&1
   if [ "$?" -gt 0 ]; then
      compile_error_occured=1
   fi
done

sed '/^\[/ d' < "$file_name".txt > "$file_name"_redacted.txt

g++ --std=c++14 "$DIR"/collect_unsupported.cpp -o CollectUnsupported
./CollectUnsupported "$file_name"_redacted.txt

if [ "$compile_error_occured" -gt 0 ]; then
   echo >&2 "-------------------------------------------------------"
   echo >&2 "Some files failed to compile during feature collection."
   echo >&2 "See \"${file_name}.txt\" for details."
   echo >&2 "-------------------------------------------------------"
   exit 7
fi
